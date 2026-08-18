# R/09_slope_heterogeneity.R
#
# STEP 9: Do countries respond to GDP the same way? (No.)
#
# This is the centre of the analysis, so it is worth stating the question
# carefully.
#
# The pooled fixed-effects baseline forces every country to share ONE GDP slope,
# and that slope comes out near zero. There are two very different reasons that
# could happen:
#
#   1. default rates genuinely do not respond to GDP, or
#   2. they do, but in different directions and by different amounts, and
#      averaging them under a single-slope assumption cancels them out.
#
# Telling these apart is what this script is for. Four estimators, the same
# sample and the same specification throughout, ordered by how much they let
# vary between countries:
#
#   Country FE   one slope for everyone                  the benchmark, steps 6/8
#   Mean Group   a slope per country, then a plain average
#   Swamy RCM    a slope per country, averaged with GLS weights
#   CCE-MG       a slope per country, plus a common factor
#
# An important point about how to read them. None of these is "the main
# estimate" with the others as support. They answer closely related questions
# under different assumptions: about slope homogeneity, about weighting, about
# whether countries are independent of one another. They are not four ways of
# computing the same quantity, so where they disagree, the disagreement is
# information rather than noise to be averaged away. The spread across them is
# part of the result.
#
# What each one is:
#
#   Mean Group (Pesaran & Smith 1995) is an ESTIMATOR, not a test. It fits each
#   country separately and averages the coefficients.
#
#   Swamy RCM takes the same country regressions and averages them with weights
#   based on their variance-covariance matrices, so precisely estimated
#   countries count for more.
#
#   CCE-MG (Pesaran 2006) adds cross-sectional averages of y and X to every
#   country regression. It is the only one of the four that does not assume
#   countries are independent of each other, and Block 3b rejects that
#   assumption on this sample. That is why CCE-MG is estimated rather than
#   merely mentioned as a caveat.
#
# A note on the regressor. gdp_ma4 is a four-quarter average of annual GDP
# growth, used as a measure of persistent macroeconomic conditions. It is NOT a
# matching of windows, since GDP growth here is already year-on-year and so
# already looks back four quarters. Nor is it "the theoretically correct"
# regressor. Block 1 below shows it does not rescue the pooled FE on its own.
#
# Inputs:  output/models/panel_model_ready.rds
# Outputs: output/models/slope_het.rds
#          output/tables/estimator_comparison.{tex,html}
#          output/tables/slope_heterogeneity.{tex,html}
#          output/tables/country_slopes.csv
#          output/tables/window_alignment.{tex,html}
#          output/figures/country_slopes_forest.png

source(here::here("R", "00_setup.R"))

set.seed(1)

# ================================================================
# Data: one sample shared by every heterogeneous-slope estimator
# ================================================================
panel <- readRDS(file.path(PATH_MODELS, "panel_model_ready.rds"))

dat <- panel |>
  dplyr::filter(is_main_sample, !is.na(default_rate)) |>
  dplyr::mutate(iso2 = as.factor(iso2))

# Common estimation sample: MA4 spec. Mean Group, Swamy and the country-by-
# country slopes must all run on THIS sample for the comparison to be clean.
dat_ma <- dat |>
  dplyr::filter(!is.na(gdp_ma4), !is.na(inf_l1))

cat("=== SAMPLES ===\n")
cat(sprintf("  FE baseline sample (L0):  N=%d, %d countries\n",
            nrow(dat), dplyr::n_distinct(dat$iso2)))
cat(sprintf("  MA4 sample (shared):      N=%d, %d countries, T=%d..%d\n",
            nrow(dat_ma), dplyr::n_distinct(dat_ma$iso2),
            min(table(dat_ma$iso2)), max(table(dat_ma$iso2))))

# ================================================================
# Block 1: Window alignment in the pooled FE
#
# Before blaming slope heterogeneity for the near-zero pooled coefficient, rule
# out the simpler explanation: maybe GDP is just measured over the wrong window,
# and the default rate responds with a delay the baseline does not capture.
#
# So this fits the pooled FE with GDP entered five different ways:
# contemporaneous, lagged 2, lagged 4, as a 4-quarter average, and the exact
# specification the later estimators use. If one of them produced a large
# coefficient, the story would end here.
#
# It does not. The pooled coefficient stays near zero throughout, which is what
# makes the heterogeneity question worth asking.
# ================================================================
dat_l2 <- dat |> dplyr::filter(!is.na(gdp_l2), !is.na(inf_l1))
dat_l4 <- dat |> dplyr::filter(!is.na(gdp_l4), !is.na(inf_l1))

fit_fe_l0  <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                    data = dat, vcov = ~ iso2)
fit_fe_l2  <- feols(default_rate ~ gdp_l2 + inf_l1  | iso2, data = dat_l2, vcov = ~ iso2)
fit_fe_l4  <- feols(default_rate ~ gdp_l4 + inf_l1  | iso2, data = dat_l4, vcov = ~ iso2)
fit_fe_ma4 <- feols(default_rate ~ gdp_ma4 + inf_ma4 | iso2, data = dat_ma, vcov = ~ iso2)

# The pooled FE fitted on the EXACT specification Mean Group and Swamy use
# (gdp_ma4 + inf_l1). This is the common-slope benchmark the other estimators
# are compared against. Without it, any difference between the pooled result and
# the heterogeneous ones could just be a difference in specification rather than
# a difference in what the estimator assumes.
fit_fe_shared <- feols(default_rate ~ gdp_ma4 + inf_l1 | iso2,
                       data = dat_ma, vcov = ~ iso2)

cat("\n=== BLOCK 1 — window alignment in pooled FE ===\n")
etable(fit_fe_l0, fit_fe_l2, fit_fe_l4, fit_fe_ma4, fit_fe_shared,
       headers = c("L0", "L2", "L4", "MA4", "MA4+infL1"),
       digits = 4, fitstat = c("n", "wr2"))

# ================================================================
# Block 2: country-level slopes: L4 lag vs MA4 window
#
# A cautionary result, kept in because it changed how the rest of this script is
# written.
#
# An earlier iteration sorted countries into "responds to GDP" and "does not"
# based on cor(DR, GDP_L4). This block re-does that classification with the
# regressor changed to the 4-quarter average, a defensible alternative rather
# than a contrived one. The classification does not survive. Countries flip sides.
#
# So the classification was an artefact of one regressor choice, not a property
# of the countries. Anything built on top of it would have been too, which is
# why the analysis below works with estimated slopes and their uncertainty
# rather than with categories.
# ================================================================
country_compare <- dat_ma |>
  dplyr::group_by(iso2) |>
  dplyr::group_modify(function(d, key) {
    d4 <- d[!is.na(d$gdp_l4), ]
    cor_l4  <- if (nrow(d4) > 3) stats::cor(d4$default_rate, d4$gdp_l4) else NA_real_
    cor_ma4 <- stats::cor(d$default_rate, d$gdp_ma4)
    b_ma4_raw <- stats::coef(stats::lm(default_rate ~ gdp_ma4, data = d))[["gdp_ma4"]]
    b_ma4_inf <- stats::coef(stats::lm(default_rate ~ gdp_ma4 + inf_l1, data = d))[["gdp_ma4"]]
    tibble::tibble(cor_l4 = cor_l4, cor_ma4 = cor_ma4,
                   b_ma4_raw = b_ma4_raw, b_ma4_inf = b_ma4_inf)
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate(flip = sign(cor_l4) != sign(b_ma4_inf)) |>
  dplyr::arrange(cor_l4)

n_neg_l4  <- sum(country_compare$cor_l4 < 0, na.rm = TRUE)
n_neg_ma4 <- sum(country_compare$cor_ma4 < 0, na.rm = TRUE)
n_neg_b   <- sum(country_compare$b_ma4_inf < 0, na.rm = TRUE)
cor_of_cors <- stats::cor(country_compare$cor_l4, country_compare$cor_ma4,
                          use = "complete.obs")

cat("\n=== BLOCK 2 — L4 vs MA4 country classification ===\n")
print(as.data.frame(country_compare), row.names = FALSE, digits = 3)
cat(sprintf("\n  negative cor_L4:            %d / %d\n", n_neg_l4, nrow(country_compare)))
cat(sprintf("  negative cor_MA4:           %d / %d\n", n_neg_ma4, nrow(country_compare)))
cat(sprintf("  negative b_MA4 (ctrl inf):  %d / %d\n", n_neg_b,   nrow(country_compare)))
cat(sprintf("  corr(cor_L4, cor_MA4):      %.3f\n", cor_of_cors))
cat(sprintf("  sign flips:                 %d\n", sum(country_compare$flip, na.rm = TRUE)))

# ================================================================
# Block 3: country-by-country slopes with HAC standard errors
#
# One regression per country, so each gets its own GDP slope with no
# cross-country restriction at all. This is the raw material for everything
# after: Mean Group, Swamy and the dispersion measures all work from these.
#
# Standard errors are Newey-West with 3 lags. The choice is not arbitrary: the
# default rate is built on a rolling four-quarter window, so adjacent
# observations share three quarters of the same data and are strongly dependent
# by construction. NW(3) spans exactly that window.
# ================================================================
country_slope <- function(d, hac_lag = 3L) {
  m  <- stats::lm(default_rate ~ gdp_ma4 + inf_l1, data = d)
  V  <- sandwich::NeweyWest(m, lag = hac_lag, prewhite = FALSE, adjust = TRUE)
  b  <- stats::coef(m)[["gdp_ma4"]]
  se_ols <- sqrt(diag(stats::vcov(m)))[["gdp_ma4"]]
  se_hac <- sqrt(diag(V))[["gdp_ma4"]]
  tibble::tibble(t_eff = nrow(d), beta = b, se_ols = se_ols, se_hac = se_hac,
                 t_hac = b / se_hac,
                 p_hac = 2 * stats::pt(-abs(b / se_hac), df = nrow(d) - 3))
}

# Confidence intervals use the t critical value with T-3 degrees of freedom, to
# match p_hac, rather than 1.96. With only a few dozen quarters per country the
# normal quantile is several percent too narrow, and these intervals are shown
# in a figure where their width is the whole point.
#
# The effective range of T is printed by the SAMPLES block above instead of
# being repeated here, because it changed once already when the sample grew.
slopes <- dat_ma |>
  dplyr::group_by(iso2) |>
  dplyr::group_modify(~ country_slope(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    t_crit = stats::qt(0.975, df = t_eff - 3),
    ci_lo  = beta - t_crit * se_hac,
    ci_hi  = beta + t_crit * se_hac,
    sig10  = p_hac < 0.10
  ) |>
  dplyr::arrange(beta)

cat("\n=== BLOCK 3 — country-specific slopes (HAC NW lag 3) ===\n")
print(as.data.frame(slopes), row.names = FALSE, digits = 3)
cat(sprintf("\n  negative slopes:      %d / %d\n", sum(slopes$beta < 0), nrow(slopes)))
cat(sprintf("  significant at 10%%:   %d / %d\n", sum(slopes$sig10), nrow(slopes)))
cat(sprintf("  mean SE_ols=%.4f  mean SE_hac=%.4f  ratio=%.3f\n",
            mean(slopes$se_ols), mean(slopes$se_hac),
            mean(slopes$se_hac) / mean(slopes$se_ols)))

# ================================================================
# Block 3b: CD test on the residuals of the country regressions
#
# This block checks an assumption the next two estimators depend on.
#
# The Mean Group standard error is sd(beta_i)/sqrt(N). That formula treats the
# 26 country estimates as independent draws. If the country regressions'
# residuals are correlated with each other, because a common shock hit
# everyone, then they are not independent. There is less information here than
# 26 independent estimates, and the standard error is too small. Swamy's
# precision weighting has the same exposure.
#
# The Pesaran CD statistic tests that assumption directly on those residuals,
# rather than assuming it holds.
# ================================================================
resid_long <- dat_ma |>
  dplyr::group_by(iso2) |>
  dplyr::group_modify(function(d, key) {
    m <- stats::lm(default_rate ~ gdp_ma4 + inf_l1, data = d)
    tibble::tibble(quarter_order = d$quarter_order, res = stats::resid(m))
  }) |>
  dplyr::ungroup()

resid_wide <- resid_long |>
  tidyr::pivot_wider(names_from = iso2, values_from = res) |>
  dplyr::arrange(quarter_order) |>
  dplyr::select(-quarter_order)

cd_from_resid <- function(mat) {
  ctry <- colnames(mat); n <- length(ctry)
  s <- 0; pairs <- 0
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    ok <- stats::complete.cases(mat[[i]], mat[[j]])
    Tij <- sum(ok)
    if (Tij > 3) {
      rho <- stats::cor(mat[[i]][ok], mat[[j]][ok])
      s <- s + sqrt(Tij) * rho
      pairs <- pairs + 1
    }
  }
  cd <- sqrt(2 / (n * (n - 1))) * s
  tibble::tibble(cd_stat = cd, p_value = 2 * stats::pnorm(-abs(cd)), n_pairs = pairs)
}

cd_country <- cd_from_resid(resid_wide)

cat("\n=== BLOCK 3b — Pesaran CD on country-regression residuals ===\n")
print(as.data.frame(cd_country), row.names = FALSE, digits = 3)
if (cd_country$p_value >= 0.05) {
  cat("  -> no evidence of cross-sectional correlation in the country-regression\n")
  cat("     residuals: the independence assumption behind the Mean Group SE is\n")
  cat("     empirically supported, not just assumed.\n")
} else {
  cat("  -> cross-sectional correlation detected: the MG SE is likely understated;\n")
  cat("     treat the MG p-value as optimistic (write-up must say so).\n")
  cat("     Block 3c estimates the version that does not assume independence.\n")
}

# ================================================================
# Block 3c: the common factor, estimated rather than only caveated
#
# Block 3b rejects cross-sectional independence. The easy response would be to
# add a sentence noting that the Mean Group p-value is optimistic and carry on.
# That is not enough: the same assumption sits underneath every estimator in
# Block 5, and underneath the dispersion measure in Block 4, whose chi-square
# reference distribution also assumes the country estimates are independent
# draws. A caveat in prose does not fix any of them.
#
# The standard response is Pesaran's (2006) Common Correlated Effects
# augmentation. Adding cross-sectional averages of the dependent variable and
# the regressors to each country regression projects out a single unobserved
# common factor with country-specific loadings, so what remains is the
# country's own response rather than its share of a shared movement.
#
# Everything downstream is then recomputed on the augmented regressions: the
# country slopes, the dispersion measures, and the CD test itself. That way the
# comparison is like-for-like instead of mixing corrected and uncorrected
# quantities in the same table.
# ================================================================
cs_avg <- dat_ma |>
  dplyr::group_by(quarter_order) |>
  dplyr::summarise(dplyr::across(c(default_rate, gdp_ma4, inf_l1), mean),
                   .groups = "drop") |>
  dplyr::rename(dr_bar = default_rate, gdp_bar = gdp_ma4, inf_bar = inf_l1)

dat_cce <- dat_ma |> dplyr::left_join(cs_avg, by = "quarter_order")

FML_PLAIN <- default_rate ~ gdp_ma4 + inf_l1
FML_CCE   <- default_rate ~ gdp_ma4 + inf_l1 + dr_bar + gdp_bar + inf_bar

# Same routine as Block 3, except the residual degrees of freedom are taken from
# the number of parameters actually estimated rather than hard-coded at 3. The
# augmented regressions have more regressors, so a fixed 3 would overstate the
# degrees of freedom and quietly narrow every interval.
slope_generic <- function(d, fml, hac_lag = 3L) {
  m  <- stats::lm(fml, data = d)
  k  <- length(stats::coef(m))
  V  <- sandwich::NeweyWest(m, lag = hac_lag, prewhite = FALSE, adjust = TRUE)
  b  <- stats::coef(m)[["gdp_ma4"]]
  se_hac <- sqrt(diag(V))[["gdp_ma4"]]
  tibble::tibble(t_eff = nrow(d), beta = b,
                 se_ols = sqrt(diag(stats::vcov(m)))[["gdp_ma4"]],
                 se_hac = se_hac, df = nrow(d) - k,
                 p_hac = 2 * stats::pt(-abs(b / se_hac), df = nrow(d) - k))
}

slopes_cce <- dat_cce |>
  dplyr::group_by(iso2) |>
  dplyr::group_modify(~ slope_generic(.x, FML_CCE)) |>
  dplyr::ungroup() |>
  dplyr::arrange(beta)

# The CD test again, now on the residuals of the AUGMENTED regressions. This
# asks whether the augmentation actually did its job, i.e. whether the common
# component it was added to absorb is in fact gone. Applying a correction
# without checking that it worked would be no better than not correcting.
resid_cce_wide <- dat_cce |>
  dplyr::group_by(iso2) |>
  dplyr::group_modify(function(d, key) {
    tibble::tibble(quarter_order = d$quarter_order,
                   res = stats::resid(stats::lm(FML_CCE, data = d)))
  }) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = iso2, values_from = res) |>
  dplyr::arrange(quarter_order) |>
  dplyr::select(-quarter_order)

cd_cce <- cd_from_resid(resid_cce_wide)

cat("\n=== BLOCK 3c — common-factor (CCE) augmentation ===\n")
cat(sprintf("  CD, plain country regressions: %+.2f (p = %.2e)\n",
            cd_country$cd_stat, cd_country$p_value))
cat(sprintf("  CD, CCE-augmented regressions: %+.2f (p = %.3f)\n",
            cd_cce$cd_stat, cd_cce$p_value))
if (abs(cd_cce$cd_stat) < abs(cd_country$cd_stat)) {
  cat("  -> the augmentation absorbs part of the common component, so the factor\n")
  cat("     structure is real and not an artefact of the CD statistic.\n")
}
if (cd_cce$p_value < 0.05) {
  cat("  -> residual dependence still rejects after augmentation: CCE-MG is a\n")
  cat("     better-specified estimator here, not a clean one. Reported with that\n")
  cat("     caveat rather than as the estimator that settles the question.\n")
}
cat(sprintf("  country slopes on gdp_ma4: %d/%d negative, median %+.4f (plain: %d/%d, %+.4f)\n",
            sum(slopes_cce$beta < 0), nrow(slopes_cce), stats::median(slopes_cce$beta),
            sum(slopes$beta < 0), nrow(slopes), stats::median(slopes$beta)))

# ================================================================
# Block 4: how dispersed are the slopes? Cochran Q / I2 / tau
#
# The country slopes clearly differ. The question is whether they differ by more
# than their own estimation error would produce anyway. A set of noisy
# estimates of one true slope will still look scattered.
#
# Three things to be careful about here, all of them about not over-claiming.
#
# 1. These are SUPPORTING MEASURES OF DISPERSION, not a formal test of slope
#    homogeneity. They are deliberately not labelled Pesaran-Yamagata.
#    Pesaran-Yamagata is a specific standardisation of Swamy's statistic with
#    its own conditions on the error structure, and the standard version is not
#    generally robust to the heteroskedasticity and serial correlation this
#    panel has. delta_std below is a standardised transform of Q, reported for
#    reference only. No Pesaran-Yamagata critical values are claimed.
#
# 2. The obvious alternative, a joint F-test on 25 country interactions, is
#    not used, because it does not work here. With 26 clusters the
#    cluster-robust variance matrix has rank 25, so testing 25 restrictions is
#    degenerate (Cameron & Miller 2015). It would return a number, and the
#    number would be meaningless.
#
# 3. Cross-sectional dependence applies to this block too. Q's chi-square
#    reference distribution assumes the country estimates are independent
#    draws, which is exactly the assumption Block 3b rejects. Caveating the
#    Mean Group p-value on those grounds while leaving Q alone would be
#    inconsistent, so the dispersion is recomputed on the CCE-augmented slopes
#    as well.
#
#    That second version is the one to rely on. It measures the dispersion that
#    survives after a common factor has been projected out: genuinely
#    country-specific variation, rather than one shared time path showing up as
#    apparent heterogeneity.
# ================================================================
het_stats <- function(beta, se, k = 1L) {
  w    <- 1 / se^2
  bbar <- sum(w * beta) / sum(w)
  Q    <- sum(w * (beta - bbar)^2)
  n    <- length(beta)
  df   <- n - 1L
  I2   <- max(0, (Q - df) / Q)
  C    <- sum(w) - sum(w^2) / sum(w)
  tau2 <- max(0, (Q - df) / C)
  # Standardised transform of Q. Reported for reference; NOT the PY delta test.
  delta <- sqrt(n) * (Q / n - k) / sqrt(2 * k)
  tibble::tibble(beta_pooled_prec = bbar, Q = Q, df = df,
                 p = stats::pchisq(Q, df, lower.tail = FALSE),
                 I2 = I2, tau = sqrt(tau2), delta_std = delta,
                 p_delta = 2 * stats::pnorm(-abs(delta)))
}

het_main <- het_stats(slopes$beta, slopes$se_hac)
het_ols  <- het_stats(slopes$beta, slopes$se_ols)
het_cce  <- het_stats(slopes_cce$beta, slopes_cce$se_hac)

# Sensitivity: does the rejection depend on the HAC lag choice?
het_by_lag <- purrr::map_dfr(0:6, function(L) {
  s <- dat_ma |>
    dplyr::group_by(iso2) |>
    dplyr::group_modify(~ country_slope(.x, hac_lag = L)) |>
    dplyr::ungroup()
  se <- if (L == 0) s$se_ols else s$se_hac
  dplyr::bind_cols(tibble::tibble(hac_lag = L), het_stats(s$beta, se))
})

cat("\n=== BLOCK 4 — slope dispersion measures (supporting, not a formal test) ===\n")
cat("HAC SEs (lag 3):\n"); print(as.data.frame(het_main), row.names = FALSE, digits = 3)
cat("OLS SEs:\n");         print(as.data.frame(het_ols),  row.names = FALSE, digits = 3)
cat("CCE-augmented slopes, HAC SEs (lag 3) — dispersion net of the common factor:\n")
print(as.data.frame(het_cce), row.names = FALSE, digits = 3)
cat(sprintf("  -> I2 goes from %.0f%% to %.0f%% once the common factor is projected out.\n",
            100 * het_main$I2, 100 * het_cce$I2))
if (het_cce$I2 > het_main$I2) {
  cat("     The dispersion result is REINFORCED by handling cross-sectional\n")
  cat("     dependence, not weakened: what the plain slopes shared was a common\n")
  cat("     time path, and removing it leaves the country-specific variation\n")
  cat("     more visible, not less.\n")
}
cat("\nSensitivity to HAC lag:\n")
print(as.data.frame(het_by_lag[, c("hac_lag", "Q", "p", "I2", "tau")]),
      row.names = FALSE, digits = 3)

# ================================================================
# Block 5: the estimators side by side
#
# Same sample (dat_ma), same specification (gdp_ma4 + inf_l1) for every row.
# The ONLY thing that changes between them is what they assume about the slopes:
#
#   Country FE   all countries share one slope
#   Mean Group   each country has its own, averaged plainly
#   Swamy RCM    each country has its own, averaged by precision
#   CCE-MG       each country has its own, after a common factor is removed
#
# Holding sample and specification fixed is what makes the comparison mean
# anything. Any difference between these rows is attributable to the assumption,
# not to the data they were fitted on.
# ================================================================
pdat <- plm::pdata.frame(dat_ma, index = c("iso2", "quarter_order"))

# Mean Group (Pesaran-Smith 1995): estimate per country, average the slopes.
fit_mg <- plm::pmg(default_rate ~ gdp_ma4 + inf_l1, data = pdat, model = "mg")

# Swamy random coefficients: same country-specific betas, GLS-weighted mean.
fit_swamy <- plm::pvcm(default_rate ~ gdp_ma4 + inf_l1, data = pdat, model = "random")

# CCE Mean Group (Pesaran 2006): country-specific betas again, but each country
# regression is augmented with the cross-section averages of y and X, so a
# common factor with country-specific loadings is projected out. This is the row
# that answers Block 3b instead of only caveating it.
fit_ccemg <- plm::pcce(default_rate ~ gdp_ma4 + inf_l1, data = pdat, model = "mg")

# How much of the CCE-MG average is carried by just a few countries?
#
# Free slopes plus a projected-out common factor leaves the individual country
# estimates noisy, and an average over 26 noisy numbers can be moved a long way
# by two of them. This is computed and reported so the write-up cannot quote the
# CCE-MG point estimate without also showing how unstable it is.
ccemg_ind <- fit_ccemg$indcoef
ccemg_b   <- if (is.matrix(ccemg_ind)) {
  ccemg_ind["gdp_ma4", ]
} else {
  sapply(ccemg_ind, function(z) z[["gdp_ma4"]])
}
ccemg_loo <- sapply(seq_along(ccemg_b), function(i) mean(ccemg_b[-i]))

# A hand-computed check on Mean Group. By construction MG is nothing more than
# the simple mean of the country slopes, with SE = sd(beta)/sqrt(n). Computing
# it directly from the Block 3 slopes and asserting it matches plm::pmg confirms
# that the country regressions and the packaged estimator are working on the
# same thing.
mg_manual_b  <- mean(slopes$beta)
mg_manual_se <- stats::sd(slopes$beta) / sqrt(nrow(slopes))
mg_manual_t  <- mg_manual_b / mg_manual_se
mg_manual_p  <- 2 * stats::pnorm(-abs(mg_manual_t))

cat("\n=== BLOCK 5 — Mean Group ===\n")
print(summary(fit_mg))
cat(sprintf("\nManual MG cross-check: beta=%.4f  SE=%.4f  t=%.2f  p=%.4f\n",
            mg_manual_b, mg_manual_se, mg_manual_t, mg_manual_p))

cat("\n=== BLOCK 5 — Swamy RCM ===\n")
print(summary(fit_swamy))

cat("\n=== BLOCK 5 — CCE Mean Group (Pesaran 2006) ===\n")
print(summary(fit_ccemg))
cat(sprintf(
  "\nStability of the CCE-MG average: mean %+.4f, median %+.4f, sd %.4f, %d/%d negative\n",
  mean(ccemg_b), stats::median(ccemg_b), stats::sd(ccemg_b),
  sum(ccemg_b < 0), length(ccemg_b)))
cat(sprintf("  10%% trimmed mean %+.4f | leave-one-country-out range %+.4f to %+.4f\n",
            mean(ccemg_b, trim = 0.10), min(ccemg_loo), max(ccemg_loo)))
cat(sprintf("  most extreme country slopes: %s\n",
            paste(sprintf("%s %+.3f", names(sort(ccemg_b))[c(1, 2, length(ccemg_b))],
                          sort(ccemg_b)[c(1, 2, length(ccemg_b))]), collapse = " | ")))
cat("  -> the CCE-MG point estimate is the least stable of the four. It is\n")
cat("     reported because it is the one estimator consistent with the rejected\n")
cat("     CD test, NOT because it is the most reliable number in the table.\n")

# --- Assemble the estimator comparison ---------------------------------------
# Intervals use the t critical value with G-1 degrees of freedom, not 1.96.
#
# Every estimator in this table is effectively an average over 26 units: the FE
# inference is clustered on 26 countries, and MG, Swamy and CCE-MG each average
# 26 country slopes. At that size the normal quantile makes the intervals about
# 5% too narrow.
#
# The country-level intervals in Block 3 already used a t critical value, so
# leaving 1.96 here meant two tables reporting the same kind of quantity
# disagreed about how to build an interval.
grab <- function(fit, param, label, assumption, n, g) {
  s  <- summary(fit)
  # fixest -> $coeftable; plm::pmg / plm::pcce -> $CoefTable; plm::pvcm -> $coefficients
  ct <- if (inherits(fit, "fixest")) s$coeftable
        else if (!is.null(s$CoefTable)) s$CoefTable
        else s$coefficients
  if (!param %in% rownames(ct)) {
    return(tibble::tibble(estimator = label, assumption = assumption,
                          beta = NA_real_, se = NA_real_, p = NA_real_,
                          ci_lo = NA_real_, ci_hi = NA_real_, N = n, countries = g))
  }
  b  <- ct[param, 1]; se <- ct[param, 2]; p <- ct[param, ncol(ct)]
  tcrit <- stats::qt(0.975, df = g - 1)
  tibble::tibble(estimator = label, assumption = assumption,
                 beta = b, se = se, p = p,
                 ci_lo = b - tcrit * se, ci_hi = b + tcrit * se, N = n, countries = g)
}

n_ma <- nrow(dat_ma); g_ma <- dplyr::n_distinct(dat_ma$iso2)
n_l0 <- nrow(dat);    g_l0 <- dplyr::n_distinct(dat$iso2)

# Every row below uses the SAME sample (dat_ma) and the SAME specification
# (gdp_ma4 + inf_l1), so the rows are directly comparable.
#
# The FE with contemporaneous GDP (fit_fe_l0) is deliberately left out. It is a
# different specification, and including it would mean one row of this table
# differed from the others in two ways at once, estimator and specification,
# so any gap between them could not be attributed to either. It belongs in the
# baselines table in steps 6 and 8, and that is where it stays.
comparison <- dplyr::bind_rows(
  grab(fit_fe_shared, "gdp_ma4", "Country FE (MA4)", "common slope",              n_ma, g_ma),
  grab(fit_mg,        "gdp_ma4", "Mean Group",       "free slopes, simple mean",  n_ma, g_ma),
  grab(fit_swamy,     "gdp_ma4", "Swamy RCM",        "free slopes, GLS weighted", n_ma, g_ma),
  grab(fit_ccemg,     "gdp_ma4", "CCE-MG",           "free slopes + common factor", n_ma, g_ma)
)

comparison_inf <- dplyr::bind_rows(
  grab(fit_fe_shared, "inf_l1", "Country FE (MA4)", "common slope",              n_ma, g_ma),
  grab(fit_mg,        "inf_l1", "Mean Group",       "free slopes, simple mean",  n_ma, g_ma),
  grab(fit_swamy,     "inf_l1", "Swamy RCM",        "free slopes, GLS weighted", n_ma, g_ma),
  grab(fit_ccemg,     "inf_l1", "CCE-MG",           "free slopes + common factor", n_ma, g_ma)
)

# Reported separately, NOT inside the same-spec comparison above.
fe_l0_gdp <- grab(fit_fe_l0, "gdp_growth_yoy", "Country FE (L0, different spec)",
                  "common slope, contemporaneous GDP", n_l0, g_l0)

cat("\n=== ESTIMATOR COMPARISON — GDP (same sample, same spec: gdp_ma4 + inf_l1) ===\n")
print(as.data.frame(comparison), row.names = FALSE, digits = 3)
cat("\n=== ESTIMATOR COMPARISON — inflation (same sample, same spec) ===\n")
print(as.data.frame(comparison_inf), row.names = FALSE, digits = 3)
cat("\nFor reference only — NOT part of the same-spec comparison:\n")
print(as.data.frame(fe_l0_gdp), row.names = FALSE, digits = 3)

# Descriptive only. This is NOT being offered as an alternative estimator.
# The simple Mean Group average is sensitive to the handful of countries with
# short series and very imprecise slopes. Quantifying that here lets the
# write-up say how much of the Mean Group standard error those countries
# account for, instead of leaving it as an unexamined worry.
med_beta  <- stats::median(slopes$beta)
worst <- slopes |> dplyr::arrange(dplyr::desc(se_hac)) |> dplyr::slice(1:2)
cat(sprintf("\nDescriptive: median country slope = %.4f (mean = %.4f)\n",
            med_beta, mg_manual_b))
cat(sprintf("Two least precise slopes: %s (b=%.3f, SE=%.3f, T=%d), %s (b=%.3f, SE=%.3f, T=%d)\n",
            worst$iso2[1], worst$beta[1], worst$se_hac[1], worst$t_eff[1],
            worst$iso2[2], worst$beta[2], worst$se_hac[2], worst$t_eff[2]))

# ================================================================
# Block 6: why the pooled FE comes out near zero
#
# Having established that country slopes differ, this explains exactly how the
# pooled estimator turns them into approximately nothing.
#
# With a SINGLE regressor, the answer is simple: the pooled FE slope is a
# weighted average of the country slopes, weighted by how much that regressor
# varies within each country. Countries whose GDP moves a lot get more say.
#
# With TWO regressors that is no longer true, and the difference matters. The
# correct statement is matrix-weighted:
#
#     beta_FE = [sum_i Xt_i'Xt_i]^-1 sum_i Xt_i'Xt_i beta_i
#
# where Xt is the within-country demeaned regressor matrix. Writing out just the
# GDP element:
#
#     beta_FE,gdp = sum_i w_i * beta_i,gdp  +  sum_i c_i * beta_i,inf
#
# The w_i sum to 1, as you would expect. But there is a second term: the
# CROSS-LOADINGS c_i, which sum to zero. Each country's INFLATION slope also
# feeds into the pooled GDP coefficient, through the correlation between the two
# regressors within that country.
#
# This block previously used scalar variance weights, which ignore that second
# term. They implied beta_FE = +0.0159 when the actual value is +0.0005, out by
# a factor of thirty. The story they told about where identification comes from
# was right, but they could not reproduce the coefficient they were being used
# to explain, and a decomposition that cannot reproduce its own target is not an
# explanation.
#
# So the decomposition below is computed directly from the normal equations, and
# there is an assertion that it reproduces what feols returns.
# ================================================================
fe_decomp <- function(d, ycol, xcols) {
  sp <- split(d, droplevels(d$iso2))
  A  <- matrix(0, length(xcols), length(xcols))
  parts <- lapply(sp, function(g) {
    X <- scale(as.matrix(g[, xcols]), scale = FALSE)   # within-country demeaning
    y <- g[[ycol]] - mean(g[[ycol]])
    XtX <- crossprod(X)
    list(XtX = XtX, beta = solve(XtX, crossprod(X, y)))
  })
  for (p in parts) A <- A + p$XtX
  Ainv <- solve(A)
  tibble::tibble(
    iso2   = names(parts),
    # loading of country i's OWN gdp slope on the pooled gdp coefficient
    w_fe   = vapply(parts, function(p) (Ainv %*% p$XtX)[1, 1], numeric(1)),
    # loading of country i's INFLATION slope on the pooled gdp coefficient
    c_fe   = vapply(parts, function(p) (Ainv %*% p$XtX)[1, 2], numeric(1)),
    b_gdp  = vapply(parts, function(p) p$beta[1], numeric(1)),
    b_inf  = vapply(parts, function(p) p$beta[2], numeric(1))
  )
}

decomp <- fe_decomp(dat_ma, "default_rate", c("gdp_ma4", "inf_l1"))

beta_fe_actual <- unname(stats::coef(fit_fe_shared)[["gdp_ma4"]])
beta_own   <- sum(decomp$w_fe * decomp$b_gdp)     # own-slope term
beta_cross <- sum(decomp$c_fe * decomp$b_inf)     # cross-loading term
stopifnot(isTRUE(all.equal(beta_own + beta_cross, beta_fe_actual, tolerance = 1e-8)))

weights_tab <- decomp |>
  dplyr::rename(beta = b_gdp) |>
  dplyr::left_join(slopes |> dplyr::select(iso2, se_hac), by = "iso2") |>
  dplyr::left_join(
    dat_ma |> dplyr::group_by(iso2) |>
      dplyr::summarise(sd_gdp_ma4 = stats::sd(gdp_ma4), t_eff = dplyr::n(), .groups = "drop"),
    by = "iso2") |>
  dplyr::mutate(w_prec = (1 / se_hac^2) / sum(1 / se_hac^2)) |>
  dplyr::arrange(dplyr::desc(w_fe))

top5_fe   <- sum(utils::head(weights_tab$w_fe, 5))
top5_prec <- sum(utils::head(sort(weights_tab$w_prec, decreasing = TRUE), 5))
pos_share <- weights_tab |> dplyr::filter(beta > 0) |> dplyr::summarise(s = sum(w_fe)) |> dplyr::pull(s)

cat("\n=== BLOCK 6 — exact FE decomposition vs precision weights ===\n")
print(as.data.frame(weights_tab), row.names = FALSE, digits = 3)
cat(sprintf("\n  own-slope term      sum_i w_i * beta_i,gdp = %+.4f\n", beta_own))
cat(sprintf("  cross-loading term  sum_i c_i * beta_i,inf = %+.4f\n", beta_cross))
cat(sprintf("  total                                      = %+.4f  (feols: %+.4f)\n",
            beta_own + beta_cross, beta_fe_actual))
cat(sprintf("\n  top-5 FE weight share:        %.1f%%\n", 100 * top5_fe))
cat(sprintf("  top-5 precision weight share: %.1f%%\n", 100 * top5_prec))
cat(sprintf("  FE weight on positive-beta countries: %.1f%%\n", 100 * pos_share))
cat("  Note: the own-slope term alone does NOT equal the FE coefficient. The\n")
cat("        write-up must quote both terms, or quote neither — concentration of\n")
cat("        FE weight describes where the estimate's statistical leverage comes\n")
cat("        from, not by itself an arithmetic explanation of the pooled estimate.\n")

# A direct demonstration that the weighting above is really what drives the
# pooled result.
#
# Ireland carries the largest FE weight, because its GDP has by far the largest
# within-country variance in the panel. That is a known measurement issue rather
# than genuine volatility: Irish GDP is distorted by multinational IP relocation
# and aircraft leasing, which is precisely why the Irish CSO publishes GNI* as
# an alternative. So excluding Ireland is a data-quality argument that can be
# made in advance, like the Cyprus exclusion in step 4.
#
# Two variants, and they are not the same kind of claim:
#
#   Drop IE          A defensible exclusion, argued on the data rather than on
#                    the result.
#
#   Drop IE, ES, PT  The three largest FE weights. There is NO advance
#                    justification for dropping Spain and Portugal, so this is
#                    NOT a candidate specification. It is a mechanical
#                    illustration of how few countries the pooled estimate
#                    actually rests on, and it is labelled as such so nobody
#                    mistakes it for a preferred result.
dat_noie  <- dat_ma |> dplyr::filter(iso2 != "IE")
dat_no3   <- dat_ma |> dplyr::filter(!iso2 %in% c("IE", "ES", "PT"))

fit_fe_noie <- feols(default_rate ~ gdp_ma4 + inf_l1 | iso2,
                     data = dat_noie, vcov = ~ iso2)
fit_fe_no3  <- feols(default_rate ~ gdp_ma4 + inf_l1 | iso2,
                     data = dat_no3, vcov = ~ iso2)

cat("\n=== BLOCK 6 — pooled FE dropping the highest-FE-weight countries ===\n")
etable(fit_fe_shared, fit_fe_noie, fit_fe_no3,
       headers = c("All 26", "Drop IE", "Drop IE/ES/PT"),
       digits = 4, fitstat = c("n", "wr2"))
cat("Note: dropping IE alone does NOT rescue the pooled FE. Only removing the\n")
cat("      three largest FE weights does, and two of those three lack an\n")
cat("      ex-ante justification -> the pooled FE is not salvageable by\n")
cat("      exclusions. The heterogeneous-slope estimators are the answer.\n")

# ================================================================
# Figure: forest plot of the 26 country slopes
# ================================================================
mg_b <- comparison$beta[comparison$estimator == "Mean Group"]
sw_b <- comparison$beta[comparison$estimator == "Swamy RCM"]

p_forest <- slopes |>
  dplyr::mutate(iso2 = forcats::fct_reorder(iso2, beta)) |>
  ggplot(aes(x = beta, y = iso2)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "grey92", alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_vline(xintercept = mg_b, colour = "#1b6ca8", linetype = "dashed") +
  geom_vline(xintercept = sw_b, colour = "#c0392b", linetype = "dotted") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), orientation = "y",
                width = 0, colour = "grey55") +
  geom_point(aes(shape = sig10), size = 2) +
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16),
                     labels = c("n.s.", "p < 0.10"), name = NULL) +
  labs(
    title = "Country-specific GDP sensitivity of corporate default rates",
    subtitle = sprintf(
      "DR ~ GDP(MA4) + HICP(L1), by country. HAC NW(3) 95%% CI.  Dispersion: Q=%.1f (df=%d, p=%.4f), I2=%.0f%%\nDashed = Mean Group (%.3f)   Dotted = Swamy RCM (%.3f)",
      het_main$Q, het_main$df, het_main$p, 100 * het_main$I2, mg_b, sw_b),
    x = "beta on GDP growth (pp of DR per pp of GDP)", y = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(PATH_FIGURES, "country_slopes_forest.png"), p_forest,
       width = 8, height = 7, dpi = 300)

# ================================================================
# Tables
# ================================================================
fmt_tab <- function(df) df |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

write_kbl <- function(df, caption, stub) {
  writeLines(
    kableExtra::kbl(df, format = "latex", booktabs = TRUE, caption = caption) |>
      kableExtra::kable_styling(latex_options = "scale_down") |> as.character(),
    file.path(PATH_TABLES, paste0(stub, ".tex"))
  )
  writeLines(
    kableExtra::kbl(df, format = "html", caption = caption) |>
      kableExtra::kable_styling() |> as.character(),
    file.path(PATH_TABLES, paste0(stub, ".html"))
  )
}

write_kbl(fmt_tab(dplyr::bind_rows(
            comparison     |> dplyr::mutate(regressor = "GDP growth"),
            comparison_inf |> dplyr::mutate(regressor = "HICP inflation")
          ) |> dplyr::relocate(regressor)),
          paste0("Common vs heterogeneous slopes: country FE, Mean Group, Swamy ",
                 "RCM and CCE-MG. All rows on the same estimation sample and the ",
                 "same specification (gdp_ma4 + inf_l1); the FE with contemporaneous ",
                 "GDP is a different specification and is reported in the ",
                 "baselines table instead. The rows differ in their maintained ",
                 "assumptions about slope homogeneity, weighting and common-factor ",
                 "dependence: common slope, free slopes averaged equally, free ",
                 "slopes weighted by precision, and free slopes net of a common ",
                 "factor. Intervals use the t critical value with G-1 degrees of ",
                 "freedom. CCE-MG is the only row consistent with the rejected ",
                 "Pesaran CD test, and also the least stable point estimate; ",
                 "neither fact makes it the preferred estimate."),
          "estimator_comparison")

write_kbl(fmt_tab(dplyr::bind_rows(
            het_main |> dplyr::mutate(se_type = "HAC NW(3)"),
            het_ols  |> dplyr::mutate(se_type = "OLS"),
            het_cce  |> dplyr::mutate(se_type = "HAC NW(3), CCE-augmented")
          ) |> dplyr::relocate(se_type)),
          paste0("Slope dispersion across countries: Cochran Q, I2, tau. ",
                 "Supporting measures of dispersion, not a formal slope-homogeneity ",
                 "test; delta_std is a standardised transform of Q reported for ",
                 "reference and is not the Pesaran-Yamagata test. The chi-square ",
                 "reference distribution for Q assumes independent country ",
                 "estimates, which Pesaran CD rejects on this sample; the third ",
                 "row recomputes the same measures on country regressions ",
                 "augmented with cross-section averages, i.e. on the dispersion ",
                 "that remains once a common factor is projected out."),
          "slope_heterogeneity")

write_kbl(fmt_tab(country_compare),
          "Country classification: L4 lag vs MA4 window",
          "window_alignment")

readr::write_csv(
  slopes |>
    dplyr::left_join(weights_tab |> dplyr::select(iso2, sd_gdp_ma4, w_fe, c_fe, w_prec),
                     by = "iso2") |>
    dplyr::left_join(slopes_cce |> dplyr::select(iso2, beta_cce = beta, se_hac_cce = se_hac),
                     by = "iso2"),
  file.path(PATH_TABLES, "country_slopes.csv")
)

# ================================================================
# Save
# ================================================================
slope_het <- list(
  dat_ma          = dat_ma,
  fe_l0           = fit_fe_l0,
  fe_shared       = fit_fe_shared,
  fe_window       = list(l2 = fit_fe_l2, l4 = fit_fe_l4, ma4 = fit_fe_ma4),
  fe_noie         = fit_fe_noie,
  fe_no3          = fit_fe_no3,
  mg              = fit_mg,
  swamy           = fit_swamy,
  ccemg           = fit_ccemg,
  ccemg_slopes    = ccemg_b,
  ccemg_loo       = ccemg_loo,
  slopes          = slopes,
  slopes_cce      = slopes_cce,
  cd_country      = cd_country,
  cd_cce          = cd_cce,
  country_compare = country_compare,
  het_main        = het_main,
  het_ols         = het_ols,
  het_cce         = het_cce,
  het_by_lag      = het_by_lag,
  weights         = weights_tab,
  fe_decomp       = list(own = beta_own, cross = beta_cross, actual = beta_fe_actual),
  comparison      = comparison,
  comparison_inf  = comparison_inf,
  fe_l0_gdp       = fe_l0_gdp
)
saveRDS(slope_het, file.path(PATH_MODELS, "slope_het.rds"))

# ================================================================
# Write-up language (agreed wording)
# ================================================================
mg_row <- comparison |> dplyr::filter(estimator == "Mean Group")
sw_row <- comparison |> dplyr::filter(estimator == "Swamy RCM")
fe_row <- comparison |> dplyr::filter(estimator == "Country FE (MA4)")
cc_row <- comparison |> dplyr::filter(estimator == "CCE-MG")

cat("\n=== SENTENCES FOR THE WRITE-UP ===\n")

# Worth reading if you are wondering why the wording is generated rather than
# written.
#
# This block used to contain a fixed sentence: "both MG and Swamy produce
# materially larger negative average point estimates". That was true of an
# earlier, shorter sample. When the sample was extended it became false, but
# the sentence went on printing, directly above the coefficients that
# contradicted it.
#
# A template that asserts a sign no matter what the estimators return is a
# reliable way to publish a stale conclusion by accident. So the wording is now
# derived from the numbers themselves: if the estimates change, the sentence
# changes with them.
n_neg_slopes <- sum(slopes$beta < 0)
sign_word    <- function(b) if (b < 0) "negative" else "positive"

cat(sprintf(
"Country-specific estimates display slope dispersion (I2 = %.0f%%, tau = %.4f,
Cochran Q p = %.3f). %d of %d country slopes are negative, with a median of
%+.4f. These are supporting measures of dispersion, not a formal test of slope
homogeneity.\n\n",
  100 * het_main$I2, het_main$tau, het_main$p,
  n_neg_slopes, nrow(slopes), med_beta))

cat(sprintf(
"The common-slope fixed-effects model finds almost no average GDP sensitivity
(beta = %+.4f, p = %.2f). Allowing country-specific sensitivities does not
overturn that: Mean Group gives %+.4f (p = %.2f, %s), Swamy gives %+.4f
(p = %.3f, %s), and CCE-MG, which additionally projects out a common factor,
gives %+.4f (p = %.3f, %s).\n",
  fe_row$beta, fe_row$p,
  mg_row$beta, mg_row$p, sign_word(mg_row$beta),
  sw_row$beta, sw_row$p, sign_word(sw_row$beta),
  cc_row$beta, cc_row$p, sign_word(cc_row$beta)))

all_rows <- comparison
n_sig    <- sum(all_rows$p < 0.05, na.rm = TRUE)

if (n_sig == 0) {
  cat(sprintf(
"
None of the four estimators yields a precisely estimated average GDP
sensitivity at the 5%% level: every interval covers zero. What they do not do is
agree on a point. The estimates span %+.4f to %+.4f across the maintained
assumptions about slope homogeneity, weighting and common factors, and the
free-slope estimators do not even agree on the sign of the average. That spread
is itself the finding — the average European GDP response is not robustly
pinned down on this sample, and a single European multiplier should be reported
as imprecisely estimated and assumption-sensitive rather than as a small number
with a direction.\n",
    min(all_rows$beta), max(all_rows$beta)))
} else {
  cat(sprintf(
"
%d of the four estimators reach significance at the 5%% level. Because the four
differ in their maintained assumptions about slope homogeneity, weighting and
common factors, the write-up must say which assumptions deliver the result
rather than quoting the significant row alone.\n",
    n_sig))
}

if (cd_country$p_value < 0.05) {
  cat(sprintf("
Pesaran CD rejects cross-sectional independence on the country-regression
residuals (CD = %+.2f). Two consequences, and both belong in the write-up.
First, the Mean Group standard error assumes those regressions are independent,
so its p-value is optimistic. Second, the estimator built for this case,
CCE-MG, gives a materially larger point estimate (%+.4f against %+.4f) with a
p-value of %.3f — still not significant at 5%%, but far enough from the Mean
Group figure that reporting 'every estimator gives zero' would overstate how
settled the result is. The CCE-MG estimate is also the least stable of the four
(leave-one-country-out range %+.4f to %+.4f), so it widens the reported
uncertainty rather than replacing the conclusion.\n",
    cd_country$cd_stat, cc_row$beta, mg_row$beta, cc_row$p,
    min(ccemg_loo), max(ccemg_loo)))
}

cat(sprintf("
The same independence assumption underlies Cochran's Q, so the dispersion
measures are recomputed on the CCE-augmented country slopes. The dispersion
remains, and becomes larger, after the common-factor adjustment: I2 rises from
%.0f%% to %.0f%%. The heterogeneity is therefore not eliminated by that
adjustment — it is what remains after CCE augmentation.\n",
  100 * het_main$I2, 100 * het_cce$I2))

cat(sprintf("
The pooled FE coefficient decomposes exactly as %+.4f from the country GDP
slopes plus %+.4f from the cross-loading on the country inflation slopes,
totalling %+.4f. The own-slope term alone does not reproduce the estimate, so
the concentration of FE weight (top five countries %.1f%%, %.1f%% on
positive-slope countries) describes where much of the estimate's statistical
leverage comes from without being, by itself, the arithmetic explanation of the
pooled zero.\n",
  beta_own, beta_cross, beta_own + beta_cross, 100 * top5_fe, 100 * pos_share))

cat("\nSaved:\n")
cat("  ", file.path(PATH_MODELS,  "slope_het.rds"), "\n")
cat("  ", file.path(PATH_TABLES,  "estimator_comparison.{tex,html}"), "\n")
cat("  ", file.path(PATH_TABLES,  "slope_heterogeneity.{tex,html}"), "\n")
cat("  ", file.path(PATH_TABLES,  "window_alignment.{tex,html}"), "\n")
cat("  ", file.path(PATH_TABLES,  "country_slopes.csv"), "\n")
cat("  ", file.path(PATH_FIGURES, "country_slopes_forest.png"), "\n")
