# R/08_robustness.R
#
# STEP 8: Does the baseline result survive being poked at?
#
# Everything here starts from the same baseline: a country fixed-effects
# regression of the default rate on GDP growth and inflation, on the 26-country
# sample, with standard errors clustered by country. Each check changes one
# thing and reports what happens.
#
# The checks, and what each one is really asking:
#
#   R1  + COVID dummy      Does the GDP coefficient survive controlling for
#                          2020-21? This is the central test, because COVID is
#                          the largest macro movement in the sample.
#   R2  drop COVID         The blunter version of R1: remove those quarters
#                          entirely rather than dummying them out.
#   R3  time fixed effects Absorb whatever hit all countries in a given quarter.
#                          Not a formality here: the CD test in step 7 rejects,
#                          the default rate trends down over the sample while
#                          inflation trends up, and country fixed effects cannot
#                          remove any of that.
#   R4  Driscoll-Kraay SE  Standard errors robust to cross-sectional dependence.
#                          Since CD rejects, this is a correction rather than a
#                          confirmation.
#   R5  logit default rate The default rate is bounded and skewed. Does modelling
#                          it on a logit scale change anything? Values are
#                          floored at 0.01 pp so that zeros remain usable.
#   R6  recovery           Swap the dependent variable for 100 - LGD. Note this
#                          is an LGD-implied proxy, not observed recovery: LGD
#                          and loss rate are different concepts and are not
#                          interchangeable.
#   R6b recovery (LR)      The other recovery proxy, 100 - loss rate. Noisier
#                          and more often missing, which is why R6 is the main
#                          one, but the choice should not decide the answer.
#   R7  HICP at L1 only    Attacks the collinearity between contemporaneous and
#                          lagged inflation by dropping the contemporaneous term.
#   R8  drop outliers      Without Portugal, then without the wider periphery.
#                          Is the result carried by a handful of countries?
#   R9  linear trend       The default rate falls and inflation rises across the
#                          sample. A common trend separates that shared drift
#                          from genuine within-quarter macro sensitivity.
#   R10 MA4 horizon        Uses 4-quarter averages of both regressors. Does the
#                          result depend on measuring GDP persistently while
#                          inflation enters only at one lag?
#   R11 Q4 only            One observation per year. The EBA default rate is a
#                          rolling four-quarter figure, so consecutive quarters
#                          share three quarters of the same window; taking Q4
#                          only gives non-overlapping windows. Low power by
#                          construction. A check, never a baseline.
#   R12 pre/post-2020      Splits the sample where the EBA changed its reporting
#                          perimeter. Step 3b found no break in the published
#                          series, so this is a declared contingency check
#                          rather than a correction for a known problem.
#   R13 first differences  The standard response to a trending dependent
#                          variable, and the counterpart to R9: same regressors,
#                          but the trend is removed by differencing instead of
#                          being fitted. (13c) repeats it on the MA4 horizon.
#
# WHY THERE IS NO LAGGED DEPENDENT VARIABLE
#
# Satellite models in this literature are usually dynamic, so leaving out a
# lagged dependent variable is a choice that needs stating rather than passing
# over in silence.
#
# The reason is the construction of the default rate. It sums the last four
# quarters, so DR_t and DR_{t-1} share three quarters of the same window. That
# makes the error term a moving average by construction, and a lagged dependent
# variable is then mechanically correlated with it.
#
# This is worth being precise about, because it is often confused with Nickell
# bias. Nickell bias shrinks as T grows; this does not. It comes from how the
# variable is built, not from how many quarters there are, so a longer sample
# does not help. It is a specification error, not a small-sample one.
#
# The within-country AR(1) of the default rate is reported below so the claim is
# quantified rather than asserted. It is high, but persistence is not the
# argument. Endogeneity is.
#
# Inputs:  output/models/panel_model_ready.rds
# Outputs: output/models/robustness.rds
#          output/tables/robustness.{tex,html}
#          output/tables/robustness_dk.{tex,html}

source(here::here("R", "00_setup.R"))

# ================================================================
# Data
# ================================================================
panel <- readRDS(file.path(PATH_MODELS, "panel_model_ready.rds"))

dat <- panel |>
  dplyr::filter(is_main_sample, !is.na(default_rate)) |>
  dplyr::mutate(iso2 = as.factor(iso2))

dat_l1    <- dat |> dplyr::filter(!is.na(gdp_l1), !is.na(inf_l1))
dat_lr    <- dat |> dplyr::filter(!is.na(loss_rate)) |>
  dplyr::mutate(recovery_lr = 100 - loss_rate)
dat_nocov <- dat |> dplyr::filter(covid == 0L)
dat_nopt  <- dat |> dplyr::filter(iso2 != "PT")
dat_noper <- dat |> dplyr::filter(!iso2 %in% c("PT", "BG", "RO", "SI", "EL"))

cat("Sample sizes:\n")
cat(sprintf("  baseline (is_main_sample):  N=%d  (%d countries, %d quarters)\n",
            nrow(dat), dplyr::n_distinct(dat$iso2), dplyr::n_distinct(dat$quarter)))
cat(sprintf("  post-COVID (covid==0):      N=%d\n", nrow(dat_nocov)))
cat(sprintf("  main + L1 available:        N=%d\n", nrow(dat_l1)))
cat(sprintf("  drop PT:                    N=%d  (%d countries)\n",
            nrow(dat_nopt), dplyr::n_distinct(dat_nopt$iso2)))
cat(sprintf("  drop periphery (5):         N=%d  (%d countries)\n",
            nrow(dat_noper), dplyr::n_distinct(dat_noper$iso2)))
cat(sprintf("  loss_rate available:        N=%d  (%d countries)\n",
            nrow(dat_lr), dplyr::n_distinct(dat_lr$iso2)))

# ================================================================
# Baseline (for comparison)
# ================================================================
fit_base <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                  data = dat, vcov = ~ iso2)

# ================================================================
# R1: FE + COVID dummy (central test: does GDP recover?)
# ================================================================
fit_covid <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy + covid | iso2,
                   data = dat, vcov = ~ iso2)

# ================================================================
# R2: COVID exclusion (post-2021Q4 only)
# ================================================================
fit_nocov <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                   data = dat_nocov, vcov = ~ iso2)

# ================================================================
# R3: Time FE
# The COVID dummy is dropped here rather than forgotten: quarter fixed effects
# already absorb anything common to a given quarter, so a COVID dummy would be
# perfectly collinear with them.
# ================================================================
fit_time <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2 + quarter,
                  data = dat, vcov = ~ iso2)

# ================================================================
# R4: Driscoll-Kraay SE
# Driscoll-Kraay standard errors are robust to correlation across countries as
# well as over time. The CD test in step 7 rejects (z = 6.1), so this is a
# correction for a problem that is actually there, not a box-ticking exercise.
# Expect the DK standard errors to come out larger than the clustered ones.
#
# maxlag = 4 is chosen to span the EBA default rate's own four-quarter window,
# which is what makes adjacent observations so strongly dependent in the first
# place.
# ================================================================
pdat <- plm::pdata.frame(dat, index = c("iso2", "quarter_order"))
pfe  <- plm::plm(default_rate ~ gdp_growth_yoy + inflation_yoy,
                 data = pdat, model = "within")

cluster_se <- sqrt(diag(vcov(fit_base)))
dk_se      <- sqrt(diag(plm::vcovSCC(pfe, type = "HC0", maxlag = 4)))

dk_tab <- data.frame(
  param      = names(coef(pfe)),
  estimate   = round(unname(coef(pfe)), 5),
  cluster_se = round(unname(cluster_se[names(coef(pfe))]), 5),
  dk_se      = round(unname(dk_se), 5)
)
dk_tab$ratio_dk_over_cluster <- round(dk_tab$dk_se / dk_tab$cluster_se, 2)

# P-values use the t distribution with G-1 degrees of freedom, not the normal.
#
# Both standard errors in this table are effectively averages over G countries,
# and with G = 26 the normal quantile is about 5% too narrow, so using it would
# make every interval here slightly too confident. Steps 9 and 11 already apply
# the t rule for the same reason; this table was the last place still using
# pnorm, and two tables reporting the same kind of quantity should not disagree
# about how to compute a p-value.
G_DK   <- dplyr::n_distinct(dat$iso2)
p_tG   <- function(est, se) round(2 * stats::pt(-abs(est / se), df = G_DK - 1L), 4)
dk_tab$cluster_p <- p_tG(dk_tab$estimate, dk_tab$cluster_se)
dk_tab$dk_p      <- p_tG(dk_tab$estimate, dk_tab$dk_se)

# ================================================================
# R5: Logit DR
# The default rate is bounded between 0 and 100 and heavily skewed, so a linear
# model can in principle predict impossible values. The logit transform removes
# that constraint. The rate goes in as a fraction here, not as percentage points.
#
# The 0.01 pp floor (1e-4 as a fraction) is not cosmetic. Three observations sit
# at roughly 1e-8 (two in Bulgaria, one in Greece), and without a floor their
# logit is about -18, so far from everything else that those three points would
# dominate the fit. Flooring is standard practice in ECB satellite models for
# exactly this reason.
# ================================================================
FLOOR_PP   <- 0.01
FLOOR_FRAC <- FLOOR_PP / 100
CAP_FRAC   <- 1 - FLOOR_FRAC

dat_logit <- dat |>
  dplyr::mutate(
    dr_frac  = pmin(pmax(default_rate / 100, FLOOR_FRAC), CAP_FRAC),
    dr_logit = log(dr_frac / (1 - dr_frac))
  )

n_floored <- sum(dat$default_rate < FLOOR_PP, na.rm = TRUE)
n_capped  <- sum(dat$default_rate / 100 > CAP_FRAC, na.rm = TRUE)
cat(sprintf("\nLogit prep: floor at %.2f pp — %d obs floored, %d obs capped.\n",
            FLOOR_PP, n_floored, n_capped))

fit_logit <- feols(dr_logit ~ gdp_growth_yoy + inflation_yoy | iso2,
                   data = dat_logit, vcov = ~ iso2)

# ================================================================
# R6: Recovery regression
# ================================================================
fit_recov <- feols(recovery ~ gdp_growth_yoy + inflation_yoy | iso2,
                   data = dat, vcov = ~ iso2)

# R6b: The other recovery proxy: 100 - loss rate.
#
# R6 uses LGD, which is a regulatory estimate. This uses the loss rate, which is
# closer to realised losses. It is noisier and missing more often, so it runs on
# a smaller panel.
#
# These are two different concepts, not the same measure computed twice. The EBA
# methodology treats LGD and loss rate as distinct, so the two specifications
# complement each other rather than one being a check on the other's arithmetic.
fit_recov_lr <- feols(recovery_lr ~ gdp_growth_yoy + inflation_yoy | iso2,
                      data = dat_lr, vcov = ~ iso2)

# The inflation coefficient in R6b comes out marginally significant. Rather than
# report that and move on, it gets the same temporal controls that made the
# inflation coefficient on the default rate disappear. The rule is symmetry: a
# result should not be scrutinised only when it is inconvenient.
fit_recov_lr_covid <- feols(recovery_lr ~ gdp_growth_yoy + inflation_yoy + covid | iso2,
                            data = dat_lr, vcov = ~ iso2)

# ================================================================
# R7: HICP at L1 only
# Contemporaneous and lagged inflation are highly correlated with each other,
# which inflates both their variances and makes either one hard to read. This
# drops the contemporaneous term and keeps the lag, which both relieves the
# collinearity and is a defensible specification in its own right.
# ================================================================
fit_hicp_l1only <- feols(default_rate ~ gdp_growth_yoy + gdp_l1 + inf_l1 | iso2,
                         data = dat_l1, vcov = ~ iso2)

fit_pref <- feols(default_rate ~ gdp_growth_yoy + inf_l1 | iso2,
                  data = dat_l1, vcov = ~ iso2)

# HICP L1 under controls: does the lagged HICP effect survive COVID / time FE?
fit_pref_covid  <- feols(default_rate ~ gdp_growth_yoy + inf_l1 + covid | iso2,
                         data = dat_l1, vcov = ~ iso2)
fit_pref_timefe <- feols(default_rate ~ gdp_growth_yoy + inf_l1 | iso2 + quarter,
                         data = dat_l1, vcov = ~ iso2)

# Logit under COVID control: does the negative HICP survive under logit + COVID?
fit_logit_covid <- feols(dr_logit ~ gdp_growth_yoy + inflation_yoy + covid | iso2,
                         data = dat_logit, vcov = ~ iso2)

# ================================================================
# R8: Drop outliers
# ================================================================
fit_nopt  <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                   data = dat_nopt, vcov = ~ iso2)
fit_noper <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                   data = dat_noper, vcov = ~ iso2)

# ================================================================
# R9: Common linear trend
# This check exists because of a specific risk of spurious correlation.
#
# Across the sample the mean default rate falls steadily, from 1.91 to 1.32 to
# 1.02 pp across the pre-COVID, COVID and post-COVID periods, while mean
# inflation rises over the same three periods: 1.31, 1.81, 5.50. One series drifts down, the other drifts
# up. A regression of one on the other will find a negative coefficient whether
# or not there is any relationship between them.
#
# Country fixed effects do not help. They remove each country's average level,
# not a time path shared by all of them. So this specification adds a common
# linear trend and asks whether the macro coefficients survive once that shared
# drift is taken out.
# ================================================================
fit_trend <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy + quarter_order | iso2,
                   data = dat, vcov = ~ iso2)

# ================================================================
# R10: Horizon robustness: both regressors as 4-quarter averages
# Be careful about what this does and does not fix. It does NOT undo the
# overlapping window in the default rate. Nothing you do to the regressors can,
# because the problem is in the dependent variable. What it checks is narrower:
# whether the result depends on GDP entering as a persistent average while
# inflation enters at a single lag. Here both are measured the same way.
#
# fit_ma4_l1 keeps the earlier combination (4-quarter GDP, inflation at L1) so
# the two can be compared like for like.
# ================================================================
fit_ma4_l1  <- feols(default_rate ~ gdp_ma4 + inf_l1  | iso2, data = dat, vcov = ~ iso2)
fit_ma4_ma4 <- feols(default_rate ~ gdp_ma4 + inf_ma4 | iso2, data = dat, vcov = ~ iso2)

# ================================================================
# R11: Q4 only, so the windows do not overlap
# The default rate sums the last four quarters, so DR_t and DR_{t+1} share three
# quarters of the same underlying data. Keeping only Q4 of each year gives
# observations whose windows do not overlap at all: 2015Q4, 2016Q4, and so on.
# The exact span is printed below rather than restated here, so it cannot go
# stale.
#
# How to read this one. It throws away three quarters in four, so it is a narrow
# check and never a baseline. The question it answers is "with the overlap
# mechanically removed, is the sign and order of magnitude still consistent?"
#
# In particular, if significance disappears here, that is a consequence of
# having a quarter of the observations. It is not evidence that the quarterly
# panel was biased.
# ================================================================
dat_q4 <- dat |> dplyr::filter(stringr::str_detect(quarter, "-Q4$"))

n_q4_years <- dplyr::n_distinct(dat_q4$quarter)
n_q4_per_country <- dat_q4 |>
  dplyr::count(iso2) |> dplyr::pull(n)
cat(sprintf("\nQ4-only subsample: N=%d, %d annual periods (%s), %d-%d obs per country\n",
            nrow(dat_q4), n_q4_years,
            paste(range(dat_q4$quarter), collapse = " .. "),
            min(n_q4_per_country), max(n_q4_per_country)))

fit_q4      <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                     data = dat_q4, vcov = ~ iso2)
fit_q4_ma4  <- feols(default_rate ~ gdp_ma4 + inf_l1 | iso2,
                     data = dat_q4, vcov = ~ iso2)

# ================================================================
# R12: pre/post-2020 split
# Step 3b already looked at the 2019Q4 -> 2020Q1 boundary and found it sits
# inside the ordinary range of quarterly default-rate changes on all three
# measures, so the panel is treated as one continuous series.
#
# The split is reported anyway. The EBA did move the Risk Dashboard to an EU27
# presentation in 2020Q1, and a reader is entitled to wonder whether that
# matters. Better to run the check and show it than to leave the question open.
# ================================================================
dat_pre20  <- dat |> dplyr::filter(quarter_date <  as.Date("2020-01-01"))
dat_post20 <- dat |> dplyr::filter(quarter_date >= as.Date("2020-01-01"))

fit_pre20  <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                    data = dat_pre20,  vcov = ~ iso2)
fit_post20 <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
                    data = dat_post20, vcov = ~ iso2)

cat(sprintf("Split: pre-2020 N=%d (%d quarters), post-2020 N=%d (%d quarters)\n",
            nrow(dat_pre20),  dplyr::n_distinct(dat_pre20$quarter),
            nrow(dat_post20), dplyr::n_distinct(dat_post20$quarter)))

# ================================================================
# R13: First differences
# The counterpart to R9. R9 removes the shared trend by fitting it; this removes
# it by differencing. Two different ways of handling the same problem, which is
# more convincing than either on its own.
#
# One thing to watch: differencing is only meaningful between CONSECUTIVE
# quarters. Seven countries have gaps where the EBA published nothing, and
# differencing across a gap would silently produce a two- or three-quarter
# change labelled as a one-quarter one. Those rows are dropped instead.
# ================================================================
#
# The differencing is applied to the BASELINE regressors, contemporaneous GDP
# and inflation, because that is the specification R9 fits the trend into.
#
# This was wrong at one point and is worth flagging. An earlier version
# differenced the MA4 specification instead, which meant the write-up's claim
# that "differencing gives the same answer as the trend specification" was
# comparing two different specifications. The MA4 variant is still here as a
# second column, so the horizon choice stays covered, but it is no longer the
# one that sentence rests on.
dat_fd <- dat |>
  dplyr::arrange(iso2, quarter_order) |>
  dplyr::group_by(iso2) |>
  dplyr::mutate(
    d_dr      = default_rate   - dplyr::lag(default_rate),
    d_gdp     = gdp_growth_yoy - dplyr::lag(gdp_growth_yoy),
    d_inf     = inflation_yoy  - dplyr::lag(inflation_yoy),
    d_gdp_ma4 = gdp_ma4        - dplyr::lag(gdp_ma4),
    d_inf_l1  = inf_l1         - dplyr::lag(inf_l1),
    gap       = quarter_order  - dplyr::lag(quarter_order)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(gap == 1L, !is.na(d_dr))

cat(sprintf("First differences: N=%d (%d dropped at series gaps or first quarter)\n",
            nrow(dat_fd), nrow(dat) - nrow(dat_fd)))

fit_fd      <- feols(d_dr ~ d_gdp + d_inf | iso2,                 data = dat_fd, vcov = ~ iso2)
fit_fd_time <- feols(d_dr ~ d_gdp + d_inf | iso2 + quarter_order, data = dat_fd, vcov = ~ iso2)
fit_fd_ma4  <- feols(d_dr ~ d_gdp_ma4 + d_inf_l1 | iso2,          data = dat_fd, vcov = ~ iso2)

# How persistent is the default rate? This is the number behind the "no lagged
# dependent variable" argument in the header. The claim is quantified here
# rather than left as an assertion.
#
# Computed on CONSECUTIVE quarters only, the same rule the first differences
# use. Seven countries have holes in their series, because the EBA does not
# publish a country-quarter with too few IRB reporters. A plain
# cor(DR, lag(DR)) would treat the quarters either side of a hole as adjacent.
# There are 35 such pairs in this sample.
#
# That matters more here than elsewhere. Quoting a persistence figure built that
# way, in the very paragraph explaining why the lagged dependent variable is
# excluded, would mean arguing from a statistic the argument itself says not to
# trust.
ar1_pairs <- dat |>
  dplyr::arrange(iso2, quarter_order) |>
  dplyr::group_by(iso2) |>
  dplyr::mutate(dr_lag = dplyr::lag(default_rate),
                gap    = quarter_order - dplyr::lag(quarter_order)) |>
  dplyr::ungroup() |>
  dplyr::filter(gap == 1L, !is.na(default_rate), !is.na(dr_lag))

ar1 <- ar1_pairs |>
  dplyr::group_by(iso2) |>
  dplyr::summarise(rho = stats::cor(default_rate, dr_lag), n_pairs = dplyr::n(),
                   .groups = "drop")

n_dropped_pairs <- (nrow(dat) - dplyr::n_distinct(dat$iso2)) - nrow(ar1_pairs)
cat(sprintf(paste0("DR within-country AR(1), consecutive quarters only: median %.3f, ",
                   "range %.3f-%.3f (n=%d countries, %d pairs dropped at series gaps)\n"),
            stats::median(ar1$rho), min(ar1$rho), max(ar1$rho), nrow(ar1),
            n_dropped_pairs))

# ================================================================
# Save all
# ================================================================
robustness <- list(
  baseline    = fit_base,
  covid       = fit_covid,
  no_covid    = fit_nocov,
  time_fe     = fit_time,
  dk_tab      = dk_tab,
  pfe_plm     = pfe,
  logit       = fit_logit,
  recovery       = fit_recov,
  recovery_lr    = fit_recov_lr,
  recovery_lr_cv = fit_recov_lr_covid,
  hicp_l1only = fit_hicp_l1only,
  pref        = fit_pref,
  pref_covid  = fit_pref_covid,
  pref_timefe = fit_pref_timefe,
  logit_covid = fit_logit_covid,
  no_pt       = fit_nopt,
  no_peri     = fit_noper,
  # --- the trend, horizon, non-overlapping and split specifications ---
  trend       = fit_trend,
  ma4_l1      = fit_ma4_l1,
  ma4_ma4     = fit_ma4_ma4,
  q4_only     = fit_q4,
  q4_only_ma4 = fit_q4_ma4,
  pre2020     = fit_pre20,
  post2020    = fit_post20,
  fd          = fit_fd,
  fd_time     = fit_fd_time,
  fd_ma4      = fit_fd_ma4,
  dr_ar1      = ar1
)
saveRDS(robustness, file.path(PATH_MODELS, "robustness.rds"))

# ================================================================
# Table 1: main robustness comparison
# ================================================================
coef_map <- c(
  "gdp_growth_yoy" = "GDP YoY",
  "gdp_l1"         = "GDP YoY (L1)",
  "inflation_yoy"  = "HICP YoY",
  "inf_l1"         = "HICP YoY (L1)",
  "gdp_ma4"        = "GDP YoY (MA4)",
  "inf_ma4"        = "HICP YoY (MA4)",
  "quarter_order"  = "Linear trend",
  "covid"          = "COVID dummy",
  "d_gdp"          = "D.GDP YoY",
  "d_inf"          = "D.HICP YoY",
  "d_gdp_ma4"      = "D.GDP YoY (MA4)",
  "d_inf_l1"       = "D.HICP YoY (L1)"
)

models_dr <- list(
  "(0) Baseline FE"      = fit_base,
  "(1) + COVID dummy"     = fit_covid,
  "(2) COVID excl."       = fit_nocov,
  "(3) Time FE"           = fit_time,
  "(4) HICP L1 only"      = fit_hicp_l1only,
  "(5) GDP + HICP L1"     = fit_pref,
  "(5a) Pref + COVID"     = fit_pref_covid,
  "(5b) Pref + Time FE"   = fit_pref_timefe,
  "(6) Drop PT"           = fit_nopt,
  "(7) Drop periphery"    = fit_noper
)

notes_dr <- c(
  sprintf(paste0("DV: default_rate (pp). Baseline (0) = FE, dr ~ gdp + inf | iso2, ",
                 "is_main_sample (%d countries, %d quarters, N=%d)."),
          dplyr::n_distinct(dat$iso2), dplyr::n_distinct(dat$quarter), nrow(dat)),
  "SE clustered by country. Time FE (col 3) drops COVID dummy by collinearity.",
  "Drop PT: baseline without Portugal (top outlier). Drop periphery: without PT, BG, RO, SI, EL."
)

modelsummary(models_dr, coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
             stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             fmt = 3, notes = notes_dr,
             output = file.path(PATH_TABLES, "robustness.tex"))
modelsummary(models_dr, coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
             stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             fmt = 3, notes = notes_dr,
             output = file.path(PATH_TABLES, "robustness.html"))

# ================================================================
# Table 2: auxiliary DVs (logit + recovery)
# ================================================================
models_aux <- list(
  "Logit(DR)"                     = fit_logit,
  "Logit(DR) + COVID"             = fit_logit_covid,
  "LGD-implied recovery proxy"    = fit_recov,
  "Loss-rate recovery proxy"      = fit_recov_lr,
  "Loss-rate proxy + COVID"       = fit_recov_lr_covid
)

notes_aux <- c(
  "Logit: dep = log(DR/(1-DR)) with DR in fraction and floor 0.01pp.",
  paste0("LGD-implied recovery proxy: dep = 100 - LGD (D5 baseline). This is a ",
         "proxy, not observed/realised recovery. Sample same as (0)."),
  paste0("Loss-rate recovery proxy: dep = 100 - loss_rate (realised-loss-based, ",
         "D5 robustness). Sub-panel with loss_rate reported; LGD and Loss Rate ",
         "are not interchangeable concepts. Its marginally significant inflation ",
         "coefficient does not survive a COVID control — same pattern as the ",
         "default-rate inflation coefficient.")
)

modelsummary(models_aux, coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
             stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             fmt = 3, notes = notes_aux,
             output = file.path(PATH_TABLES, "robustness_aux.tex"))
modelsummary(models_aux, coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
             stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             fmt = 3, notes = notes_aux,
             output = file.path(PATH_TABLES, "robustness_aux.html"))

# ================================================================
# Table 2b: trend, horizon, non-overlapping subsample, sample split
# ================================================================
models_v2 <- list(
  "(0) Baseline FE"       = fit_base,
  "(9) + linear trend"    = fit_trend,
  "(10a) MA4 + HICP L1"   = fit_ma4_l1,
  "(10b) MA4 + HICP MA4"  = fit_ma4_ma4,
  "(11a) Q4-only"         = fit_q4,
  "(11b) Q4-only, MA4"    = fit_q4_ma4,
  "(12a) Pre-2020"        = fit_pre20,
  "(12b) 2020 onwards"    = fit_post20,
  "(13a) First diff."     = fit_fd,
  "(13b) FD + time FE"    = fit_fd_time,
  "(13c) FD, MA4 spec"    = fit_fd_ma4
)

notes_v2 <- c(
  paste0("Specs added for the extended 2015Q4-2026Q1 sample. DV: default_rate (pp), ",
         "SE clustered by country, country FE throughout."),
  paste0("(9) A common linear trend. The extended sample has a falling DR and a rising ",
         "HICP; country FE absorb country means, not a shared time path."),
  paste0("(10) Horizon check. Measuring both regressors as four-quarter averages does ",
         "not remove the default rate's overlapping window — no regressor can."),
  sprintf(paste0("(11) Non-overlapping annual subsample: Q4 of each year only, N=%d over ",
                 "%d annual periods. Windows no longer overlap, at the cost of three ",
                 "quarters in four. Directional evidence, not a decisive test."),
          nrow(dat_q4), n_q4_years),
  paste0("(12) Split at the EBA EU27 presentation change. 03b_compat_check.R found the ",
         "2019Q4-2020Q1 seam within the ordinary range of quarterly DR changes; the ",
         "split is reported as a declared check, not as a correction."),
  sprintf(paste0("(13) First differences on consecutive quarters only (N=%d): the trend ",
                 "is differenced out rather than fitted. (13a) and (13b) difference the ",
                 "BASELINE regressors, so they are the like-for-like complement to (9); ",
                 "(13c) repeats the exercise on the MA4 horizon. DV is the quarterly ",
                 "change in the default rate, so the coefficients are not comparable in ",
                 "level to the other columns."), nrow(dat_fd))
)

modelsummary(models_v2, coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
             stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             fmt = 3, notes = notes_v2,
             output = file.path(PATH_TABLES, "robustness_v2.tex"))
modelsummary(models_v2, coef_map = coef_map,
             gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
             stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             fmt = 3, notes = notes_v2,
             output = file.path(PATH_TABLES, "robustness_v2.html"))

# ================================================================
# Table 3: Cluster vs Driscoll-Kraay
# ================================================================
writeLines(
  kableExtra::kbl(dk_tab, format = "latex", booktabs = TRUE,
                  caption = paste0("Baseline FE — cluster vs Driscoll-Kraay SEs (maxlag=4). ",
                                    "p-values use the t critical value with G-1 degrees of freedom.")) |>
    kableExtra::kable_styling(latex_options = "scale_down") |> as.character(),
  file.path(PATH_TABLES, "robustness_dk.tex")
)
writeLines(
  kableExtra::kbl(dk_tab, format = "html",
                  caption = paste0("Baseline FE — cluster vs Driscoll-Kraay SEs (maxlag=4). ",
                                    "p-values use the t critical value with G-1 degrees of freedom.")) |>
    kableExtra::kable_styling() |> as.character(),
  file.path(PATH_TABLES, "robustness_dk.html")
)

# ================================================================
# Console summaries + narrative
# ================================================================
cat("\n=== ROBUSTNESS — DR specs ===\n")
etable(fit_base, fit_covid, fit_nocov, fit_time,
       fit_hicp_l1only, fit_pref, fit_nopt, fit_noper,
       headers = c("Base", "+COVID", "NoCOVID", "TimeFE",
                   "HICP-L1", "Pref", "NoPT", "NoPeri"),
       digits = 3, fitstat = c("n", "r2", "wr2"))

cat("\n=== ROBUSTNESS — HICP L1 under controls ===\n")
etable(fit_pref, fit_pref_covid, fit_pref_timefe,
       headers = c("Pref", "Pref+COVID", "Pref+TimeFE"),
       digits = 3, fitstat = c("n", "r2", "wr2"))

cat("\n=== ROBUSTNESS — V2 specs (trend / horizon / non-overlapping / split) ===\n")
etable(fit_base, fit_trend, fit_ma4_l1, fit_ma4_ma4,
       fit_q4, fit_q4_ma4, fit_pre20, fit_post20,
       headers = c("Base", "+Trend", "MA4+L1", "MA4+MA4",
                   "Q4only", "Q4only-MA4", "Pre2020", "Post2020"),
       digits = 4, fitstat = c("n", "wr2"))

cat("\n=== ROBUSTNESS — first differences (DV = quarterly change in DR) ===\n")
etable(fit_fd, fit_fd_time, fit_fd_ma4,
       headers = c("FD (baseline spec)", "FD + time FE", "FD (MA4 spec)"),
       digits = 4, fitstat = c("n", "wr2"))

cat("\n=== ROBUSTNESS — auxiliary DVs ===\n")
etable(fit_logit, fit_logit_covid, fit_recov, fit_recov_lr, fit_recov_lr_covid,
       headers = c("Logit DR", "Logit+COVID", "Recovery (LGD)", "Recovery (LR)",
                   "Rec(LR)+COVID"),
       digits = 3, fitstat = c("n", "r2", "wr2"))

cat("\n=== Cluster vs Driscoll-Kraay (baseline FE) ===\n")
print(dk_tab)

# --- Central story extraction ---
extract <- function(fit, param) {
  s <- summary(fit)$coeftable
  if (!param %in% rownames(s)) return(c(NA_real_, NA_real_, NA_real_))
  c(s[param, "Estimate"], s[param, "Std. Error"], s[param, "Pr(>|t|)"])
}

story_tab <- data.frame(
  spec = c("(0) Baseline", "(1) +COVID", "(2) COVID excl", "(3) Time FE",
           "(6) Drop PT", "(7) Drop periphery"),
  gdp_est  = c(extract(fit_base,"gdp_growth_yoy")[1], extract(fit_covid,"gdp_growth_yoy")[1],
               extract(fit_nocov,"gdp_growth_yoy")[1], extract(fit_time,"gdp_growth_yoy")[1],
               extract(fit_nopt,"gdp_growth_yoy")[1], extract(fit_noper,"gdp_growth_yoy")[1]),
  gdp_p    = c(extract(fit_base,"gdp_growth_yoy")[3], extract(fit_covid,"gdp_growth_yoy")[3],
               extract(fit_nocov,"gdp_growth_yoy")[3], extract(fit_time,"gdp_growth_yoy")[3],
               extract(fit_nopt,"gdp_growth_yoy")[3], extract(fit_noper,"gdp_growth_yoy")[3]),
  hicp_est = c(extract(fit_base,"inflation_yoy")[1], extract(fit_covid,"inflation_yoy")[1],
               extract(fit_nocov,"inflation_yoy")[1], extract(fit_time,"inflation_yoy")[1],
               extract(fit_nopt,"inflation_yoy")[1], extract(fit_noper,"inflation_yoy")[1]),
  hicp_p   = c(extract(fit_base,"inflation_yoy")[3], extract(fit_covid,"inflation_yoy")[3],
               extract(fit_nocov,"inflation_yoy")[3], extract(fit_time,"inflation_yoy")[3],
               extract(fit_nopt,"inflation_yoy")[3], extract(fit_noper,"inflation_yoy")[3])
)
story_tab[, -1] <- round(story_tab[, -1], 4)

cat("\n=== STORY — GDP and HICP across key specs ===\n")
print(story_tab, row.names = FALSE)

covid_coef <- extract(fit_covid, "covid")
cat(sprintf("\nCOVID dummy: β=%.3f  SE=%.3f  p=%.4f\n",
            covid_coef[1], covid_coef[2], covid_coef[3]))

# Settled reading of these specs. Kept as the actual
# conclusions, not as the ex-ante prediction menu this block used to hold.
cat("\nSettled reading — extended 2015Q4-2026Q1 sample:\n")
cat(" 1. GDP: no configuration here rescues it. Across every FE spec above,\n")
cat("    including the non-overlapping Q4-only subsample, the coefficient stays\n")
cat("    small and far from significance. The common-slope FE finds almost no\n")
cat("    average GDP sensitivity — a real result under that restriction, not a\n")
cat("    technical failure. Heterogeneous slopes are handled in 09.\n")
cat(" 2. HICP: the negative coefficient is larger and more precisely estimated\n")
cat("    than on the 25-quarter sample, and it now survives the COVID dummy,\n")
cat("    COVID exclusion, outlier drops, the logit transform, Driscoll-Kraay SEs\n")
cat("    and the non-overlapping Q4-only subsample. It does NOT survive a common\n")
cat("    time control: it goes to -0.030 (p=0.37) under quarter FE, and a single\n")
cat("    linear trend takes within-R2 from 0.05 to 0.15 while cutting the\n")
cat("    coefficient by two thirds.\n")
cat("    Reading: over 2015Q4-2026Q1 the default rate trends down (1.91 -> 1.32\n")
cat("    -> 1.02 pp) while HICP trends up (1.31 -> 1.81 -> 5.50 pp). Country FE\n")
cat("    absorb country means, not that shared path, so what the baseline picks\n")
cat("    up is level co-movement over time rather than within-quarter inflation\n")
cat("    sensitivity. Still 'not robust' — never 'spurious': the specs establish\n")
cat("    that the coefficient is not robust to controls for the common time path,\n")
cat("    not which mechanism produced either trend.\n")
cat(" 3. Outlier robustness: coefficients stable -> finding not driven by outliers.\n")
cat(" 4. DK / cluster ratio is now ABOVE 1 (see table): Driscoll-Kraay SEs are\n")
cat("    wider than clustered ones, consistent with 07 where Pesaran CD rejects\n")
cat("    on the extended sample. Cluster SEs are no longer the conservative\n")
cat("    choice here, which is the opposite of the 25-quarter result. Inference\n")
cat("    on HICP is reported under both.\n")
cat(" 5. Recovery is an LGD-implied recovery proxy (100 - LGD), not observed\n")
cat("    recovery: little systematic sensitivity to GDP and inflation.\n")
cat(" 6. Loss-rate recovery proxy (100 - loss_rate): on the extended sample\n")
cat("    neither regressor is significant, so the marginal inflation coefficient\n")
cat("    that needed a COVID control on the short sample is no longer present.\n")
cat(" 7. Pre/post-2020 split: same sign on both sides of the EBA presentation\n")
cat("    change for both regressors. No sign reversal at the seam, in line with\n")
cat("    03b_compat_check.R.\n")
cat(" 8. First differences: both macro coefficients are small and insignificant,\n")
cat("    and within-R2 is essentially zero. (13a) differences the SAME regressors\n")
cat("    the trend spec (9) fits a trend into, so the two are a like-for-like pair:\n")
cat("    differencing the common time path out rather than fitting it gives the\n")
cat("    same answer, and the result does not depend on how that path is handled.\n")
cat("    (13c) repeats it on the MA4 horizon. The coefficients are on the quarterly\n")
cat("    CHANGE in the default rate and are not comparable in level to the other\n")
cat("    specs.\n")
cat(" 9. No lagged dependent variable, by construction rather than by omission:\n")
cat("    the overlapping four-quarter window makes the disturbance a moving\n")
cat("    average, so an LDV would be correlated with it at any T. See the header\n")
cat("    of this file; the AR(1) figures above quantify the persistence.\n")

cat("\nSaved:\n")
cat("  ", file.path(PATH_MODELS, "robustness.rds"), "\n")
cat("  ", file.path(PATH_TABLES, "robustness.{tex,html}"), "\n")
cat("  ", file.path(PATH_TABLES, "robustness_aux.{tex,html}"), "\n")
cat("  ", file.path(PATH_TABLES, "robustness_dk.{tex,html}"), "\n")
