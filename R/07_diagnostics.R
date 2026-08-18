# R/07_diagnostics.R
#
# STEP 7: Do the baselines hold up?
#
# Every regression makes assumptions. This step checks them one at a time, in
# the order that makes sense: first whether the series are stationary (if they
# are not, the regression may be spurious), then multicollinearity, then the
# three things that affect standard errors rather than coefficients
# (heteroskedasticity, serial correlation, cross-sectional dependence), then a
# look at the residuals, and finally what to do about all of it.
#
# Blocks 1-6 diagnose. Block 7 responds: with only 26 clusters, the usual
# clustered standard errors are known to be too small, so it adds CR2
# (Bell-McCaffrey) standard errors and a wild cluster bootstrap.
#
# Worth saying plainly: several of these tests reject. That is reported rather
# than worked around. What each rejection means for the conclusions is dealt
# with where it matters. The cross-sectional dependence result, for instance, is
# the reason step 9 estimates a CCE-MG model at all.

source(here::here("R", "00_setup.R"))

# ---- Load the data and refit the baselines ---------------------------------
# The models are refitted here rather than read back from baselines.rds.
# fwildclusterboot needs a live model object with its data still attached; a
# deserialised fit does not carry what the bootstrap needs to resample.
panel <- readRDS(file.path(PATH_MODELS, "panel_model_ready.rds"))

dat <- panel |>
  dplyr::filter(is_main_sample, !is.na(default_rate)) |>
  dplyr::mutate(iso2 = as.factor(iso2))

dat_l1 <- dat |> dplyr::filter(!is.na(gdp_l1), !is.na(inf_l1))

fit_pooled <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy,               dat, cluster = ~ iso2)
fit_fe     <- feols(default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,        dat, cluster = ~ iso2)
fit_fe_l1  <- feols(default_rate ~ gdp_l1 + inf_l1 | iso2,                        dat, cluster = ~ iso2)
fit_fe_dl  <- feols(default_rate ~ gdp_growth_yoy + gdp_l1 +
                                    inflation_yoy + inf_l1 | iso2,               dat, cluster = ~ iso2)

pdat     <- pdata.frame(dat,    index = c("iso2", "quarter_order"))
pdat_l1  <- pdata.frame(dat_l1, index = c("iso2", "quarter_order"))

pfe_base <- plm::plm(default_rate ~ gdp_growth_yoy + inflation_yoy,
                     data = pdat, model = "within")
pfe_dl   <- plm::plm(default_rate ~ gdp_growth_yoy + gdp_l1 + inflation_yoy + inf_l1,
                     data = pdat_l1, model = "within")

# Sample dimensions are read off the data, never written by hand. The panel grew
# from 25 to 42 quarters partway through this project, and every hard-coded
# constant became wrong at that moment without anything failing. The
# balanced-subset filter below was one of them.
N_COUNTRY <- dplyr::n_distinct(dat$iso2)
N_QUARTER <- dplyr::n_distinct(dat$quarter_order)

cat("Sample: ", N_COUNTRY, " countries, T=", N_QUARTER,
    " — N=", nrow(dat), " (DL: ", nrow(dat_l1), ")\n\n", sep = "")

# ================================================================
# Block 1: Unit root / stationarity
# ================================================================
cat("=== 1. Unit root ===\n")

# Two tests, because they need different things: IPS requires a balanced panel,
# Fisher-ADF (Maddala-Wu) does not.
#
# There is a trap here worth explaining, because it caught this project once.
#
# The macro series are balanced in the panel itself. But `dat` drops the rows
# where the EBA published no default rate, and dropping those rows unbalances
# GDP and inflation too, so pdim(pdat)$balanced comes back FALSE. An earlier
# version of this script claimed in a comment that pdat was balanced and then
# handed it to IPS anyway. purtest runs happily on it and returns a number, so
# nothing looked wrong.
#
# The fix is to give each test the panel it is actually entitled to:
#   IPS on the macro series  -> pdat_bal,    all countries x all quarters
#   IPS on the default rate  -> pdat_dr_bal, only countries with no DR gaps
#   Fisher-ADF (everything)  -> pdat,        the estimation sample as it is
pdat_bal <- pdata.frame(
  panel |> dplyr::filter(is_main_sample) |> dplyr::mutate(iso2 = droplevels(as.factor(iso2))),
  index = c("iso2", "quarter_order")
)

dr_balanced_iso <- dat |>
  dplyr::group_by(iso2) |>
  dplyr::summarise(n = sum(!is.na(default_rate)), .groups = "drop") |>
  dplyr::filter(n == N_QUARTER) |> dplyr::pull(iso2)

n_bal <- length(dr_balanced_iso)
stopifnot(n_bal >= 5)   # if this subset empties, IPS/CIPS below fail cryptically
cat("Balanced DR subset: ", n_bal, " countries with all ", N_QUARTER,
    " quarters observed\n", sep = "")

pdat_dr_bal <- pdata.frame(
  dat |> dplyr::filter(iso2 %in% dr_balanced_iso) |> dplyr::mutate(iso2 = droplevels(iso2)),
  index = c("iso2", "quarter_order")
)

# The claim the split above rests on, checked in code rather than asserted in a
# comment. Asserting it in a comment is precisely how the bug above survived.
stopifnot(plm::pdim(pdat_bal)$balanced, plm::pdim(pdat_dr_bal)$balanced)

ur_specs <- list(
  list(var = "default_rate",   test = "IPS (balanced subset)", data = pdat_dr_bal, method = "ips"),
  list(var = "default_rate",   test = "Fisher-ADF (MW)",    data = pdat,        method = "madwu"),
  list(var = "gdp_growth_yoy", test = "IPS (balanced)",     data = pdat_bal,    method = "ips"),
  list(var = "gdp_growth_yoy", test = "Fisher-ADF (MW)",    data = pdat,        method = "madwu"),
  list(var = "inflation_yoy",  test = "IPS (balanced)",     data = pdat_bal,    method = "ips"),
  list(var = "inflation_yoy",  test = "Fisher-ADF (MW)",    data = pdat,        method = "madwu")
)

ur_df <- purrr::map_dfr(ur_specs, function(s) {
  fml <- as.formula(paste(s$var, "~ 1"))
  res <- plm::purtest(fml, data = s$data, test = s$method, lags = "AIC", pmax = 4)
  tibble::tibble(
    variable = s$var, test = s$test,
    statistic = as.numeric(res$statistic$statistic),
    p_value = as.numeric(res$statistic$p.value)
  )
}) |>
  dplyr::mutate(
    interpretation = ifelse(p_value < 0.05,
                            "reject H0 unit root → stationary",
                            paste0("fail to reject → non-stationary (T=", N_QUARTER, " power)"))
  )

# --- CIPS (Pesaran 2007, second generation) ---------------------------------
# IPS and Fisher-ADF both assume countries are independent of each other. In
# this panel they clearly are not: COVID hit everywhere at once, and so did the
# 2022 energy shock. When series move together like that, first-generation tests
# reject the unit-root null too often, so a "stationary" verdict from them is
# not worth much on its own.
#
# CIPS handles it by augmenting each country's ADF regression with
# cross-sectional averages, which soaks up the common component. It needs a
# balanced panel, so it reuses the two built for IPS above: pdat_bal for the
# macro series, and the balanced-DR subset for the default rate. That subset's
# size is printed above rather than hard-coded here.
cips_specs <- list(
  list(var = "default_rate",   data = pdat_dr_bal, label = "CIPS (balanced subset)"),
  list(var = "gdp_growth_yoy", data = pdat_bal,    label = "CIPS"),
  list(var = "inflation_yoy",  data = pdat_bal,    label = "CIPS")
)

cips_df <- purrr::map_dfr(cips_specs, function(s) {
  res <- tryCatch(
    plm::cipstest(s$data[[s$var]], lags = 2, type = "drift"),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(tibble::tibble(variable = s$var, test = s$label,
                          statistic = NA_real_, p_value = NA_real_))
  }
  tibble::tibble(variable = s$var, test = s$label,
                 statistic = as.numeric(res$statistic),
                 p_value = as.numeric(res$p.value))
}) |>
  dplyr::mutate(
    interpretation = dplyr::case_when(
      is.na(p_value)  ~ "test not computable",
      p_value < 0.05  ~ "reject H0 unit root → stationary (robust to CD)",
      TRUE            ~ "fail to reject (CIPS p-values truncated at table bounds)"
    )
  )

ur_df <- dplyr::bind_rows(ur_df, cips_df)

print(ur_df)

# ================================================================
# Block 2: Multicollinearity
# ================================================================
cat("\n=== 2. Multicollinearity ===\n")

cor_vars <- dat_l1 |> dplyr::select(gdp_growth_yoy, gdp_l1, inflation_yoy, inf_l1)
cor_mat <- cor(cor_vars)
cat("Correlation matrix (spec 4 regressors, N =", nrow(dat_l1), "):\n")
print(round(cor_mat, 3))

# VIF is computed on a macro-only OLS rather than on the FE model. Running it on
# the FE model would return a GVIF, which is harder to read against the usual
# rules of thumb. The question here is only whether the macro regressors are
# collinear with each other, and this answers it directly.
vif_lm <- lm(default_rate ~ gdp_growth_yoy + gdp_l1 + inflation_yoy + inf_l1, data = dat_l1)
vifs <- car::vif(vif_lm)
cat("\nVIF (spec 4 regressors, macro-only OLS):\n")
print(round(vifs, 2))

# ================================================================
# Block 3: Heteroskedasticity
# ================================================================
cat("\n=== 3. Heteroskedasticity ===\n")

# Breusch-Pagan asks whether the residual variance moves with the fitted values,
# i.e. whether the model is noisier at high default rates than at low ones.
bp <- lmtest::bptest(pfe_base)
cat("Breusch-Pagan on FE baseline (functional-form hetero):\n")
print(bp)

# Levene asks a different question: are the residual variances the same ACROSS
# countries? That is the form heteroskedasticity usually takes in a fixed-effects
# panel, where some countries are simply more volatile than others, and it is
# what Greene's modified Wald test is for. Levene is the equivalent here.
resid_fe <- residuals(fit_fe)
lev <- car::leveneTest(resid_fe ~ dat$iso2, center = "median")
cat("\nLevene (groupwise hetero across countries):\n")
print(lev)

var_by_country <- dat |> dplyr::mutate(r = resid_fe) |>
  dplyr::group_by(iso2) |>
  dplyr::summarise(var = var(r, na.rm = TRUE), n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(desc(var))

cat("\nResidual variance by country — top 3 / bottom 3:\n")
print(dplyr::bind_rows(head(var_by_country, 3), tail(var_by_country, 3)))
cat("  max/min ratio:", round(max(var_by_country$var) / min(var_by_country$var), 1), "\n")

# ================================================================
# Block 4: Serial correlation
# ================================================================
# This one is expected to reject, and rejecting does not mean the model is
# wrong. The EBA default rate is a rolling four-quarter figure, so consecutive
# observations share three quarters of the same underlying data by construction.
# Strong serial correlation is what that looks like. The point of testing is to
# confirm the magnitude and to justify clustering by country, which is what
# handles it.
cat("\n=== 4. Serial correlation (Wooldridge) ===\n")

pwar_base <- plm::pwartest(pfe_base)
pwar_dl   <- plm::pwartest(pfe_dl)

cat("FE baseline:\n"); print(pwar_base)
cat("\nFE + DL:\n");   print(pwar_dl)

# ================================================================
# Block 5: Cross-sectional dependence
# ================================================================
# The Pesaran CD test asks whether the residuals of different countries move
# together. Clustering by country does nothing about this: it allows for
# correlation WITHIN a country over time, not ACROSS countries at the same date.
#
# This test rejects, which has a real consequence rather than a cosmetic one.
# Common shocks that hit every country at once are exactly what a country fixed
# effect cannot absorb, and leaving them in the residuals biases the standard
# errors. It is the reason step 8 also reports Driscoll-Kraay standard errors,
# and the reason step 9 estimates a CCE-MG model, which removes the common
# factor instead of merely allowing for it.
cat("\n=== 5. Cross-sectional dependence (Pesaran CD) ===\n")

pcd_base <- plm::pcdtest(pfe_base, test = "cd")
pcd_dl   <- plm::pcdtest(pfe_dl,   test = "cd")

cat("FE baseline:\n"); print(pcd_base)
cat("\nFE + DL:\n");   print(pcd_dl)

# ================================================================
# Block 6: Residual inspection
# ================================================================
cat("\n=== 6. Residual inspection ===\n")

dat_r <- dat |>
  dplyr::mutate(
    fitted   = predict(fit_fe),
    residual = default_rate - fitted,
    abs_r    = abs(residual)
  )

p1 <- ggplot(dat_r, aes(fitted, residual)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_smooth(se = FALSE, colour = "steelblue", method = "loess", formula = y ~ x) +
  labs(title = "Residuals vs Fitted", x = "Fitted (pp)", y = "Residual (pp)")

p2 <- ggplot(dat_r, aes(quarter_date, residual)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_smooth(se = FALSE, colour = "steelblue", method = "loess", formula = y ~ x) +
  labs(title = "Residuals vs Time", x = "Quarter", y = "Residual (pp)")

p3 <- ggplot(dat_r, aes(sample = residual)) +
  stat_qq(alpha = 0.5, size = 1.5) +
  stat_qq_line(colour = "steelblue") +
  labs(title = "Normal QQ (informative, not gate)", x = "Theoretical", y = "Sample")

p_resid <- p1 | p2 | p3
ggsave(file.path(PATH_FIGURES, "diagnostics_residuals.png"),
       p_resid, width = 15, height = 5, dpi = 300)

top_outliers <- dat_r |>
  dplyr::arrange(desc(abs_r)) |>
  dplyr::slice_head(n = 10) |>
  dplyr::select(iso2, quarter, default_rate, fitted, residual)

write.csv(top_outliers, file.path(PATH_TABLES, "diagnostics_outliers.csv"), row.names = FALSE)
# The outliers are identified and written out, but NOT removed from the sample.
# Dropping the observations a model fits worst is how you make any model look
# good. Step 8 instead re-runs the baseline without specific countries and
# reports the difference, which is a claim a reader can check.
cat("Top-10 residual outliers (identified, NOT removed):\n")
print(top_outliers)

# ================================================================
# Block 7: CR2 + wild bootstrap
# ================================================================
# Blocks 1-6 diagnose; this block responds to the most serious finding.
#
# Clustered standard errors rely on having a large number of clusters. Here
# there are 26 countries, which is not large. In that regime the usual clustered
# SEs are too small and p-values are too optimistic, so a "significant" result
# might be an artefact of the correction rather than of the data.
#
# Two independent second opinions:
#   CR2 (Bell-McCaffrey)  a small-sample correction to the cluster-robust
#                         variance, with adjusted degrees of freedom
#   wild cluster bootstrap  resamples at the cluster level and gets a p-value
#                         without relying on the asymptotics at all
#
# If all three broadly agree, the inference is not an artefact of the method.
cat("\n=== 7. CR2 + wild cluster bootstrap ===\n")

# Each fit is paired with the exact dataset it was estimated on. This matters:
# clubSandwich needs a cluster vector whose length equals nrow(model.frame(fit)),
# and the lagged specifications lose rows the others keep. Passing the wrong
# dataset here would either error or, worse, silently misalign countries.
fits <- list(
  pooled = list(fit = fit_pooled, dat = dat),
  fe     = list(fit = fit_fe,     dat = dat),
  fe_l1  = list(fit = fit_fe_l1,  dat = dat_l1),
  fe_dl  = list(fit = fit_fe_dl,  dat = dat_l1)
)

cr2_tests <- purrr::imap(fits, function(x, name) {
  cat("  CR2:", name, "\n")
  clubSandwich::coef_test(x$fit, vcov = "CR2", cluster = x$dat$iso2)
})

wild_specs <- list(
  pooled = c("gdp_growth_yoy", "inflation_yoy"),
  fe     = c("gdp_growth_yoy", "inflation_yoy"),
  fe_l1  = c("gdp_l1", "inf_l1"),
  fe_dl  = c("gdp_growth_yoy", "gdp_l1", "inflation_yoy", "inf_l1")
)

if (!HAS_WILDBOOT)
  cat("  fwildclusterboot not installed — reporting CR2 and clustered SEs only,\n",
      "  the wild bootstrap column will be NA.\n", sep = "")

run_wild <- function(fit, param, B = 9999) {
  # If the package is not installed, return NULL. The p-value table below
  # already renders that as NA, so the run continues and the column is simply
  # blank. See the note in 00_setup.R for why this one is allowed to be
  # missing: it is a second opinion on the standard errors, not an input to any
  # coefficient the write-up reports.
  if (!HAS_WILDBOOT) return(NULL)
  set.seed(1); dqrng::dqset.seed(1)
  suppressWarnings(suppressMessages(
    tryCatch(
      fwildclusterboot::boottest(fit, clustid = "iso2", param = param, B = B),
      error = function(e) { message("  wild fail: ", param, " — ", conditionMessage(e)); NULL }
    )
  ))
}

wild_results <- purrr::imap(wild_specs, function(params, spec_name) {
  cat("  wild boot:", spec_name, "\n")
  purrr::set_names(purrr::map(params, ~ run_wild(fits[[spec_name]]$fit, .x)), params)
})

# ================================================================
# Consolidated output tables
# ================================================================
cat("\n=== Building output tables ===\n")

# --- Table 1: diagnostics summary ---
diagnostics_summary <- dplyr::bind_rows(
  ur_df |> dplyr::transmute(
    block = "1. Unit root",
    target = variable,
    test,
    statistic,
    p_value,
    interpretation
  ),
  tibble::tibble(
    block = c("3. Hetero", "3. Hetero"),
    target = c("FE resid vs fitted", "FE resid across countries"),
    test = c("Breusch-Pagan", "Levene (groupwise)"),
    statistic = c(as.numeric(bp$statistic), lev[1, "F value"]),
    p_value = c(bp$p.value, lev[1, "Pr(>F)"]),
    interpretation = c(
      ifelse(bp$p.value < 0.05,
             "reject → functional-form hetero",
             "no functional-form hetero"),
      ifelse(lev[1, "Pr(>F)"] < 0.05,
             "reject → groupwise hetero across countries (justifies cluster SE)",
             "no groupwise hetero")
    )
  ),
  tibble::tibble(
    block = c("4. Serial", "4. Serial"),
    target = c("FE baseline residuals", "FE + DL residuals"),
    test = c("Wooldridge AR(1)", "Wooldridge AR(1)"),
    statistic = c(as.numeric(pwar_base$statistic), as.numeric(pwar_dl$statistic)),
    p_value = c(pwar_base$p.value, pwar_dl$p.value),
    interpretation = ifelse(c(pwar_base$p.value, pwar_dl$p.value) < 0.05,
                            "reject → strong serial dependence (expected: overlapping 4Q window)",
                            "no serial correlation")
  ),
  tibble::tibble(
    block = c("5. CD", "5. CD"),
    target = c("FE baseline residuals", "FE + DL residuals"),
    test = c("Pesaran CD", "Pesaran CD"),
    statistic = c(as.numeric(pcd_base$statistic), as.numeric(pcd_dl$statistic)),
    p_value = c(pcd_base$p.value, pcd_dl$p.value),
    # A reminder about what country fixed effects actually do, because it is
    # easy to over-claim here. They absorb whatever is specific to a country and
    # constant over time. They do NOT absorb COVID, the energy shock or ECB
    # tightening: those vary over time and hit every country, and removing them
    # would take time fixed effects, not country ones.
    #
    # So if this test fails to reject, the only thing that follows is that no
    # strong contemporaneous dependence was detected in the residuals. It is not
    # evidence that common shocks have been dealt with.
    interpretation = ifelse(c(pcd_base$p.value, pcd_dl$p.value) < 0.05,
                            "reject → contemporaneous cross-sectional dependence in the residuals",
                            "no strong evidence of remaining contemporaneous cross-sectional dependence")
  )
)

diag_kbl <- diagnostics_summary |>
  dplyr::mutate(statistic = round(statistic, 3), p_value = round(p_value, 4))

writeLines(
  kableExtra::kbl(diag_kbl, format = "latex", booktabs = TRUE,
                  caption = "Panel diagnostics") |>
    kableExtra::kable_styling(latex_options = "scale_down") |>
    as.character(),
  file.path(PATH_TABLES, "diagnostics.tex")
)
writeLines(
  kableExtra::kbl(diag_kbl, format = "html", caption = "Panel diagnostics") |>
    kableExtra::kable_styling() |>
    as.character(),
  file.path(PATH_TABLES, "diagnostics.html")
)

# --- Table 2: CR1 vs CR2 vs wild p-values per (spec, param) ---
get_cr1_p <- function(fit, param) {
  tt <- summary(fit)$coeftable
  if (param %in% rownames(tt)) tt[param, 4] else NA_real_
}

comp_rows <- purrr::imap_dfr(wild_specs, function(params, spec) {
  fit <- fits[[spec]]$fit
  cr2 <- cr2_tests[[spec]]
  cr2_df <- as.data.frame(cr2)
  purrr::map_dfr(params, function(p) {
    tibble::tibble(
      spec = spec,
      param = p,
      estimate = unname(coef(fit)[p]),
      cr1_p = get_cr1_p(fit, p),
      cr2_p = cr2_df$p_Satt[match(p, cr2_df$Coef)],
      wild_p = if (!is.null(wild_results[[spec]][[p]])) wild_results[[spec]][[p]]$p_val else NA_real_
    )
  })
})

comp_rows <- comp_rows |>
  dplyr::mutate(dplyr::across(c(estimate, cr1_p, cr2_p, wild_p), ~ round(.x, 4)))

print(comp_rows)

writeLines(
  kableExtra::kbl(comp_rows, format = "latex", booktabs = TRUE,
                  caption = "Baselines under CR1 / CR2 / wild cluster bootstrap SEs") |>
    kableExtra::kable_styling() |>
    as.character(),
  file.path(PATH_TABLES, "baselines_cr2.tex")
)
writeLines(
  kableExtra::kbl(comp_rows, format = "html",
                  caption = "Baselines under CR1 / CR2 / wild cluster bootstrap SEs") |>
    kableExtra::kable_styling() |>
    as.character(),
  file.path(PATH_TABLES, "baselines_cr2.html")
)

saveRDS(
  list(unit_root = ur_df, corr_mat = cor_mat, vif = vifs,
       bp = bp, resid_var = var_by_country,
       pwar_base = pwar_base, pwar_dl = pwar_dl,
       pcd_base = pcd_base, pcd_dl = pcd_dl,
       cr2 = cr2_tests, wild = wild_results,
       summary = diagnostics_summary, comparison = comp_rows,
       outliers = top_outliers),
  file.path(PATH_MODELS, "diagnostics.rds")
)

cat("\n=== DONE ===\n")
cat("Outputs:\n")
cat(" ", file.path(PATH_TABLES, "diagnostics.{tex,html}"), "\n")
cat(" ", file.path(PATH_TABLES, "baselines_cr2.{tex,html}"), "\n")
cat(" ", file.path(PATH_TABLES, "diagnostics_outliers.csv"), "\n")
cat(" ", file.path(PATH_FIGURES, "diagnostics_residuals.png"), "\n")
cat(" ", file.path(PATH_MODELS, "diagnostics.rds"), "\n")
