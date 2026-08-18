# R/06_baselines.R
#
# STEP 6: The baseline regressions.
#
# Four specifications, all on the 26-country estimation sample, all with
# standard errors clustered by country:
#
#   1. pooled OLS            no country effects at all
#   2. country fixed effects the workhorse: within-country variation only
#   3. FE + lagged macro     does the response show up with a delay?
#   4. FE + distributed lag  several lags at once
#
# None of them include a COVID dummy. That is on purpose, and it is not an
# oversight: 2020-21 is real variation in exactly the variable of interest, and
# absorbing it with a dummy would remove the most informative episode in the
# sample. Step 8 adds the dummy as a robustness check and reports what it does.
#
# These four are the reference point for everything after. When step 9 finds
# that a single pooled slope hides very different country slopes, this is the
# pooled slope it is talking about.
#
# Input:   output/models/panel_model_ready.rds
# Outputs: output/models/baselines.rds
#          output/tables/baselines.{tex,html}

source(here::here("R", "00_setup.R"))

panel <- readRDS(file.path(PATH_MODELS, "panel_model_ready.rds"))

# ---- Main sample -----------------------------------------------------------
# 26 countries: the coverage filter removes MT and LV, and CY is excluded on
# data quality (see step 4).
#
# Rows with a missing default rate are dropped here, once, rather than being
# left to each model to drop on its own. That way all four specifications are
# fitted on the same rows and their coefficients are directly comparable.
dat <- panel |>
  filter(is_main_sample, !is.na(default_rate))

message("Modelling sample: ", nrow(dat), " obs, ",
        dplyr::n_distinct(dat$iso2), " countries, ",
        dplyr::n_distinct(dat$quarter), " quarters")

# ---- Specifications --------------------------------------------------------

fit_pool <- feols(
  default_rate ~ gdp_growth_yoy + inflation_yoy,
  data = dat, vcov = ~ iso2
)

fit_fe <- feols(
  default_rate ~ gdp_growth_yoy + inflation_yoy | iso2,
  data = dat, vcov = ~ iso2
)

fit_fe_l1 <- feols(
  default_rate ~ gdp_l1 + inf_l1 | iso2,
  data = dat, vcov = ~ iso2
)

fit_fe_dl <- feols(
  default_rate ~ gdp_growth_yoy + gdp_l1 + inflation_yoy + inf_l1 | iso2,
  data = dat, vcov = ~ iso2
)

# ---- Save fit objects ------------------------------------------------------

baselines <- list(
  pooled = fit_pool,
  fe     = fit_fe,
  fe_l1  = fit_fe_l1,
  fe_dl  = fit_fe_dl
)
saveRDS(baselines, file.path(PATH_MODELS, "baselines.rds"))

# ---- Comparison table (modelsummary) ---------------------------------------

coef_map <- c(
  "gdp_growth_yoy" = "GDP YoY",
  "gdp_l1"         = "GDP YoY (L1)",
  "inflation_yoy"  = "HICP YoY",
  "inf_l1"         = "HICP YoY (L1)",
  "(Intercept)"    = "Constant"
)

models <- list(
  "(1) Pooled OLS"       = fit_pool,
  "(2) Country FE"       = fit_fe,
  "(3) FE + L1"          = fit_fe_l1,
  "(4) FE + distributed" = fit_fe_dl
)

# The country count and the names of the excluded countries are read off the
# data rather than typed in. This note ends up in the write-up, and a
# hand-written "26 countries" fails the same way a hand-written coefficient
# does: it stays put while the thing it describes moves.
excluded_cov <- panel |>
  dplyr::filter(!passes_coverage) |> dplyr::distinct(iso2) |> dplyr::pull(iso2) |> sort()

notes_vec <- c(
  "Dependent variable: default_rate (pp).",
  sprintf(paste0("Sample: is_main_sample (%d countries, N=%d). Excludes CY (D8) and ",
                 "coverage-failing %s."),
          dplyr::n_distinct(dat$iso2), nrow(dat), paste(excluded_cov, collapse = ", ")),
  "SE clustered by country. No COVID dummy (D7); tested in 08_robustness.R.",
  "L1 = lagged one quarter within country."
)

modelsummary(
  models,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  fmt      = 3,
  notes    = notes_vec,
  output   = file.path(PATH_TABLES, "baselines.tex")
)

modelsummary(
  models,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE|F|Std.Errors",
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  fmt      = 3,
  notes    = notes_vec,
  output   = file.path(PATH_TABLES, "baselines.html")
)

# ---- Console summary -------------------------------------------------------

etable(
  fit_pool, fit_fe, fit_fe_l1, fit_fe_dl,
  headers = c("Pooled OLS", "FE", "FE+L1", "FE+DL"),
  digits  = 3,
  fitstat = c("n", "r2", "wr2")
)

message("\nSaved: ",
        file.path(PATH_MODELS, "baselines.rds"), "\n       ",
        file.path(PATH_TABLES, "baselines.{tex,html}"))
