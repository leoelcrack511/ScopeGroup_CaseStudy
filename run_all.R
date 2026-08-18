# =============================================================================
# run_all.R: runs the whole project from start to finish
#
# Corporate Default Rates and Macroeconomic Conditions
# A satellite model for stress testing (EBA Credit Risk Parameters x Eurostat)
#
# This is the only file you need to run. From the project root:
#
#     Rscript run_all.R
#
# It runs the eleven steps in order and ends with a reproducibility check: it
# re-reads the headline estimates from the objects the run just produced and
# compares them to the values the write-up reports. Takes about 1-3 minutes,
# plus another minute if you set REBUILD_DATA to TRUE.
#
# WHAT EACH STEP DOES
#
#   Data layer (steps 1-3). Turns the published source files into one table.
#
#     Step 1  R/01_download_data.R
#       Downloads the EBA Credit Risk Parameters annex: 35 quarterly
#       spreadsheets, one bundled workbook that covers 2015Q4-2016Q4, and the
#       two 2017 PDFs. Also the three Eurostat series (real GDP growth,
#       monthly HICP, and a faster-published HICP series for recent months).
#       Files that are already on disk are skipped, so running this against
#       the data/raw/ snapshot that ships with the project does nothing.
#
#     Step 2  R/02_parse_eba.R
#       Reads every EBA file and pulls out three numbers per country-quarter
#       for the Corporates asset class: default rate, loss rate and LGD. The
#       EBA has changed its sheet layout three times since 2015, so there are
#       three readers. There is also a PDF reader for 2017Q1 and 2017Q2, the
#       two quarters that were never published as a spreadsheet. 2017Q4 came
#       out in both formats, so the script uses it to check that the PDF
#       reader and the spreadsheet reader agree.
#
#     Step 3  R/03_build_panel.R
#       Matches EBA country names to ISO2 codes, converts decimals to
#       percentage points, averages monthly inflation up to quarters (only
#       where all three months are present), and merges everything into
#       data/clean/panel_country_quarter.csv. That is 29 countries x 42 quarters,
#       2015Q4-2026Q1, one row per country-quarter.
#
#     These three steps do NOT run by default. The panel in data/clean/ is the
#     exact file the results came from, so the default is to use it as-is. Set
#     REBUILD_DATA <- TRUE below to rebuild it from data/raw/. That still does
#     not hit the network, because step 1 skips files it already has. Read the
#     README section on data vintages before you delete anything.
#
#   Analysis layer (steps 4-11). Always runs.
#
#     Step 4  R/04_load_data.R
#       Adds the columns the models need: recovery (100 - LGD), a COVID
#       dummy, the coverage flags that define the estimation sample of 26
#       countries, and the macro lags and 4-quarter averages.
#
#     Step 5  R/05_eda.R
#       Descriptive statistics, time-series and scatter plots, correlations,
#       and a look at what is missing and where.
#
#     Step 6  R/06_baselines.R
#       The four baseline regressions: pooled OLS, country fixed effects, FE
#       with lagged macro, and FE with a distributed lag. Standard errors are
#       clustered by country throughout.
#
#     Step 7  R/07_diagnostics.R
#       Do the baselines hold up? Unit root tests (IPS, Fisher-ADF, CIPS),
#       multicollinearity (VIF), heteroskedasticity, serial correlation
#       (Wooldridge), cross-sectional dependence (Pesaran CD), a look at the
#       residuals, and two corrections for having only 26 clusters (CR2
#       standard errors and a wild cluster bootstrap).
#
#     Step 8  R/08_robustness.R
#       Twelve ways of asking whether the baseline result survives: COVID
#       controls, time fixed effects, a common linear trend, Driscoll-Kraay
#       standard errors, a logit transformation of the default rate, dropping
#       outlier countries, both recovery proxies, a 4-quarter-average
#       specification, the Q4-only annual subsample, first differences, and a
#       pre/post-2020 split.
#
#     Step 9  R/09_slope_heterogeneity.R
#       The core of the analysis. Estimates a separate GDP slope for every
#       country, measures how much those slopes really differ (Cochran Q, I2,
#       tau), tests the country residuals for cross-sectional dependence, and
#       runs the three estimators that allow slopes to differ: Mean Group,
#       Swamy RCM and CCE-MG.
#
#     Step 10 R/10_stress_scenario.R
#       A stylised illustration: GDP growth drops 5 pp for a year. What does
#       each estimator, and each country slope, imply for the default rate?
#
#     Step 11 R/11_key_figures.R
#       Builds the exhibits for the write-up. This step only formats numbers
#       that steps 4-10 already produced; it never re-estimates anything.
#
#   Everything lands in output/ (tables/, figures/, models/), which this script
#   creates. Nothing is shipped pre-built: the tables and figures in the
#   write-up are exactly what the run below regenerates.
# =============================================================================

REBUILD_DATA <- FALSE   # TRUE = rebuild the panel from data/raw/ first

t_start <- Sys.time()

# ---- Dependency check -------------------------------------------------------
# Done up front, so a missing package stops the run here with a message telling
# you what to install, instead of failing three minutes in during step 7.

required <- c(
  # data handling and IO
  "here", "tidyverse", "readxl", "jsonlite", "janitor", "skimr", "scales",
  # panel econometrics
  "fixest",           # primary FE estimator: fast, clustered SEs
  "plm",              # panel tests fixest does not provide
  "sandwich", "lmtest", "car",
  "clubSandwich",     # CR2 (Bell-McCaffrey) SEs for few clusters
  # tidiers, tables and plots
  "broom", "performance", "modelsummary", "kableExtra", "patchwork"
)

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  cat("Missing R packages:\n  ", paste(missing, collapse = ", "), "\n\n", sep = "")
  cat("Install them with:\n\n")
  cat("  Rscript R/install_packages.R\n\n")
  stop("Install the packages above, then re-run.")
}

# fwildclusterboot is deliberately left out of the list above. It is used in
# exactly one place: the wild cluster bootstrap in step 7. That is a robustness
# column, a second opinion on the standard errors given that there are only 26
# clusters. No reported coefficient depends on it.
#
# It is optional because it needs a Fortran compiler to build, which is the one
# thing here that can fail for reasons that have nothing to do with the
# analysis. Requiring it would let a toolchain problem block the whole run.
# Step 7 notices when it is missing and falls back to CR2 and clustered SEs.
if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
  cat("Note: fwildclusterboot is not installed. The pipeline runs; step 7 omits\n")
  cat("the wild bootstrap p-value column. Everything else is unaffected.\n")
}

step <- function(n, title, script) {
  cat("\n", strrep("=", 77), "\n", sep = "")
  cat(sprintf("STEP %d/11 — %s\n", n, title))
  cat(sprintf("  script: %s\n", script))
  cat(strrep("=", 77), "\n", sep = "")
  status <- system2("Rscript", script, stdout = "", stderr = "")
  if (status != 0) stop(sprintf("Step %d failed (%s). See output above.", n, script))
}

# ---- Data layer (optional) --------------------------------------------------
if (REBUILD_DATA) {
  step(1, "Download the raw EBA and Eurostat files", "R/01_download_data.R")
  step(2, "Parse the EBA annex files",               "R/02_parse_eba.R")
  step(3, "Build the country-quarter panel",         "R/03_build_panel.R")
  # The panel splices together three EBA publication formats. This checks that
  # the default rate does not jump at the two joins, i.e. that the result is one
  # continuous series rather than three stitched together. It reads the panel
  # and writes its own outputs; nothing downstream uses them, which is why it
  # sits with the data layer instead of getting its own numbered step.
  step(3, "Check the publication seams",             "R/03b_compat_check.R")
} else {
  cat("Steps 1-3 (data layer) skipped: using the frozen panel at\n")
  cat("  data/clean/panel_country_quarter.csv\n")
  cat("Set REBUILD_DATA <- TRUE at the top of this file to rebuild it.\n")
  if (!file.exists("data/clean/panel_country_quarter.csv")) {
    stop("The panel is missing. Set REBUILD_DATA <- TRUE to build it from data/raw/.")
  }
}

# ---- Analysis layer ---------------------------------------------------------
step(4,  "Load panel and build derived columns",             "R/04_load_data.R")
step(5,  "Exploratory data analysis",                        "R/05_eda.R")
step(6,  "Baseline regressions",                             "R/06_baselines.R")
step(7,  "Diagnostics and small-sample cluster corrections", "R/07_diagnostics.R")
step(8,  "Robustness checks and recovery proxies",           "R/08_robustness.R")
step(9,  "Slope heterogeneity: MG, Swamy and CCE-MG",        "R/09_slope_heterogeneity.R")
step(10, "Stress-scenario illustration",                     "R/10_stress_scenario.R")
step(11, "Report exhibits",                                  "R/11_key_figures.R")

# ---- Reproducibility check --------------------------------------------------
# The nine numbers the write-up leads with, re-read from the .rds files this run
# just wrote and compared against the published values. Nothing upstream is
# random (seeds are fixed wherever sampling happens), so on this data these have
# to match to numerical precision. If one of them moves, something changed.
cat("\n", strrep("=", 77), "\n", sep = "")
cat("REPRODUCIBILITY CHECK — headline estimates vs published values\n")
cat(strrep("=", 77), "\n", sep = "")

sh   <- readRDS("output/models/slope_het.rds")
st   <- readRDS("output/models/stress_scenario.rds")
base <- readRDS("output/models/baselines.rds")

# Rows are looked up by estimator name, not by position. The comparison table
# gained a fourth row (CCE-MG) partway through the project. With positional
# indices, this check would have gone on passing while quietly reading the wrong
# row. That is the worst kind of failure, because it looks like success.
beta_of <- function(name) sh$comparison$beta[match(name, sh$comparison$estimator)]
p_of    <- function(name) sh$comparison$p[match(name, sh$comparison$estimator)]

got <- c(
  `FE baseline beta_GDP (contemporaneous)` = unname(coef(base$fe)["gdp_growth_yoy"]),
  `FE (MA4 spec) beta_GDP`                 = beta_of("Country FE (MA4)"),
  `Mean Group beta_GDP`                    = beta_of("Mean Group"),
  `Swamy RCM beta_GDP`                     = beta_of("Swamy RCM"),
  `CCE-MG beta_GDP`                        = beta_of("CCE-MG"),
  `Mean Group p-value`                     = p_of("Mean Group"),
  `CCE-MG p-value`                         = p_of("CCE-MG"),
  `Stress: Mean Group delta DR (pp)`       = st$scenario$delta_dr[st$scenario$estimator == "Mean Group"],
  `Sample mean default rate (pp)`          = st$dr_mean
)

# The published values for the 2015Q4-2026Q1 sample (26 countries, N = 1023).
# These are hard-coded on purpose: they are what the write-up says, so a change
# anywhere in the pipeline that moves one of them shows up here rather than
# being noticed later by a reader comparing the prose to a table.
expected <- c(-0.0014, 0.0005, -0.0036, 0.0085, -0.1175, 0.8497, 0.0949, 0.0179, 1.4379)

check <- data.frame(
  quantity   = names(got),
  expected   = expected,
  reproduced = round(unname(got), 4),
  match      = abs(unname(got) - expected) < 1e-3
)
print(check, row.names = FALSE)

if (all(check$match)) {
  cat("\nALL CHECKS PASSED — the pipeline reproduces the published results.\n")
} else {
  cat("\nWARNING: some values differ from the published results.\n")
  cat("If REBUILD_DATA = TRUE was used, re-downloaded Eurostat data may be a\n")
  cat("newer vintage (see README, 'Data vintages').\n")
}

cat(sprintf("\nTotal runtime: %.1f minutes\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
