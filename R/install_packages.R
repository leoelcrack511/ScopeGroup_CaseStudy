# R/install_packages.R
#
# Installs the packages the project needs. Run this once, before run_all.R:
#
#   Rscript R/install_packages.R
#
# Packages already present are skipped, so it is safe to run again.
#
# Everything in the list below is on CRAN and installs without any system
# libraries. There is one optional package that does not, and it is handled
# separately at the bottom of this file.

repo <- "https://cloud.r-project.org"

pkgs <- c(
  # Data & utilities
  "here", "tidyverse", "readxl", "jsonlite", "janitor", "skimr", "scales",

  # Panel econometrics
  "fixest",           # primary FE estimator: fast, clustered / multi-way SE
  "plm",              # panel tests fixest has no equivalent for: purtest,
                      # cipstest, pcdtest, pwartest, pcce, vcovSCC (Driscoll-Kraay)
  "sandwich",         # additional robust vcov
  "lmtest",           # coeftest, waldtest
  "car",              # VIF, linearHypothesis
  "clubSandwich",     # CR2 (Bell-McCaffrey) SE for the small number of clusters

  # Tidiers & diagnostics
  "broom", "performance",

  # Tables & plots
  "modelsummary", "kableExtra", "patchwork"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = repo)
} else {
  cat("All required packages already installed.\n")
}

# ---- Optional: fwildclusterboot --------------------------------------------
# Used in exactly one place, the wild cluster bootstrap in step 7, which is a
# robustness column on the small number of clusters (G = 26) and not an input to
# any reported coefficient. It is optional because it needs a Fortran compiler
# to build, which is the one thing in this project that can fail for reasons
# that have nothing to do with the analysis. Step 7 detects its absence and
# reports CR2 and clustered standard errors only.
if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
  cat("\nOptional: fwildclusterboot is not installed.\n",
      "The pipeline runs without it; step 7 then omits the wild bootstrap\n",
      "p-value column. To add it: install.packages('fwildclusterboot')\n", sep = "")
}

cat("\nDone. Next: Rscript run_all.R\n")
