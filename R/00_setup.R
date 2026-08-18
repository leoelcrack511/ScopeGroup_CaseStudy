# Shared setup. Every other script starts with:
#
#   source(here::here("R", "00_setup.R"))
#
# It does three things: loads the packages, defines the paths the scripts read
# from and write to, and fixes the plot theme and number formatting so that
# output looks the same no matter which script produced it.
#
# Nothing here computes a result.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(readxl)
  library(jsonlite)
  library(janitor)
  library(skimr)
  library(scales)
  library(fixest)
  library(plm)
  library(sandwich)
  library(lmtest)
  library(car)
  library(clubSandwich)
  library(broom)
  library(performance)
  library(modelsummary)
  library(kableExtra)
  library(patchwork)
})

# fwildclusterboot is loaded on its own, and only if it is there.
#
# It is the one fragile dependency in the project: it needs a Fortran compiler
# to build, so it is the package most likely to be missing on someone else's
# machine. It is used in exactly one place, the wild cluster bootstrap in step
# 7, which is a robustness check on having only 26 clusters. No reported
# coefficient depends on it.
#
# Handling it this way means a machine without it still runs the whole pipeline
# and loses one column of one diagnostics table, instead of dying on the very
# first source() call.
#
# No message here on purpose: all eleven scripts source this file, so a warning
# would print eleven times per run. Step 7 is the only script that cares, and it
# says so once.
HAS_WILDBOOT <- requireNamespace("fwildclusterboot", quietly = TRUE)
if (HAS_WILDBOOT) suppressPackageStartupMessages(library(fwildclusterboot))

# Paths. Defined once here so no script ever builds a path by hand.
#
#   data/raw    the source files exactly as published. Read by steps 1-3,
#               never written to after the download.
#   data/clean  what steps 1-3 produce. PATH_PANEL is the single file the
#               whole analysis layer (steps 4-11) reads.
#   output/     everything the analysis produces: tables, figures, and the
#               fitted model objects.
#
# The loop below creates any of these that are missing, so the project runs on a
# fresh copy without needing empty folders to be shipped alongside it.
PATH_RAW_EBA      <- here::here("data", "raw", "eba")
PATH_RAW_EUROSTAT <- here::here("data", "raw", "eurostat")
PATH_CLEAN        <- here::here("data", "clean")
PATH_PANEL        <- file.path(PATH_CLEAN, "panel_country_quarter.csv")
# The macro series on their own, starting well before the panel does. This
# matters: the panel begins in 2015Q4, and a 4-quarter lag at that date needs
# GDP growth for 2014Q4. Building the lags off this longer file instead of off
# the panel is what keeps the first year of every country from coming out NA.
# Written by step 3, read by step 4.
PATH_MACRO        <- file.path(PATH_CLEAN, "macro_country_quarter.csv")
PATH_TABLES       <- here::here("output", "tables")
PATH_FIGURES      <- here::here("output", "figures")
PATH_MODELS       <- here::here("output", "models")

for (p in c(PATH_CLEAN, PATH_TABLES, PATH_FIGURES, PATH_MODELS)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}

# ggplot theme
theme_set(theme_minimal(base_size = 11))

# Display options
options(
  scipen  = 999,
  digits  = 4,
  modelsummary_stars_note = FALSE
)
