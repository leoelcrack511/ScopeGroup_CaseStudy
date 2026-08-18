# R/04_load_data.R
#
# STEP 4: Turn the panel into the dataset the models actually use.
#
# This is the first step of the analysis layer, and the last one that touches
# the data. Everything downstream reads the .rds this writes, so if a variable
# does not exist here, no model can use it.
#
# What gets added:
#   - quarter parsed into a real date plus an integer ordering
#   - recovery = 100 - LGD
#   - a COVID dummy
#   - the coverage flags that decide which countries are in the sample
#   - macro lags (1 to 4 quarters) and 4-quarter moving averages
#
# Two decisions here shape every result that follows: which countries are in the
# estimation sample, and how the lags are built. Both are explained where they
# happen rather than here.
#
# Input:  data/clean/panel_country_quarter.csv, data/clean/macro_country_quarter.csv
# Output: output/models/panel_model_ready.rds

source(here::here("R", "00_setup.R"))

panel_raw <- readr::read_csv(PATH_PANEL, show_col_types = FALSE)

# ---- Quarter parsing --------------------------------------------------------
# "2020-Q1" becomes a Date (the first day of the quarter) plus an integer
# position 1..T. The date is for plotting and comparisons, the integer for
# anything that needs a simple ordering.
parse_quarter <- function(q) {
  parts <- stringr::str_split_fixed(q, "-Q", 2)
  year  <- as.integer(parts[, 1])
  qnum  <- as.integer(parts[, 2])
  lubridate::make_date(year, (qnum - 1L) * 3L + 1L, 1L)
}

panel <- panel_raw |>
  mutate(quarter_date = parse_quarter(quarter)) |>
  arrange(quarter_date) |>
  mutate(quarter_order = dense_rank(quarter_date))

# ---- Recovery ---------------------------------------------------------------
# The EBA does not publish a recovery rate, so this is a proxy: whatever is not
# lost on a defaulted exposure is treated as recovered. Step 8 also tries the
# alternative proxy, 100 - loss rate, to check the choice does not drive
# anything.
panel <- panel |>
  mutate(recovery = 100 - lgd)

# ---- COVID dummy: 2020Q1 to 2021Q4 -----------------------------------------
# Note that BOTH bounds are written out. That looks redundant but it is not.
# When the sample started in 2020Q1, an upper bound on its own was enough. The
# sample now starts in 2015Q4, and a one-sided test would flag every quarter
# from 2015 to 2019 as a COVID quarter. That bug would not throw an error, it
# would just quietly change the results.
panel <- panel |>
  mutate(covid = as.integer(quarter_date >= as.Date("2020-01-01") &
                            quarter_date <= as.Date("2021-10-01")))

# ---- Coverage flag per country ----------------------------------------------
# A quarter counts as valid for a country only if all three variables are there:
# default rate, GDP growth and inflation. A country passes if at least 60% of
# its quarters are valid.
#
# The threshold is set in advance and applied to everyone. It is not tuned to
# the results.
#
# Worth knowing, because it is the kind of bug that hides for a long time: an
# earlier version also let a country pass on "n_valid >= 20". That clause was
# harmless when the panel was 25 quarters long, because 20 of 25 is 80%
# coverage, so the 60% rule always bound first. Extending the panel to 42
# quarters turned it into a 47.6% back door that would have readmitted exactly
# the two countries the filter exists to remove. The clause is gone; the
# threshold is the whole rule.
coverage <- panel |>
  group_by(iso2) |>
  summarise(
    n_total         = n(),
    n_valid         = sum(!is.na(default_rate) &
                          !is.na(gdp_growth_yoy) &
                          !is.na(inflation_yoy)),
    coverage_pct    = n_valid / n_total,
    passes_coverage = coverage_pct >= 0.60,
    .groups = "drop"
  )

panel <- panel |>
  left_join(coverage |> select(iso2, coverage_pct, passes_coverage), by = "iso2")

# ---- Main sample: coverage, minus Cyprus ------------------------------------
# Cyprus passes the coverage test but is excluded anyway, on data quality.
#
# The reason: CY reports five consecutive quarters (2023Q3-2024Q3) at roughly
# 1e-5 pp, effectively zero, after a reading of 17 pp in 2020Q3. A run of
# near-zeros like that gives a handful of observations enormous influence over
# any regression, and it does not fit the rest of the country's own series.
#
# What is NOT claimed: any specific explanation of why the EBA published those
# numbers. That would require documentation this project does not have. The
# exclusion is a data-quality judgement, stated as such.
#
# Because it is a judgement, it is testable: step 8 puts Cyprus back in via
# passes_coverage and reports what changes.
panel <- panel |>
  mutate(is_main_sample = passes_coverage & iso2 != "CY")

# ---- Macro lags (L1..L4) + 4-quarter moving averages ------------------------
# Since the EBA default rate is built over an overlapping four-quarter window,
# we also consider a four-quarter average of annual GDP growth, as a measure of
# persistent macroeconomic conditions.
#
# Be careful about what that is and is not. It is NOT a matching of windows.
# GDP growth here is already year-on-year, so it already compares against four
# quarters ago; averaging it over four more quarters gives a heavily smoothed
# series, not "the theoretically correct regressor". It is one more way of
# describing the macro environment, and it is treated as such.
#
# It is used in step 9, on the heterogeneous-slope specifications. It does not
# replace the contemporaneous baselines in steps 6 and 8.
#
# Where these are built from matters as much as how. They come from
# data/clean/macro_country_quarter.csv, NOT from the panel. That file has the
# same GDP and HICP series over a window starting well before the panel does, so
# the lags and averages at 2015Q4 are computed from observed 2014Q4-2015Q3 data.
#
# Lagging inside the panel instead would have left the first three or four
# quarters of every country as NA. Nothing would have failed. The models would
# just have dropped those rows, and the estimation sample would have started a
# year later than the data does, without anyone being told.
#
# One requirement makes this valid: the macro file has to be a complete
# quarterly grid per country, with no missing rows, because lagging works by row
# position within a country. Step 3 asserts exactly that.
ma4 <- function(x) (x + dplyr::lag(x, 1) + dplyr::lag(x, 2) + dplyr::lag(x, 3)) / 4

macro_lags <- readr::read_csv(PATH_MACRO, show_col_types = FALSE) |>
  arrange(iso2, quarter) |>
  group_by(iso2) |>
  mutate(
    gdp_l1 = dplyr::lag(gdp_growth_yoy, 1),
    gdp_l2 = dplyr::lag(gdp_growth_yoy, 2),
    gdp_l3 = dplyr::lag(gdp_growth_yoy, 3),
    gdp_l4 = dplyr::lag(gdp_growth_yoy, 4),
    inf_l1 = dplyr::lag(inflation_yoy,  1),
    inf_l2 = dplyr::lag(inflation_yoy,  2),
    gdp_ma4 = ma4(gdp_growth_yoy),
    inf_ma4 = ma4(inflation_yoy)
  ) |>
  ungroup() |>
  select(iso2, quarter, gdp_l1, gdp_l2, gdp_l3, gdp_l4, inf_l1, inf_l2,
         gdp_ma4, inf_ma4)

n_before <- nrow(panel)
panel <- panel |>
  left_join(macro_lags, by = c("iso2", "quarter")) |>
  arrange(iso2, quarter_order)
stopifnot(nrow(panel) == n_before)   # the join must not duplicate rows

# ---- Sanity checks ---------------------------------------------------------
message("Rows:           ", nrow(panel))
message("Countries:      ", dplyr::n_distinct(panel$iso2))
message("Quarters:       ", dplyr::n_distinct(panel$quarter))
message("Pass coverage:  ", sum(coverage$passes_coverage), " / ", nrow(coverage))
message("Main sample:    ", dplyr::n_distinct(panel$iso2[panel$is_main_sample]),
        " countries (passes_coverage & != CY)")
message("COVID rows:     ", sum(panel$covid), " / ", nrow(panel))

# Since the lags are built off the longer macro window, not one of them should
# be NA anywhere in the panel, not even in a country's very first quarter.
# This asserts that rather than hoping for it. A single NA here would mean the
# macro window does not reach far enough back, and the estimation sample would
# silently start later than it appears to.
lag_cols <- c("gdp_l1", "gdp_l2", "gdp_l3", "gdp_l4", "inf_l1", "inf_l2",
              "gdp_ma4", "inf_ma4")
na_lags <- colSums(is.na(panel[, lag_cols]))
message("Macro lag NAs:  ", paste(sprintf("%s=%d", names(na_lags), na_lags), collapse = " "))
stopifnot(all(na_lags == 0L))

# Check gdp_ma4 against a mean worked out by hand. It is computed from the macro
# file directly, so it does not depend on the join above being right. The point
# is to catch an error in that join, not to restate it.
#
# The test picks the hardest case on purpose: at a country's FIRST panel
# quarter, the 4-quarter average has to cover that quarter and the three before
# it, all three of which lie outside the panel entirely. If the lags had been
# built on the panel rather than the macro grid, this is exactly where it would
# break.
macro_raw <- readr::read_csv(PATH_MACRO, show_col_types = FALSE) |> arrange(iso2, quarter)
first_q <- min(panel$quarter)
i <- which(macro_raw$iso2 == "DE" & macro_raw$quarter == first_q)
chk_manual <- mean(macro_raw$gdp_growth_yoy[(i - 3L):i])
chk_value  <- panel$gdp_ma4[panel$iso2 == "DE" & panel$quarter == first_q]
stopifnot(isTRUE(all.equal(chk_value, chk_manual)))
message("gdp_ma4 check:  DE ", first_q, " = ", round(chk_value, 4),
        " = mean(", paste(macro_raw$quarter[(i - 3L):i], collapse = ", "), ") OK")

# Countries by coverage (sanity check)
coverage |>
  arrange(desc(coverage_pct)) |>
  print(n = Inf)

# ---- Save ------------------------------------------------------------------
dir.create(PATH_MODELS, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(PATH_MODELS, "panel_model_ready.rds")
saveRDS(panel, out_path)
message("Saved: ", out_path)
