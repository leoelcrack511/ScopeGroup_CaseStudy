# R/03_build_panel.R
#
# STEP 3: Build the country x quarter panel.
#
# This is where the EBA credit-risk data and the Eurostat macro series are
# joined into one rectangular table. That table is the only thing the analysis
# layer ever reads.
#
# What happens, in order:
#
#   1. Read the parsed EBA Corporates series and map the EBA's country names to
#      ISO2 codes. Countries that Eurostat does not cover, the UK and ten
#      non-European reporters, are dropped at this point. The case is defined
#      on EBA x Eurostat, and there is simply no macro data for them.
#
#   2. Convert the EBA values from decimals to percentage points, so every
#      number in the panel is on one scale. 1.25 means 1.25%, not 125%.
#
#   3. Flatten the Eurostat JSON-stat files into long format. GDP is already
#      quarterly. HICP is monthly, so it is averaged up to quarters, but only
#      for quarters where all three months are present, so that a quarter with
#      one month of data cannot masquerade as a full one.
#
#   4. Top up the main HICP series with the faster-published short-term one,
#      for recent months only. This is what lets the panel reach the current
#      quarter instead of stopping wherever the main series was last released.
#
#   5. Merge on (iso2, quarter), then check the result: no country-quarter may
#      appear twice, and coverage is reported per country so you can see who is
#      thin before any model is fitted.
#
# Missing default rates stay missing. The EBA publishes a country-quarter only
# when enough IRB banks report it, so a gap means "not reported". It does not
# mean zero, and it is never imputed.
#
# Input:  data/clean/eba_corporates_wa.csv
#         data/raw/eurostat/*.json
# Output: data/clean/panel_country_quarter.csv  (29 countries x 42 quarters,
#         2015Q4-2026Q1)

source(here::here("R", "00_setup.R"))

# ---- EBA country name -> ISO2 ----------------------------------------------
# Note EL, not GR, for Greece. That is Eurostat's convention, and using GR here
# would silently fail to match. The entries mapped to NA are countries the EBA
# reports on but Eurostat does not cover; they get dropped a few lines below.

EBA_TO_ISO <- c(
  "Austria" = "AT", "Belgium" = "BE", "Bulgaria" = "BG", "Croatia" = "HR",
  "Cyprus" = "CY", "Czech" = "CZ", "Denmark" = "DK", "Estonia" = "EE",
  "Finland" = "FI", "France" = "FR", "Germany" = "DE", "Greece" = "EL",
  "Hungary" = "HU", "Ireland" = "IE", "Italy" = "IT", "Latvia" = "LV",
  "Lithuania" = "LT", "Luxembourg" = "LU", "Malta" = "MT", "Netherlands" = "NL",
  "Norway" = "NO", "Poland" = "PL", "Portugal" = "PT", "Romania" = "RO",
  "Slovakia" = "SK", "Slovenia" = "SI", "Spain" = "ES", "Sweden" = "SE",
  "Switzerland" = "CH",
  "United Kingdom" = NA, "Australia" = NA, "Canada" = NA, "China" = NA,
  "Hong Kong" = NA, "India" = NA, "Korea, Republic Of" = NA,
  "Russian Federation" = NA, "Singapore" = NA, "United States" = NA
)

# ---- JSON-stat reader ------------------------------------------------------
# Eurostat returns JSON-stat, which is not a table. It is a flat sparse map from
# one integer index to one value, plus the sizes of each dimension. To get back
# to (country, period, value) you have to decode that index into coordinates
# yourself.
#
# The decoding is row-major: the stride of a dimension is the product of the
# sizes of all the dimensions to its right. Get that backwards and the data
# still loads, it is just attached to the wrong countries, which is why the
# checks further down matter.

jsonstat_to_long <- function(path, value_col) {
  d <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  dims  <- unlist(d$id)
  sizes <- unlist(d$size)

  # Position -> code, for each dimension.
  dim_codes <- lapply(dims, function(nm) {
    idx <- d$dimension[[nm]]$category$index
    if (!is.null(names(idx))) {
      codes <- character(length(idx))
      codes[unlist(idx) + 1L] <- names(idx)   # index is a code -> position map
      codes
    } else {
      unlist(idx)                              # already a position -> code list
    }
  })

  strides <- rep(1L, length(sizes))
  if (length(sizes) > 1L) {
    for (i in (length(sizes) - 1L):1L) strides[i] <- strides[i + 1L] * sizes[i + 1L]
  }

  vals <- d$value
  flat <- as.integer(names(vals))
  value <- vapply(vals, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))

  coord <- function(i) (flat %/% strides[i]) %% sizes[i]
  geo_i  <- match("geo",  dims)
  time_i <- match("time", dims)

  out <- dplyr::tibble(
    geo  = dim_codes[[geo_i]][coord(geo_i) + 1L],
    time = dim_codes[[time_i]][coord(time_i) + 1L],
    value = unname(value)
  )
  names(out)[3] <- value_col
  out
}

build_gdp <- function() {
  jsonstat_to_long(file.path(PATH_RAW_EUROSTAT, "namq_10_gdp_yoy.json"), "gdp_growth_yoy") |>
    dplyr::rename(iso2 = geo, quarter = time)      # time already reads "YYYY-Qn"
}

build_hicp <- function() {
  main <- jsonstat_to_long(file.path(PATH_RAW_EUROSTAT, "prc_hicp_manr.json"), "inflation_yoy") |>
    dplyr::rename(iso2 = geo, month = time)

  recent <- jsonstat_to_long(file.path(PATH_RAW_EUROSTAT, "ei_cphi_m_recent.json"), "inflation_yoy") |>
    dplyr::rename(iso2 = geo, month = time)

  # The short-term series is used only for months the main series has not
  # published yet. Where both exist, the main one wins. This way the two can
  # never disagree about the same month, and the choice of which to believe is
  # made here, once, rather than implicitly by join order.
  max_month <- max(main$month)
  recent <- recent |> dplyr::filter(month > max_month)
  if (nrow(recent)) {
    cat(sprintf("HICP complement from ei_cphi_m: +%d rows, months %s\n",
                nrow(recent), paste(sort(unique(recent$month)), collapse = ", ")))
  }

  dplyr::bind_rows(main, recent) |>
    dplyr::mutate(
      year    = as.integer(substr(month, 1, 4)),
      m       = as.integer(substr(month, 6, 7)),
      quarter = paste0(year, "-Q", (m - 1L) %/% 3L + 1L)
    ) |>
    dplyr::group_by(iso2, quarter) |>
    dplyr::filter(dplyr::n() == 3L) |>       # drop partial quarters
    dplyr::summarise(inflation_yoy = mean(inflation_yoy), .groups = "drop")
}

# ---- Build ------------------------------------------------------------------

eba <- readr::read_csv(file.path(PATH_CLEAN, "eba_corporates_wa.csv"),
                       show_col_types = FALSE) |>
  dplyr::mutate(iso2 = unname(EBA_TO_ISO[country_name]))

cat(sprintf("EBA rows total:                %s\n", format(nrow(eba), big.mark = ",")))
unmapped <- sort(unique(eba$country_name[is.na(eba$iso2)]))
if (length(setdiff(unmapped, names(EBA_TO_ISO)))) {
  stop("Unknown EBA country name(s): ", paste(setdiff(unmapped, names(EBA_TO_ISO)), collapse = ", "))
}
eba_iso <- eba |> dplyr::filter(!is.na(iso2))
cat(sprintf("EBA rows with ISO2 (Eurostat): %s  countries=%d\n",
            format(nrow(eba_iso), big.mark = ","), dplyr::n_distinct(eba_iso$iso2)))
cat(sprintf("Dropped (non-Eurostat):        %s\n", paste(unmapped, collapse = ", ")))

# Decimals to percentage points. This is the unit convention for the whole
# project: from here on, every rate in every table is in pp, so 1.25 is 1.25%.
eba_iso <- eba_iso |>
  dplyr::mutate(dplyr::across(c(default_rate, loss_rate, lgd), ~ .x * 100))

gdp  <- build_gdp()
hicp <- build_hicp()
cat(sprintf("\nGDP rows:   %s  geos=%d  quarters=%d\n", format(nrow(gdp), big.mark = ","),
            dplyr::n_distinct(gdp$iso2), dplyr::n_distinct(gdp$quarter)))
cat(sprintf("HICP rows:  %s  geos=%d  quarters=%d\n", format(nrow(hicp), big.mark = ","),
            dplyr::n_distinct(hicp$iso2), dplyr::n_distinct(hicp$quarter)))

panel <- eba_iso |>
  dplyr::left_join(gdp,  by = c("iso2", "quarter")) |>
  dplyr::left_join(hicp, by = c("iso2", "quarter")) |>
  dplyr::select(iso2, country_name, quarter, default_rate, loss_rate, lgd,
                gdp_growth_yoy, inflation_yoy)
# Radix ordering on purpose. The default sort depends on the machine's locale,
# which would make the row order of the output CSV differ between machines for
# no good reason. Radix is locale-independent, so the file is byte-reproducible.
panel <- panel[order(panel$iso2, panel$quarter, method = "radix"), ]

# ---- Checks ----------------------------------------------------------------

cat(sprintf("\nFinal panel: %d rows x %d cols\n", nrow(panel), ncol(panel)))
cat(sprintf("Countries:   %d\n", dplyr::n_distinct(panel$iso2)))
cat(sprintf("Quarters:    %d (%s .. %s)\n", dplyr::n_distinct(panel$quarter),
            min(panel$quarter), max(panel$quarter)))

cat("\nMissing values per column:\n")
print(colSums(is.na(panel)))

n_dup <- sum(duplicated(panel[, c("iso2", "quarter")]))
cat(sprintf("\nDuplicate (iso2, quarter) pairs: %d\n", n_dup))
stopifnot(n_dup == 0)

cat("\nCoverage per country (quarters with default rate, GDP and inflation all present):\n")
cov <- panel |>
  dplyr::mutate(complete = !is.na(default_rate) & !is.na(gdp_growth_yoy) & !is.na(inflation_yoy)) |>
  dplyr::group_by(iso2) |>
  dplyr::summarise(n_complete = sum(complete), n = dplyr::n(),
                   pct = round(100 * sum(complete) / dplyr::n(), 1), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(pct))
print(as.data.frame(cov), row.names = FALSE)

# ---- The macro series on their own, over a longer window --------------------
# This second file is easy to overlook but it matters.
#
# The panel above starts where the default rate starts, in 2015Q4. But step 4
# needs lags and 4-quarter averages, and those reach back further than that. If
# they were computed on the panel itself, the first three or four quarters of
# every country would come out NA. Since the models drop incomplete rows,
# the estimation sample would quietly begin a year after the data does.
#
# So the macro series are written out separately over their full downloaded
# window, restricted to the panel's countries, as a complete quarterly grid.
# Step 4 builds the lags on this file and then joins them onto the panel.

macro_quarters <- sort(unique(c(gdp$quarter, hicp$quarter)))
macro <- tidyr::expand_grid(iso2 = sort(unique(panel$iso2)), quarter = macro_quarters) |>
  dplyr::left_join(gdp,  by = c("iso2", "quarter")) |>
  dplyr::left_join(hicp, by = c("iso2", "quarter")) |>
  dplyr::arrange(iso2, quarter)

# The grid has to be complete: every country x every quarter, gaps included as
# rows. Step 4 lags by row order within country, and that is only correct if
# there are no missing rows. With a hole in the grid, a "1-quarter lag" would
# silently reach back two or three quarters instead.
stopifnot(nrow(macro) == dplyr::n_distinct(macro$iso2) * length(macro_quarters),
          sum(duplicated(macro[, c("iso2", "quarter")])) == 0L)

cat(sprintf("\nMacro grid:  %d rows = %d countries x %d quarters (%s .. %s)\n",
            nrow(macro), dplyr::n_distinct(macro$iso2), length(macro_quarters),
            min(macro_quarters), max(macro_quarters)))
cat(sprintf("  missing GDP %d, missing inflation %d (outside the panel window these are expected)\n",
            sum(is.na(macro$gdp_growth_yoy)), sum(is.na(macro$inflation_yoy))))

# The macro data has to be complete over the range the analysis actually uses:
# the panel window itself, plus the four quarters of run-up that the lags reach
# back into. Gaps outside that range are fine and are not checked.
need_from <- macro_quarters[max(1L, match(min(panel$quarter), macro_quarters) - 4L)]
gaps <- macro |>
  dplyr::filter(quarter >= need_from, quarter <= max(panel$quarter),
                is.na(gdp_growth_yoy) | is.na(inflation_yoy))
if (nrow(gaps)) {
  cat("\n[warn] macro gaps inside the window the lags need:\n")
  print(as.data.frame(gaps), row.names = FALSE)
} else {
  cat(sprintf("  no macro gaps from %s onwards — lags and MA4 are fully fed.\n", need_from))
}

# na = "" writes an empty cell instead of the literal text "NA". That way a
# missing default rate reads as missing in Excel, pandas or anything else that
# opens the file, rather than as a two-character string that quietly turns the
# whole column into text.
readr::write_csv(panel, PATH_PANEL, na = "")
readr::write_csv(macro, PATH_MACRO, na = "")
cat(sprintf("\nSaved: %s\n       %s\n", PATH_PANEL, PATH_MACRO))
