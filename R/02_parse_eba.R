# R/02_parse_eba.R
#
# STEP 2: Parse the EBA annex spreadsheets into tidy data.
#
# Each annex is a spreadsheet of credit-risk parameters, broken down by country,
# asset class and statistic. This script reads every file, puts the numbers into
# one long table, and then narrows that down to the three series the analysis
# actually uses: the Corporates weighted-average default rate, loss rate and
# LGD.
#
# Most of this file exists because of one problem: the EBA has published the
# same table in four different shapes over the years.
#
#   2015-2020 files  the country name sits on the Retail row, which is in the
#                    MIDDLE of that country's block of rows
#   2021+ files      the country name sits on the FIRST row of the block, and
#                    is usually repeated on the Retail row too
#   2022Q1           not a quarterly file at all. It is a multi-sheet workbook
#                    covering 2015Q1-2022Q1, with a different column layout and
#                    ISO2 codes where the others have country names
#   2017Q1, Q2       never published as a spreadsheet. PDF only.
#
# One reader handles the first two layouts. That works because of something
# both layouts have in common: every country's block of rows starts with its
# "Corporates" row. So instead of tracking where the country name is, the
# script cuts the sheet into blocks at each Corporates row, and takes the
# country to be whichever label appears anywhere inside that block. Where the
# label sits within the block is exactly what differs between the layouts, and
# this rule does not care.
#
# The obvious alternative, assigning each row to the nearest country label
# above it, is broken. In the 2015-2020 layout the label is in the middle of the
# block, so the rows above it are nearer to the PREVIOUS country's label, and
# Retail rows end up filed under the wrong country. The block rule cannot fail
# that way, and the checks below assert that each block contains exactly one
# label.
#
# Two files are workbooks rather than single quarters:
#   - The Q4-2016 release has one sheet per quarter for 2015Q1-2016Q4, all in
#     the standard layout, so the same reader just walks every data sheet.
#   - The 2022Q1 master gets its own reader, and only its 2022Q1 sheet is
#     used. That quarter is missing from the standalone releases, which is why
#     the workbook is here at all. Its 2015-2019 sheets are empty templates,
#     which is why the pre-2020 quarters are read from the standalone files
#     instead of from this one workbook.
#
# The PDF reader rebuilds the table from `pdftotext -layout` output. The tricky
# part is that the numbers on the page are right-aligned to each other, not to
# the header labels above them, so you cannot use the headers to work out which
# column a number belongs to. Instead the reader calibrates the twenty column
# positions from the rows on the page that are complete, then matches the
# partial rows against those positions. If a number does not land unambiguously
# in one column, it refuses to guess rather than filing it somewhere plausible.
#
# 2017Q4 was published in both formats, so the cross-check at the bottom of this
# script reads it both ways and asserts the two readers agree cell for cell.
#
# `pdftotext` (from poppler) is the only non-R dependency in the whole project.
# Its output is cached in data/raw/eba/pdf_text/ so the script runs on a machine
# without it. See the comment above parse_annex_pdf() for how the cache is kept
# honest rather than just trusted.
#
# Where the sample starts is decided by the data, not by the file formats. The
# annex methodology says "DR and LR from 2015 Q1 to 2015 Q3 are not provided as
# they are computed as [a yearly sum]". Those three sheets do exist and carry PD
# and LGD, but no default rate, so the analysis slice starts at 2015Q4. There is
# an assertion below that checks this rather than taking it on faith.
#
# One reading detail worth knowing: cells are read with col_types = "list". That
# keeps each cell's own type instead of forcing a whole column into one type.
# Only genuinely numeric cells become observations. Text, blanks and footnote
# markers are skipped, the same way a person reading cell by cell would.
#
# Input:  data/raw/eba/*.xlsx, data/raw/eba/EBA_CreditRisk_2017Q[124].pdf
# Output: data/clean/eba_credit_risk_long.csv   (every country/asset class/stat)
#         data/clean/eba_corporates_wa.csv      (the Corporates W.A. slice used
#                                                by the next step, 2015Q4 on)

source(here::here("R", "00_setup.R"))

dir.create(PATH_CLEAN, recursive = TRUE, showWarnings = FALSE)

STAT_NAMES <- c("N", "25th", "50th", "75th", "W.A")

# ---- Cell-level helpers ----------------------------------------------------
# read_cells() returns the sheet as a grid whose [row, col] indices line up with
# the spreadsheet's own. That means the column constants below are the same
# numbers you would count off if you opened the file in Excel, which makes them
# checkable instead of magic.

read_cells <- function(path, sheet, max_col) {
  suppressWarnings(readxl::read_excel(
    path, sheet = sheet, col_names = FALSE, col_types = "list",
    range = readxl::cell_limits(c(1L, 1L), c(NA, max_col)),
    .name_repair = "minimal"
  ))
}

cell <- function(g, r, c) {
  if (r > nrow(g) || c > ncol(g)) return(NULL)
  v <- g[[c]][[r]]
  if (length(v) == 0) NULL else v
}

is_num <- function(v) length(v) == 1 && is.numeric(v) && !is.na(v)
is_str <- function(v) length(v) == 1 && is.character(v) && !is.na(v)

# Pull the five statistics of one parameter block out of a row's values.
block_records <- function(values, base_col, first_col, param, quarter, country, asset_class) {
  idx <- (base_col - first_col + 1L):(base_col - first_col + 5L)
  out <- list()
  for (i in seq_along(idx)) {
    v <- values[[idx[i]]]
    if (!is_num(v)) next
    out[[length(out) + 1L]] <- list(
      quarter = quarter, country_name = country, asset_class = asset_class,
      parameter = param, stat = STAT_NAMES[i], value = as.numeric(v)
    )
  }
  out
}

# ---- Layout A/B: the standalone quarterly files ----------------------------
# Column layout, stable across every standalone file:
#   col B  country name (only on the block's anchor row)
#   col C  asset class
#   cols D-H   Default rate  (N, 25th, 50th, 75th, W.A)
#   cols I-M   Loss rate
#   cols N-R   PD - adjusted
#   cols S-W   LGD

STANDALONE_PARAMS <- list("Default rate" = 4L, "Loss rate" = 9L,
                          "PD - adjusted" = 14L, "LGD" = 19L)
STANDALONE_START_ROW <- 14L

# The reporting quarter is in cell D8 in every file seen so far. The fallback
# scans the top-left corner for anything shaped like "2026 Q1", so that a future
# release moving the header does not silently produce a quarter-less table.
detect_quarter <- function(g) {
  pat <- "^\\s*(\\d{4})\\s*Q(\\d)\\s*$"
  v <- cell(g, 8L, 4L)
  if (is_str(v) || is_num(v)) {
    m <- regmatches(as.character(v), regexec(pat, as.character(v)))[[1]]
    if (length(m) == 3L) return(paste0(m[2], "-Q", m[3]))
  }
  for (r in 1:15) for (c in 1:7) {
    x <- cell(g, r, c)
    if (is_str(x)) {
      m <- regmatches(x, regexec(pat, x))[[1]]
      if (length(m) == 3L) return(paste0(m[2], "-Q", m[3]))
    }
  }
  stop("cannot detect the reporting quarter in this sheet")
}

NON_DATA_SHEETS <- c("Cover", "Methodology", "Last Page", "SaS output",
                     "check differences", "ST data 2015")

# Most files hold a single quarter. The Q4-2016 release holds eight, one per
# sheet, in exactly the same layout. So this walks every data sheet it finds
# rather than assuming there is only one.
parse_standalone <- function(path) {
  data_sheets <- setdiff(readxl::excel_sheets(path), NON_DATA_SHEETS)
  if (!length(data_sheets)) stop(basename(path), ": no data sheet found")
  dplyr::bind_rows(lapply(data_sheets, function(sh) parse_standalone_sheet(path, sh)))
}

parse_standalone_sheet <- function(path, sheet) {
  g <- read_cells(path, sheet, 23L)
  quarter <- detect_quarter(g)

  # Pass 1: walk the sheet top to bottom and collect the data rows in order,
  # each one carrying whatever label happens to sit on it (usually nothing) plus
  # its numeric cells. No country is assigned yet.
  rows <- list()
  for (r in STANDALONE_START_ROW:nrow(g)) {
    asset_class <- cell(g, r, 3L)
    if (!is_str(asset_class)) next
    asset_class <- trimws(asset_class)
    values <- lapply(4:23, function(c) cell(g, r, c))
    if (!any(vapply(values, is_num, logical(1)))) next   # header or spacer row
    lab <- cell(g, r, 2L)
    rows[[length(rows) + 1L]] <- list(
      asset_class = asset_class,
      label = if (is_str(lab) && nzchar(trimws(lab))) trimws(lab) else NA_character_,
      values = values
    )
  }
  where <- paste0(basename(path), " [", sheet, "]")
  if (!length(rows)) stop(where, ": no data rows found")

  # Pass 2: cut the rows into blocks, starting a new block at each Corporates
  # row, then work out the one country each block belongs to. This is the step
  # that makes both spreadsheet layouts readable by the same code.
  asset_classes <- vapply(rows, function(x) x$asset_class, character(1))
  labels        <- vapply(rows, function(x) x$label, character(1))
  block <- cumsum(asset_classes == "Corporates")
  if (block[1] != 1L) stop(where, ": rows appear before the first Corporates row")

  block_country <- vapply(split(labels, block), function(l) {
    u <- unique(l[!is.na(l)])
    if (length(u) == 0L) NA_character_ else if (length(u) == 1L) u else
      stop(where, ": a country block carries more than one label: ", paste(u, collapse = " / "))
  }, character(1))
  if (anyNA(block_country)) stop(where, ": a country block carries no label")

  countries <- unname(block_country[as.character(block)])

  out <- list()
  for (i in seq_along(rows)) {
    for (param in names(STANDALONE_PARAMS)) {
      out <- c(out, block_records(rows[[i]]$values, STANDALONE_PARAMS[[param]], 4L,
                                  param, quarter, countries[i], rows[[i]]$asset_class))
    }
  }
  dplyr::bind_rows(out)
}

# ---- Layout C: the 2022Q1 sheet of the historical master file --------------
# This workbook is laid out differently from everything else, and it identifies
# countries by ISO2 code instead of by name, so it needs its own reader:
#   R11 col C  year        R13 col C  quarter ("Q1".."Q4")
#   col C  ISO2 code       col G  country name (absent on older sheets)
#   col H  asset class
#   cols I-AB  Default rate | Loss rate | PD - adjusted | LGD  (5 columns each)

MASTER_PARAMS <- list("Default rate" = 9L, "Loss rate" = 14L,
                      "PD - adjusted" = 19L, "LGD" = 24L)
MASTER_START_ROW <- 17L

# Maps ISO2 back to the EBA's own country names. Needed when the name column on
# a sheet is empty, so that what this reader produces can be stacked on top of
# the standalone files, which always carry names rather than codes.
ISO2_TO_NAME <- c(
  AT = "Austria", BE = "Belgium", BG = "Bulgaria", HR = "Croatia",
  CY = "Cyprus", CZ = "Czech", DK = "Denmark", EE = "Estonia",
  FI = "Finland", FR = "France", DE = "Germany", GR = "Greece",
  HU = "Hungary", IE = "Ireland", IT = "Italy", LV = "Latvia",
  LT = "Lithuania", LU = "Luxembourg", MT = "Malta", NL = "Netherlands",
  NO = "Norway", PL = "Poland", PT = "Portugal", RO = "Romania",
  SK = "Slovakia", SI = "Slovenia", ES = "Spain", SE = "Sweden",
  GB = "United Kingdom", UK = "United Kingdom",
  AU = "Australia", CA = "Canada", CN = "China", HK = "Hong Kong",
  IN = "India", KR = "Korea, Republic Of", RU = "Russian Federation",
  SG = "Singapore", CH = "Switzerland", US = "United States",
  LI = "Liechtenstein", IS = "Iceland", JP = "Japan"
)

parse_master_sheet <- function(path, sheet) {
  g <- read_cells(path, sheet, 28L)

  year <- cell(g, 11L, 3L)
  qtr  <- cell(g, 13L, 3L)
  if (is.null(year) || is.null(qtr)) return(dplyr::tibble())
  year <- suppressWarnings(as.integer(trimws(as.character(year))))
  qtr  <- toupper(trimws(as.character(qtr)))
  if (is.na(year) || !startsWith(qtr, "Q")) return(dplyr::tibble())
  quarter <- paste0(year, "-", qtr)

  out <- list()
  for (r in MASTER_START_ROW:nrow(g)) {
    iso2 <- cell(g, r, 3L)
    asset_class <- cell(g, r, 8L)
    if (!is_str(iso2) || !is_str(asset_class)) next
    iso2 <- trimws(iso2)
    asset_class <- trimws(asset_class)

    nm <- cell(g, r, 7L)
    country <- if (is_str(nm)) trimws(nm) else {
      if (iso2 %in% names(ISO2_TO_NAME)) unname(ISO2_TO_NAME[iso2]) else iso2
    }

    values <- lapply(9:28, function(c) cell(g, r, c))
    if (!any(vapply(values, is_num, logical(1)))) next
    for (param in names(MASTER_PARAMS)) {
      out <- c(out, block_records(values, MASTER_PARAMS[[param]], 9L,
                                  param, quarter, country, asset_class))
    }
  }
  dplyr::bind_rows(out)
}

# ---- The pdf layout: 2017Q1 and 2017Q2 -------------------------------------
# `pdftotext -layout` turns the table into fixed-width text, preserving roughly
# where things sat on the page. Everything below is about one question: which of
# the twenty statistic columns does each number on a line belong to?
#
# It would be easy if every row had twenty numbers, but rows where the default
# rate was not published have fewer, and those numbers still have to land in the
# right columns rather than being packed in from the left.
#
# The header labels are not a usable ruler. The numbers are right-aligned to
# each other rather than to the labels above them, and the "N" header in
# particular sits several characters to the left of the counts underneath it.
#
# So each page is calibrated against itself: take the rows that do carry all
# twenty statistics, record where those numbers sit, and match the partial rows
# against those positions.
#
# Three assertions then make it impossible for this to go wrong quietly:
#   - no two numbers on a row may be assigned the same column
#   - the assignment must run left to right
#   - every number must be clearly closer to its own column than to any other
# If any of them fails the script stops instead of producing a plausible table.

PDF_PARAMS <- c("Default rate", "Loss rate", "PD - adjusted", "LGD")
PDF_HDR_PAT <- "(N|25th|50th|75th|W\\.A)"
PDF_NUM_PAT <- "[0-9][0-9.,]*%?"

# Positions of every match of `pat` in one line, with each match's own extent.
line_tokens <- function(s, pat) {
  m <- gregexpr(pat, s, perl = TRUE)[[1]]
  if (m[1] == -1L) return(NULL)
  txt <- regmatches(s, gregexpr(pat, s, perl = TRUE))[[1]]
  data.frame(txt = txt, start = as.integer(m),
             end = as.integer(m) + nchar(txt) - 1L, stringsAsFactors = FALSE)
}

# `pdftotext` is the only part of this pipeline that is not R, so its output is
# cached next to the PDFs in data/raw/eba/pdf_text/. That way the project runs
# on a machine without poppler installed.
#
# The important detail: what is cached is the TEXT, not the parsed numbers. The
# reader below runs either way, and so do its three column assertions and the
# 2017Q4 cross-check against the spreadsheet. Caching parsed numbers would mean
# shipping results nobody can re-derive; caching text means the derivation still
# happens on your machine.
#
# And when poppler IS installed, the conversion runs live as well and the two
# results are asserted to match. So the cache cannot quietly drift away from the
# PDF it was made from without the script noticing.

pdf_cache_path <- function(path)
  file.path(PATH_RAW_EBA, "pdf_text",
            sub("[.]pdf$", ".txt", basename(path), ignore.case = TRUE))

# Text straight from poppler, or NULL when poppler is not on the PATH.
pdf_text_live <- function(path) {
  if (!nzchar(Sys.which("pdftotext"))) return(NULL)
  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt), add = TRUE)
  if (system2("pdftotext", c("-layout", shQuote(path), shQuote(txt))) != 0L)
    stop(basename(path), ": pdftotext failed — is poppler installed?")
  readLines(txt, warn = FALSE)
}

parse_annex_pdf <- function(path) {
  cache <- pdf_cache_path(path)
  live  <- pdf_text_live(path)

  if (!file.exists(cache)) {
    if (is.null(live))
      stop(basename(path), ": pdftotext is not on the PATH and there is no ",
           "cached text at ", cache, ". Install poppler (brew install poppler).")
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    writeLines(live, cache)
    cat(sprintf("  %-38s text cached\n", basename(path)))
    return(parse_annex_pdf_text(live, path))
  }

  out <- parse_annex_pdf_text(readLines(cache, warn = FALSE), path)
  if (is.null(live)) {
    cat(sprintf("  %-38s poppler absent, cached text used\n", basename(path)))
  } else if (!isTRUE(all.equal(out, parse_annex_pdf_text(live, path)))) {
    stop(basename(path), ": the cached text at ", cache, " does not parse to ",
         "the same values as the PDF itself — one of the two has changed.")
  } else {
    cat(sprintf("  %-38s cache verified against pdftotext\n", basename(path)))
  }
  out
}

parse_annex_pdf_text <- function(raw, path) {
  # The reporting quarter is the cover line, "Q1 2017".
  qline <- grep("^Q[1-4]\\s+\\d{4}$", trimws(raw))
  if (!length(qline)) stop(basename(path), ": cannot find the reporting quarter")
  g <- regmatches(trimws(raw[qline[1]]), regexec("^Q([1-4])\\s+(\\d{4})$", trimws(raw[qline[1]])))[[1]]
  quarter <- paste0(g[3], "-Q", g[2])

  rows <- list()
  for (pg in strsplit(paste(raw, collapse = "\n"), "\f", fixed = TRUE)[[1]]) {
    lines <- strsplit(pg, "\n", fixed = TRUE)[[1]]
    hdr_i <- which(vapply(lines, function(l) {
      h <- line_tokens(l, PDF_HDR_PAT)
      !is.null(h) && nrow(h) == 20L && sum(h$txt == "W.A") == 4L
    }, logical(1)))
    if (!length(hdr_i)) next               # cover page, methodology page
    h <- line_tokens(lines[hdr_i[1]], PDF_HDR_PAT)
    label_end <- h$start[1] - 4L           # counts can sit left of the "N" label

    cand <- list()
    for (l in lines[(hdr_i[1] + 1L):length(lines)]) {
      head_txt <- substr(l, 1, label_end)
      ac_at <- regexpr("(Corporates|Retail)", head_txt)
      if (ac_at == -1L) next
      v <- line_tokens(substr(l, label_end + 1L, nchar(l)), PDF_NUM_PAT)
      if (is.null(v)) next
      v$end <- v$end + label_end
      lab <- trimws(substr(head_txt, 1, ac_at - 1L))
      cand[[length(cand) + 1L]] <- list(
        asset_class = trimws(substr(head_txt, ac_at, nchar(head_txt))),
        label = if (nzchar(lab)) lab else NA_character_,
        tok = v, line = trimws(l))
    }
    if (!length(cand)) next

    complete <- Filter(function(x) nrow(x$tok) == 20L, cand)
    if (length(complete) < 3L)
      stop(basename(path), ": only ", length(complete),
           " complete rows on a page — too few to calibrate the columns on")
    anchors <- apply(vapply(complete, function(x) x$tok$end, numeric(20)), 1L, stats::median)
    if (min(diff(anchors)) < 4)
      stop(basename(path), ": calibrated columns only ", min(diff(anchors)), " chars apart")

    for (x in cand) {
      d    <- abs(outer(x$tok$end, anchors, "-"))
      slot <- apply(d, 1L, which.min)
      gap  <- apply(d, 1L, function(z) diff(sort(z)[1:2]))
      if (anyDuplicated(slot))  stop(basename(path), ": two numbers claim one column: ", x$line)
      if (any(diff(slot) <= 0)) stop(basename(path), ": columns not left to right: ", x$line)
      if (any(gap < 1))         stop(basename(path), ": ambiguous column: ", x$line)

      vals <- rep(NA_real_, 20L)
      pct  <- grepl("%", x$tok$txt, fixed = TRUE)
      num  <- suppressWarnings(as.numeric(gsub("[%,]", "", x$tok$txt)))
      vals[slot] <- ifelse(pct, num / 100, num)
      rows[[length(rows) + 1L]] <- list(asset_class = x$asset_class,
                                        label = x$label, values = vals)
    }
  }
  if (!length(rows)) stop(basename(path), ": no data rows found")

  # Same block rule as the spreadsheets: start a new block at each Corporates
  # row, and expect exactly one country label inside each block.
  asset_classes <- vapply(rows, function(x) x$asset_class, character(1))
  labels        <- vapply(rows, function(x) x$label, character(1))
  block <- cumsum(asset_classes == "Corporates")
  if (block[1] != 1L) stop(basename(path), ": rows appear before the first Corporates row")
  block_country <- vapply(split(labels, block), function(l) {
    u <- unique(l[!is.na(l)])
    if (length(u) == 0L) NA_character_ else if (length(u) == 1L) u else
      stop(basename(path), ": a country block carries more than one label: ",
           paste(u, collapse = " / "))
  }, character(1))
  if (anyNA(block_country)) stop(basename(path), ": a country block carries no label")
  countries <- unname(block_country[as.character(block)])

  out <- list()
  for (i in seq_along(rows)) {
    for (p in seq_along(PDF_PARAMS)) {
      for (s in 1:5) {
        v <- rows[[i]]$values[(p - 1L) * 5L + s]
        if (is.na(v)) next
        out[[length(out) + 1L]] <- list(
          quarter = quarter, country_name = countries[i],
          asset_class = rows[[i]]$asset_class, parameter = PDF_PARAMS[p],
          stat = STAT_NAMES[s], value = v)
      }
    }
  }
  dplyr::bind_rows(out)
}

# ---- Run over every file ---------------------------------------------------

xlsx <- sort(list.files(PATH_RAW_EBA, pattern = "^EBA_CreditRisk_.*[.]xlsx$",
                        full.names = TRUE))
if (!length(xlsx)) stop("No EBA files in ", PATH_RAW_EBA, " — run R/01_download_data.R first.")

master_file <- file.path(PATH_RAW_EBA, "EBA_CreditRisk_2022Q1.xlsx")
standalone  <- xlsx[basename(xlsx) != basename(master_file)]

cat(sprintf("Parsing %d annex spreadsheets...\n", length(standalone)))
parsed <- lapply(standalone, function(f) {
  d <- parse_standalone(f)
  cat(sprintf("  %-38s %6s rows  quarters=%2d  countries=%2d\n", basename(f),
              format(nrow(d), big.mark = ","), dplyr::n_distinct(d$quarter),
              dplyr::n_distinct(d$country_name)))
  d
})

# The 2022Q1 quarter comes from the master file's own sheet.
if (file.exists(master_file)) {
  d <- parse_master_sheet(master_file, "2022Q1")
  cat(sprintf("  %-38s %6s rows  (2022Q1 sheet of the master workbook)\n",
              basename(master_file), format(nrow(d), big.mark = ",")))
  parsed <- c(parsed, list(d))
}

# 2017Q1 and 2017Q2 exist only as pdf.
pdf_quarters <- c("2017Q1", "2017Q2")
cat("\nParsing the pdf-only quarters...\n")
for (lbl in pdf_quarters) {
  f <- file.path(PATH_RAW_EBA, paste0("EBA_CreditRisk_", lbl, ".pdf"))
  if (!file.exists(f)) stop("Missing ", basename(f), " — run R/01_download_data.R first.")
  d <- parse_annex_pdf(f)
  cat(sprintf("  %-38s %6s rows  countries=%2d\n", basename(f),
              format(nrow(d), big.mark = ","), dplyr::n_distinct(d$country_name)))
  parsed <- c(parsed, list(d))
}

# ---- Does the pdf reader agree with the spreadsheet reader? -----------------
# This is the test that makes the PDF numbers trustworthy.
#
# 2017Q4 is the one quarter the EBA published in both formats, so both readers
# can be pointed at the same quarter and compared. If the PDF reader has a
# column-alignment bug, this is where it shows up.
#
# What has to hold: both readers find exactly the same set of cells, and the
# values agree. They cannot agree perfectly, because the PDF displays only two
# decimals, so the tolerance is exactly that rounding: at most 0.005 percentage
# points, which is 5e-5 on the decimal scale the raw files use.

check_pdf <- file.path(PATH_RAW_EBA, "EBA_CreditRisk_2017Q4.pdf")
if (file.exists(check_pdf)) {
  cat("\nCross-checking the pdf reader against the spreadsheet on 2017Q4...\n")
  a <- parse_annex_pdf(check_pdf)
  b <- parse_standalone(file.path(PATH_RAW_EBA, "EBA_CreditRisk_2017Q4.xlsx"))
  k <- c("country_name", "asset_class", "parameter", "stat")
  cmp <- dplyr::full_join(dplyr::select(a, dplyr::all_of(k), from_pdf = value),
                          dplyr::select(b, dplyr::all_of(k), from_xlsx = value), by = k)
  only_one <- sum(is.na(cmp$from_pdf) | is.na(cmp$from_xlsx))
  worst <- max(abs(cmp$from_pdf - cmp$from_xlsx), na.rm = TRUE)
  cat(sprintf("  cells in both: %s   in only one: %d   max |difference|: %.1e pp\n",
              format(sum(!is.na(cmp$from_pdf) & !is.na(cmp$from_xlsx)), big.mark = ","),
              only_one, worst * 100))
  if (only_one != 0L) stop("the two readers disagree on which cells exist")
  # The real bound is 5e-5. The tiny bit of slack on top is there only to absorb
  # floating-point noise from comparing a rounded two-decimal percentage against
  # a full-precision one, not to loosen the test.
  if (worst > 5e-5 + 1e-9)
    stop("the two readers disagree by more than display rounding: ", worst)
  cat("  agreed: identical cell sets, differences bounded by two-decimal rounding.\n")
} else {
  cat("\n[warn] 2017Q4 pdf absent — the pdf/spreadsheet cross-check was skipped.\n")
}

long <- dplyr::bind_rows(parsed)

# No quarter may arrive from two different files. The sources overlap in places
# (the 2022Q1 workbook covers years the standalone releases also cover), so this
# guards against the same quarter being read twice and silently duplicated.
dup_q <- long |>
  dplyr::distinct(quarter, country_name, asset_class, parameter, stat) |>
  nrow()
if (dup_q != nrow(long)) stop("the same statistic arrived twice — overlapping sources?")

cat(sprintf("\nTotal rows:    %s\n", format(nrow(long), big.mark = ",")))
cat(sprintf("Quarters:      %d (%s .. %s)\n", dplyr::n_distinct(long$quarter),
            min(long$quarter), max(long$quarter)))
cat(sprintf("Countries:     %d\n", dplyr::n_distinct(long$country_name)))
cat(sprintf("Asset classes: %d\n", dplyr::n_distinct(long$asset_class)))

readr::write_csv(long, file.path(PATH_CLEAN, "eba_credit_risk_long.csv"))

# ---- The slice the analysis needs ------------------------------------------
# Corporates, weighted average, three parameters, one row per country-quarter.
#
# If the EBA did not publish a country-quarter, it simply has no row here, and
# it stays missing for the rest of the pipeline. It is never filled with a zero
# or interpolated. A missing default rate means "not reported", which is not the
# same thing as "no defaults", and treating it as zero would bias every average
# downwards.
#
# The sample starts at 2015Q4 because that is where the variable starts, not
# where the data does. 2015Q1-Q3 are present in the long table above, carrying
# PD and LGD, but they have no default rate at all. The line below asserts that
# before dropping them, so if the EBA ever backfills those quarters this stops
# rather than silently continuing to discard them.

SAMPLE_START <- "2015-Q4"

pre <- long |>
  dplyr::filter(quarter < SAMPLE_START, parameter %in% c("Default rate", "Loss rate"))
if (nrow(pre) > 0L)
  stop("found ", nrow(pre), " default/loss rate cells before ", SAMPLE_START,
       " — the sample start needs revisiting")
cat(sprintf("\nQuarters before %s carry no default rate, as the annex states. Cut.\n",
            SAMPLE_START))

wide <- long |>
  dplyr::filter(quarter >= SAMPLE_START,
                asset_class == "Corporates", stat == "W.A",
                parameter %in% c("Default rate", "Loss rate", "LGD")) |>
  tidyr::pivot_wider(id_cols = c(quarter, country_name),
                     names_from = parameter, values_from = value,
                     values_fn = dplyr::first) |>
  dplyr::rename(default_rate = `Default rate`, loss_rate = `Loss rate`, lgd = LGD)

readr::write_csv(wide, file.path(PATH_CLEAN, "eba_corporates_wa.csv"))

cat(sprintf("\nSaved:\n  %s\n  %s  (%s rows)\n",
            file.path(PATH_CLEAN, "eba_credit_risk_long.csv"),
            file.path(PATH_CLEAN, "eba_corporates_wa.csv"),
            format(nrow(wide), big.mark = ",")))
