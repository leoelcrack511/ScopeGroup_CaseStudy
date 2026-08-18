# R/01_download_data.R
#
# STEP 1: Download the raw source files.
#
# There are two data providers, and they behave very differently.
#
# EBA Credit Risk Parameters annex (2015Q4-2026Q1)
#   These are static publications: once released, a file never changes, so
#   downloading it again gives you the same bytes.
#
#   The URLs below are written out one by one rather than discovered from an
#   index. That looks tedious, and it is, but the EBA does not maintain a
#   stable index of these files and has changed its URL format at least three
#   times. Hard-coding them is the only approach that actually reproduces.
#
#   The files come in three shapes:
#     35 quarterly spreadsheets   2017Q3-2026Q1, one quarter each
#      1 multi-sheet workbook     2015Q1-2016Q4, all bundled into the Q4-2016
#                                 release, one sheet per quarter
#      2 PDFs                     2017Q1 and 2017Q2, never published as a
#                                 spreadsheet at all
#
#   The Q4-2017 PDF is also downloaded, but it is not a data source. 2017Q4 is
#   the one quarter the EBA published in both formats, so step 2 reads it both
#   ways and checks that the two readers return the same numbers. It is a test,
#   not an input.
#
#   Why the sample starts in 2015Q4 and not 2015Q1: because that is where the
#   variable starts, not where the files start. The annex methodology says so
#   directly: "DR and LR from 2015 Q1 to 2015 Q3 are not provided as they are
#   computed as [a yearly sum]". Those three sheets exist, but they carry PD
#   and LGD only, with no default rate at all.
#
# Eurostat (3 JSON-stat files: real GDP growth and two HICP series)
#   These DO change. Eurostat revises national accounts, so downloading again
#   later fetches the latest vintage, not the one this analysis used. The
#   difference is small but real, and it can move a coefficient in the third
#   decimal. This is why the project ships the JSONs it actually ran on.
#
#   The window starts in 2013, well before the panel does, so that the lags and
#   4-quarter averages at the panel's first quarter are built from observed
#   data instead of NAs.
#
# Anything already on disk is skipped. So running this against the data/raw/
# snapshot that ships with the project does nothing at all, and the frozen files
# are what get used. To force a refresh you have to delete the files first.
# Read the README section on vintages before you do that.
#
# Writes: data/raw/eba/EBA_CreditRisk_<YYYY>Q<n>.xlsx       one per quarter
#         data/raw/eba/EBA_CreditRisk_master_2015_2016.xlsx the 2015-16 bundle
#         data/raw/eba/EBA_CreditRisk_<YYYY>Q<n>.pdf        2017Q1, Q2, Q4
#         data/raw/eurostat/*.json

source(here::here("R", "00_setup.R"))

dir.create(PATH_RAW_EBA,      recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_RAW_EUROSTAT, recursive = TRUE, showWarnings = FALSE)

# ---- EBA -------------------------------------------------------------------

EBA_BASE <- "https://www.eba.europa.eu"
p <- function(x) paste0(EBA_BASE, x)

EBA_URLS <- c(
  "2026Q1" = p("/sites/default/files/2026-06/3445c227-75ab-4222-b1e4-ac1ba701b854/Credit%20Risk%20parameters%20annex%20-%20Q1%202026.xlsx"),
  "2025Q4" = p("/sites/default/files/2026-03/59c749e7-8d66-40ea-801a-67cdac87d6c5/Credit%20Risk%20parameters%20annex%20-%20Q4%202025.xlsx"),
  "2025Q3" = p("/sites/default/files/2025-12/830f3c91-3621-459f-abff-26be53c305b2/Credit%20Risk%20parameters%20annex%20-%20Q3%202025.xlsx"),
  "2025Q2" = p("/sites/default/files/2025-09/eea92f94-7c45-4524-99d7-097b4353db4e/Credit%20Risk%20parameters%20annex%20-%20Q2%202025.xlsx"),
  "2025Q1" = p("/sites/default/files/2025-08/b5afe311-f7e3-4e61-873a-3a2fa8526237/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202025.xlsx"),
  "2024Q4" = p("/sites/default/files/2025-03/43a7008b-0022-405b-9148-f068715657cc/Credit%20Risk%20parameters%20annex%20-%20Q4%202024.xlsx"),
  "2024Q3" = p("/sites/default/files/2024-12/80d0372e-3446-4acf-b512-00b6c1d858cc/Credit%20Risk%20parameters%20annex%20-%20Q3%202024.xlsx"),
  "2024Q2" = p("/sites/default/files/2024-09/eef89f88-11c3-43b9-8530-474deda6513b/Credit%20Risk%20parameters%20annex%20-%20Q2%202024.xlsx"),
  "2024Q1" = p("/sites/default/files/2024-06/772c8c6c-a8b8-4dce-901e-984a5c75022a/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202024.xlsx"),
  "2023Q4" = p("/sites/default/files/2024-04/e499dad2-1ead-4c05-abc2-1125f6c5e1e4/KRI%20-%20Risk%20parameters%20annex%20-%20Q4%202023.xlsx"),
  "2023Q3" = p("/sites/default/files/2024-01/1a0703fa-8b5e-4ebc-8dda-dcdeb7bbd9a9/KRI%20-%20Risk%20parameters%20annex%20-%20Q3%202023.xlsx"),
  "2023Q2" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q2%202023/1062613/KRI%20-%20Risk%20parameters%20annex%20-%20Q2%202023.xlsx"),
  "2023Q1" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q2%202023/1058322/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202023.xlsx"),
  "2022Q4" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q4%202022/1054312/KRI%20-%20Risk%20parameters%20annex%20-%20Q4%202022.xlsx"),
  "2022Q3" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q3%202022/1050800/KRI%20-%20Risk%20parameters%20annex%20-%20Q3%202022.xlsx"),
  "2022Q2" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q2%202022/1040170/KRI%20-%20Risk%20parameters%20annex%20-%20Q2%202022.xlsx"),
  "2022Q1" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/q1%202022/1036531/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202022.xlsx"),
  "2021Q4" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q4%202021/1029359/KRI%20-%20Risk%20parameters%20annex%20-%20Q4%202021.xlsx"),
  "2021Q3" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q3%202021/1025836/KRI%20-%20Risk%20parameters%20annex%20-%20Q3%202021.xlsx"),
  "2021Q2" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q2%202021/1021368/KRI%20-%20Risk%20parameters%20annex%20-%20Q2%202021.xlsx"),
  "2021Q1" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q1%202021/1016350/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202021.xlsx"),
  "2020Q4" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q4%202020/972090/KRI%20-%20Risk%20parameters%20annex%20-%20Q4%202020.xlsx"),
  "2020Q3" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q3%202020/961891/KRI%20-%20Risk%20parameters%20annex%20-%20Q3%202020.xlsx"),
  "2020Q2" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q2%202020/933051/KRI%20-%20Risk%20parameters%20annex%20-%20Q2%202020.xlsx"),
  "2020Q1" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q1%202020/897889/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202020.xlsx"),
  # Releases before 2020. The URLs look completely different because the EBA
  # website used to run on Liferay, with /documents/10180/<node>/<uuid>/ paths.
  # The files themselves are the same: same regulatory source (COREP C 9.02),
  # same sheet layout, same 39 country names. That was checked against the
  # 2020+ files rather than assumed.
  "2019Q4" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q4%202019/882135/KRI%20-%20Risk%20parameters%20annex%20-%20Q4%202019.xlsx"),
  "2019Q3" = p("/sites/default/files/document_library/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q3%202019/KRI%20-%20Risk%20parameters%20annex%20-%20Q3%202019.xlsx"),
  "2019Q2" = p("/sites/default/files/Risk%20Analysis%20and%20Data/Risk%20dashboard/Q2%202019//KRI%20-%20Risk%20parameters%20annex%20-%20Q2%202019.xlsx"),
  "2019Q1" = p("/sites/default/files/documents/10180/2854739/7c4a5fc4-4e25-4191-b571-2f7645f90a78/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202019.xlsx"),
  "2018Q4" = p("/sites/default/files/documents/10180/2666948/e6132352-b8e0-44a4-9e49-8eb58b281757/KRI%20-%20Risk%20parameters%20annex%20-%20Q4%202018.xlsx"),
  "2018Q3" = p("/sites/default/files/documents/10180/2547788/3c9db9f3-b2cc-4756-8893-585411abc24a/KRI%20-%20Risk%20parameters%20annex%20-%20Q3%202018.xlsx"),
  "2018Q2" = p("/sites/default/files/documents/10180/2385362/f125f7c3-fec2-44c8-8439-06ec416f6b9a/KRI%20-%20Risk%20parameters%20annex%20-%20Q2%202018.xlsx"),
  "2018Q1" = p("/sites/default/files/documents/10180/2282718/6601f88f-259d-4c25-8cf2-8ae0e28bc362/KRI%20-%20Risk%20parameters%20annex%20-%20Q1%202018.xlsx"),
  "2017Q4" = p("/sites/default/files/documents/10180/2175405/f7ede5a4-68f6-47fd-9275-cc7176ad1fdf/KRI%20-%20Risk%20parameters%20annex%20-%20Q4%202017.xlsx"),
  # The Q3-2017 file is the last one the EBA published as a spreadsheet with
  # "pwp" in the name; the URL is otherwise unremarkable.
  "2017Q3" = p("/sites/default/files/documents/10180/2085616/6677f6e8-d706-47ef-9c7f-9af5efc66424/KRI%20-%20Risk%20parameters%20annex%20-%20Q3%202017%20pwp.xlsx")
)

# The Q4-2016 release is not one quarter, it is eight. The workbook has one
# sheet per quarter from 2015Q1 to 2016Q4. Only 2015Q4 onwards is usable, for
# the reason given in the header: the first three sheets have no default rate.
EBA_MASTER_URLS <- c(
  "master_2015_2016" = p("/sites/default/files/documents/10180/1804996/bd939b5c-475c-4419-be93-5472402dc67f/Risk%20parameters%20annex%20-%20Q4%202016.xlsx")
)

# 2017Q1 and 2017Q2 were never published as spreadsheets, so the PDFs are the
# only way to get those two quarters. Q4-2017 is downloaded too, but as a test
# rather than as data: it exists in both formats, which lets step 2 check the
# PDF reader against the spreadsheet reader on a quarter where both are known.
EBA_PDF_URLS <- c(
  "2017Q1" = p("/documents/10180/1898284/97bb33ad-704a-45a8-b399-59e2c86eeb2f/Risk%20parameters%20-%20Q1%202017.pdf"),
  "2017Q2" = p("/documents/10180/1981506/0c670bb0-6b0f-4ad6-bab2-71b3027b2895/Risk%20parameters%20annex%20-%20Q2%202017.pdf"),
  "2017Q4" = p("/documents/10180/2175405/KRI+-+Risk+parameters+annex+-+Q4+2017.pdf/727b2f01-75d0-45e9-9e37-2153f3fd3fe2")
)

# The min_bytes check is not paranoia. When a file is missing, the EBA server
# returns a ~10 kB HTML error page with a success status code instead of a real
# failure. Without a size floor, that page gets saved as "the spreadsheet" and
# the problem only surfaces later as a confusing parse error.
fetch <- function(label, url, dest, min_bytes) {
  if (file.exists(dest) && file.size(dest) > min_bytes) {
    cat(sprintf("  [skip] %-18s (%s B)\n", label, format(file.size(dest), big.mark = ",")))
    return(invisible(TRUE))
  }
  ok <- tryCatch({
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
    file.exists(dest) && file.size(dest) > min_bytes
  }, error = function(e) {
    cat(sprintf("  [FAIL] %-18s %s\n", label, conditionMessage(e))); FALSE
  })
  if (ok) cat(sprintf("  [ok]   %-18s (%s B)\n", label, format(file.size(dest), big.mark = ",")))
  else    cat(sprintf("  [FAIL] %-18s download did not produce a usable file\n", label))
  invisible(ok)
}

cat("=== EBA Credit Risk Parameters annex — quarterly spreadsheets ===\n")
for (label in names(EBA_URLS)) {
  fetch(label, EBA_URLS[[label]],
        file.path(PATH_RAW_EBA, paste0("EBA_CreditRisk_", label, ".xlsx")), 50000)
}

cat("\n=== EBA — multi-sheet historical workbook ===\n")
for (label in names(EBA_MASTER_URLS)) {
  fetch(label, EBA_MASTER_URLS[[label]],
        file.path(PATH_RAW_EBA, paste0("EBA_CreditRisk_", label, ".xlsx")), 50000)
}

cat("\n=== EBA — pdf-only quarters (plus the Q4-2017 cross-check fixture) ===\n")
for (label in names(EBA_PDF_URLS)) {
  fetch(label, EBA_PDF_URLS[[label]],
        file.path(PATH_RAW_EBA, paste0("EBA_CreditRisk_", label, ".pdf")), 50000)
}

# ---- Eurostat --------------------------------------------------------------

ES_BASE <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data"

ES_URLS <- c(
  # Real GDP, year-on-year % change, seasonally and calendar adjusted.
  # Starts in 2013-Q1, not at the panel's own start. A 4-quarter lag at 2015Q4
  # already reaches back to 2014Q4, and the 4-quarter averages need a run-up
  # before that again, so the series has to start earlier than the panel.
  namq_10_gdp_yoy = paste0(
    ES_BASE, "/namq_10_gdp?format=JSON&lang=en",
    "&unit=CLV_PCH_SM&s_adj=SCA&na_item=B1GQ&sinceTimePeriod=2013-Q1"),
  # HICP all-items, annual rate of change, monthly. This is the main inflation
  # series and the one that is used wherever it is available.
  prc_hicp_manr = paste0(
    ES_BASE, "/prc_hicp_manr?format=JSON&lang=en",
    "&coicop=CP00&sinceTimePeriod=2013-01"),
  # The same concept taken from Eurostat's short-term dataset, which is
  # published faster. It is used only to fill in the most recent months, where
  # the main series has not been released yet, so that the panel can reach the
  # current quarter.
  #
  # The parameter names differ between the two datasets but mean the same
  # thing: indic=TOTAL here is all-items (CP00 above), and unit=RT12 is the
  # rate of change against the same month a year earlier, i.e. annual.
  ei_cphi_m_recent = paste0(
    ES_BASE, "/ei_cphi_m?format=JSON&lang=en",
    "&indic=TOTAL&unit=RT12&sinceTimePeriod=2024-01")
)

cat("\n=== Eurostat ===\n")
for (nm in names(ES_URLS)) {
  dest <- file.path(PATH_RAW_EUROSTAT, paste0(nm, ".json"))
  if (file.exists(dest)) {
    cat(sprintf("  [skip] %-18s (%s B)\n", nm, format(file.size(dest), big.mark = ",")))
    next
  }
  utils::download.file(ES_URLS[[nm]], dest, mode = "wb", quiet = TRUE)
  cat(sprintf("  [ok]   %-18s (%s B)\n", nm, format(file.size(dest), big.mark = ",")))
}

cat("\nRaw data ready.\n")
