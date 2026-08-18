# Corporate Default Rates and Macroeconomic Conditions

A satellite model for stress testing, built on EBA Credit Risk Parameters and Eurostat macro data.

This repo holds the full code and data behind the analysis. One command reproduces every table,
figure and number in the write-up. The findings themselves are covered in the presentation, so
this file is just a guide to the project.

## Quick start

```bash
Rscript R/install_packages.R    # once
Rscript run_all.R               # 1-3 minutes
```

Run both from the project root. Everything lands in `output/`, which the run creates. Nothing is
shipped pre-built.

`run_all.R` ends with a reproducibility check: it re-reads nine headline estimates from the
objects it just produced and compares them against the published values. If a change moves one,
the run tells you.

## What the project does

The question: do corporate default rates respond to the macroeconomy in a way a stress test can
actually use?

Short answer for 2015Q4 to 2026Q1: no estimator here gives a precisely estimated average European
GDP sensitivity, and the four of them disagree on the point estimate by a wide margin. What holds
up better is dispersion, the sensitivities clearly differ across countries. So there is no single
coefficient to treat as a European GDP-to-default multiplier.

## How it is organised

Eleven steps in two layers, always run in order.

**Data layer, steps 1 to 3.** Downloads the EBA annex releases and three Eurostat series, parses
the Corporates default rate, loss rate and LGD out of four different publication formats, and
merges it all into one panel. Step 3b is an optional check on the publication seams.

**Analysis layer, steps 4 to 11.** Derived columns and sample filters, EDA, four baseline
regressions, diagnostics, robustness checks, country-by-country slopes and heterogeneous-slope
estimators, the stress illustration, and finally the presentation layer.

Step 11 writes `output/models/report_numbers.rds`. Every number the write-up states in prose is
computed there and read back from that object, so the text formats values instead of restating
them by hand.

## The panel

`data/clean/panel_country_quarter.csv`, 1,218 rows, 29 countries by 42 quarters, 2015Q4 to
2026Q1. Everything in percentage points.

| Column | Notes |
|---|---|
| `iso2`, `country_name`, `quarter` | `EL` for Greece, Eurostat convention |
| `default_rate` | Corporates weighted average, rolling four quarters |
| `loss_rate`, `lgd` | Corporates weighted average |
| `gdp_growth_yoy` | Real GDP YoY, Eurostat `namq_10_gdp` |
| `inflation_yoy` | HICP all-items YoY, `prc_hicp_manr` plus `ei_cphi_m` |

Three things worth knowing. The sample starts in 2015Q4 because that is where the variable starts,
the annex does not report a default rate for 2015Q1 to Q3. The default rate covers only the IRB
banks that report to the EBA, not every firm in a country. And missing means not reported, nothing
is ever imputed.

The estimation sample drops countries with thin coverage plus one data-quality exclusion, leaving
26 countries and N = 1,023. Both are flags in `04_load_data.R`, and the robustness checks put the
excluded countries back.

## Output

```
output/
├── tables/      34 regression and diagnostic tables (.tex, .html, .png, .csv)
├── figures/     12 plots (.png, 300 dpi)
└── models/      7 .rds objects, the fitted models and report_numbers.rds
```

## Notes on running it

**Rebuilding the panel.** `run_all.R` sets `REBUILD_DATA <- FALSE`, so steps 4 to 11 run against
the panel already in `data/clean/`. Set it to `TRUE` to rebuild from `data/raw/` first. Both paths
produce a byte-identical panel and the same nine headline numbers.

**Data vintages.** `data/raw/` is a frozen snapshot and step 1 skips files that already exist, so a
rebuild stays offline. Deleting the Eurostat JSONs will fetch a current vintage, which can move
third decimals. The EBA spreadsheets are static and do not have this problem.

**poppler is optional.** Two quarters exist only as PDF and need `pdftotext` to convert. The
extracted text is cached in `data/raw/eba/pdf_text/`, so the pipeline runs without poppler
installed. If poppler is present, step 2 also runs the conversion live and checks it against the
cache.

**fwildclusterboot is optional.** Used once for a wild cluster bootstrap in step 7, which is a
robustness column and not an input to any reported coefficient. It needs a Fortran compiler, so it
is not a hard requirement. Without it, step 7 reports CR2 and clustered standard errors only.

## Layout

```
.
├── README.md
├── presentation.pdf              the write-up and results
├── run_all.R                     single entry point, 11 steps plus the reproducibility check
├── R/
│   ├── 00_setup.R                packages, paths, theme, sourced by every script
│   ├── 01_download_data.R  ...  03b_compat_check.R     data layer
│   ├── 04_load_data.R      ...  11_key_figures.R       analysis layer
│   └── install_packages.R
└── data/
    ├── raw/
    │   ├── eba/                  36 xlsx and 3 pdf, as published
    │   └── eurostat/             3 json, as returned by the API
    └── clean/                    the built panel, macro grid and Corporates extract
```
