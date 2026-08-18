# R/05_eda.R
#
# STEP 5: Look at the data before modelling it.
#
# Descriptive statistics, distributions, time series by country, scatter plots,
# correlations, and a map of what is missing. The aim is to know the shape of
# the data: how skewed the default rate is, how much of the variation is
# between countries rather than over time, which countries are thin. Better to
# know that now than to meet it later as a regression coefficient.
#
# Deliberately no regressions and no formal tests here. Those are step 7. This
# step only describes.
#
# Input:   output/models/panel_model_ready.rds
# Outputs: output/tables/eda_*.tex, .html
#          output/figures/eda_*.png

source(here::here("R", "00_setup.R"))

panel <- readRDS(file.path(PATH_MODELS, "panel_model_ready.rds"))

dir.create(PATH_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_FIGURES, recursive = TRUE, showWarnings = FALSE)

# The subset used for anything meant to describe the estimation sample: the
# countries that pass the coverage filter, minus Cyprus. Plots showing the data
# as a whole use all 29 countries instead, and say so.
panel_model <- panel |> filter(is_main_sample)

# Constants for COVID shading
COVID_START <- as.Date("2020-01-01")
COVID_END   <- as.Date("2021-12-31")

# ============================================================================
# A. Descriptive statistics (modelling sample = is_main_sample countries)
# ============================================================================

desc_vars <- panel_model |>
  select(default_rate, loss_rate, lgd, recovery,
         gdp_growth_yoy, inflation_yoy)

datasummary(
  All(desc_vars) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data   = desc_vars,
  output = file.path(PATH_TABLES, "eda_descriptives.tex")
)
datasummary(
  All(desc_vars) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data   = desc_vars,
  output = file.path(PATH_TABLES, "eda_descriptives.html")
)

# Country-level summary, all 29 countries. Shown in full rather than filtered so
# you can see how different the countries are from each other, and which ones
# the coverage and sample flags remove.
country_summary <- panel |>
  group_by(iso2, country_name, coverage_pct, passes_coverage, is_main_sample) |>
  summarise(
    n_valid   = sum(!is.na(default_rate) &
                    !is.na(gdp_growth_yoy) &
                    !is.na(inflation_yoy)),
    mean_dr   = mean(default_rate,   na.rm = TRUE),
    sd_dr     = sd(default_rate,     na.rm = TRUE),
    mean_gdp  = mean(gdp_growth_yoy, na.rm = TRUE),
    mean_inf  = mean(inflation_yoy,  na.rm = TRUE),
    .groups   = "drop"
  ) |>
  arrange(desc(is_main_sample), desc(coverage_pct), iso2)

readr::write_csv(country_summary,
                 file.path(PATH_TABLES, "eda_country_summary.csv"))

# ============================================================================
# B. Distribution of DR
# ============================================================================

p_hist <- ggplot(panel_model, aes(x = default_rate)) +
  geom_histogram(bins = 40, fill = "grey40", colour = "white") +
  geom_vline(xintercept = mean(panel_model$default_rate,   na.rm = TRUE),
             linetype = "dashed", colour = "steelblue") +
  geom_vline(xintercept = median(panel_model$default_rate, na.rm = TRUE),
             linetype = "dotted", colour = "firebrick") +
  labs(title = "Default rate — histogram",
       subtitle = "dashed = mean, dotted = median",
       x = "Default rate (pp)", y = "Count")

p_dens <- ggplot(panel_model, aes(x = default_rate)) +
  geom_density(fill = "grey70", alpha = 0.6) +
  labs(title = "Default rate — density",
       x = "Default rate (pp)", y = "Density")

ggsave(file.path(PATH_FIGURES, "eda_dr_distribution.png"),
       p_hist + p_dens, width = 10, height = 4.5, dpi = 300)

# ============================================================================
# C. Time series by country
# ============================================================================

# C1. DR by country: 29-panel facet, COVID shaded, excluded marked
country_order <- country_summary |>
  arrange(desc(is_main_sample), desc(coverage_pct), iso2) |>
  pull(iso2)

# Facet labels: countries in the estimation sample get a plain ISO2 code, the
# rest get a star. "The rest" means either failing the coverage filter (MT, LV)
# or being dropped on data quality (CY). Marking them keeps all 29 countries
# visible while making it obvious which ones the estimates rest on.
in_main <- panel |>
  distinct(iso2, is_main_sample) |>
  tibble::deframe()

label_order <- country_order |>
  purrr::map_chr(~ if (isTRUE(in_main[[.x]])) .x else paste0(.x, " *"))

panel_facet <- panel |>
  mutate(iso2 = factor(iso2, levels = country_order),
         panel_label = if_else(is_main_sample,
                               as.character(iso2),
                               paste0(iso2, " *")),
         panel_label = factor(panel_label, levels = label_order))

p_dr_facet <- panel_facet |>
  ggplot(aes(x = quarter_date, y = default_rate)) +
  annotate("rect", xmin = COVID_START, xmax = COVID_END,
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "red") +
  geom_line(colour = "grey20") +
  geom_point(size = 0.6, colour = "grey20") +
  facet_wrap(~ panel_label, ncol = 6, scales = "free_y") +
  labs(title = paste0("Default rate by country (", gsub("-","",min(panel$quarter)), " – ", gsub("-","",max(panel$quarter)), ")"),
       subtitle = "Red shading = COVID (2020Q1–2021Q4). '*' = excluded from main sample (coverage or artefact).",
       x = NULL, y = "Default rate (pp)") +
  theme(strip.text = element_text(size = 8))

ggsave(file.path(PATH_FIGURES, "eda_dr_by_country.png"),
       p_dr_facet, width = 13, height = 10, dpi = 300)

# C2. Detail: DR + GDP + INF for core countries
core <- c("DE", "FR", "IT", "ES", "NL", "IE")

panel_core_long <- panel |>
  filter(iso2 %in% core) |>
  select(iso2, quarter_date, default_rate, gdp_growth_yoy, inflation_yoy) |>
  pivot_longer(cols = c(default_rate, gdp_growth_yoy, inflation_yoy),
               names_to = "variable", values_to = "value") |>
  mutate(variable = dplyr::recode(variable,
                                  default_rate   = "Default rate",
                                  gdp_growth_yoy = "GDP YoY",
                                  inflation_yoy  = "HICP YoY"))

p_core <- ggplot(panel_core_long,
                 aes(x = quarter_date, y = value, colour = variable)) +
  annotate("rect", xmin = COVID_START, xmax = COVID_END,
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "red") +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ iso2, ncol = 3) +
  scale_colour_manual(values = c("Default rate" = "black",
                                 "GDP YoY"      = "steelblue",
                                 "HICP YoY"     = "darkorange")) +
  labs(title = "Core sample — DR, GDP YoY, HICP YoY",
       subtitle = "Red shading = COVID (2020Q1–2021Q4)",
       x = NULL, y = "pp", colour = NULL) +
  theme(legend.position = "bottom")

ggsave(file.path(PATH_FIGURES, "eda_timeseries_core.png"),
       p_core, width = 11, height = 6.5, dpi = 300)

# ============================================================================
# D. Scatter macro vs DR (contemporaneous + L1), colour by COVID
# ============================================================================

scatter_data <- panel_model |>
  mutate(period = if_else(covid == 1L, "COVID (2020Q1-2021Q4)", "Non-COVID"))

make_scatter <- function(data, x, y, xlab, title) {
  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(aes(colour = period), alpha = 0.55, size = 1.2) +
    geom_smooth(method = "lm", se = TRUE, colour = "black",
                linewidth = 0.5, formula = y ~ x) +
    scale_colour_manual(values = c("COVID (2020Q1-2021Q4)" = "firebrick",
                                   "Non-COVID"             = "steelblue")) +
    labs(title = title, x = xlab, y = "Default rate (pp)", colour = NULL) +
    theme(legend.position = "bottom")
}

p_s1 <- make_scatter(scatter_data, "gdp_growth_yoy", "default_rate",
                     "GDP YoY (pp)", "DR vs GDP (contemp.)")
p_s2 <- make_scatter(scatter_data |> filter(!is.na(gdp_l1)),
                     "gdp_l1", "default_rate",
                     "GDP YoY L1 (pp)", "DR vs GDP (L1)")
p_s3 <- make_scatter(scatter_data, "inflation_yoy", "default_rate",
                     "HICP YoY (pp)", "DR vs HICP (contemp.)")
p_s4 <- make_scatter(scatter_data |> filter(!is.na(inf_l1)),
                     "inf_l1", "default_rate",
                     "HICP YoY L1 (pp)", "DR vs HICP (L1)")

p_scatters <- (p_s1 + p_s2) / (p_s3 + p_s4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(PATH_FIGURES, "eda_scatters.png"),
       p_scatters, width = 11, height = 9, dpi = 300)

# ============================================================================
# E. Correlation matrix
# ============================================================================

corr_vars <- panel_model |>
  select(default_rate, gdp_growth_yoy, gdp_l1, gdp_l2,
         inflation_yoy, inf_l1, inf_l2, recovery) |>
  tidyr::drop_na()

datasummary_correlation(
  corr_vars,
  output = file.path(PATH_TABLES, "eda_correlations.tex")
)
datasummary_correlation(
  corr_vars,
  output = file.path(PATH_TABLES, "eda_correlations.html")
)

# ============================================================================
# F. Missing heatmap (all 29 countries, ordered by coverage)
# ============================================================================

panel_miss <- panel |>
  mutate(iso2 = factor(iso2, levels = rev(country_order)),
         status = case_when(
           !is.na(default_rate) & !is.na(gdp_growth_yoy) &
             !is.na(inflation_yoy)                     ~ "Complete",
           is.na(default_rate)                         ~ "DR missing",
           TRUE                                        ~ "Macro missing"
         ))

p_miss <- ggplot(panel_miss,
                 aes(x = quarter_date, y = iso2, fill = status)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("Complete"      = "grey30",
                               "DR missing"    = "firebrick",
                               "Macro missing" = "orange")) +
  labs(title = "Data availability by country × quarter",
       subtitle = "Ordered by main-sample flag + coverage. Below dashed line = excluded from main sample.",
       x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

n_excluded <- sum(!country_summary$is_main_sample)
if (n_excluded > 0) {
  p_miss <- p_miss +
    geom_hline(yintercept = n_excluded + 0.5,
               linetype = "dashed", colour = "black")
}

ggsave(file.path(PATH_FIGURES, "eda_missing_heatmap.png"),
       p_miss, width = 11, height = 7.5, dpi = 300)

# ============================================================================
# Summary to console
# ============================================================================
message("Descriptives (modelling sample = ",
        dplyr::n_distinct(panel_model$iso2), " countries):")
print(summary(desc_vars))
message("")
message("Correlations (complete cases: ", nrow(corr_vars), " rows)")
print(round(cor(corr_vars), 3))
message("")
message("Saved 5 figures + 2 tables (tex/html) + 1 country summary (csv).")
