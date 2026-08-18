# R/11_key_figures.R
#
# STEP 11: Build the exhibits for the write-up.
#
# This is a presentation layer and nothing else. It reads the model objects that
# steps 4-10 saved into output/models/ and decides what to show and how.
#
# NOTHING IS RE-ESTIMATED HERE. Every number comes from an object another script
# produced. If a figure in this file disagrees with a table from step 8, the bug
# is in this file, because this file has no independent source of numbers.
#
# There is one more rule attached to this script, and it matters more than
# anything else in it. The write-up does not compute or type numbers. Every
# figure the prose quotes is computed here and written to
# output/models/report_numbers.rds; the write-up only formats what it reads.
#
# The reason is concrete rather than stylistic. The same bug appeared repeatedly
# during this project: a sentence with a hard-coded number stating a conclusion,
# printed directly next to the coefficient that contradicted it. Sentences do
# not update themselves when the sample changes. So if you add a figure to the
# write-up, add it to the .rds first. Never type it into the prose.
#
# Exhibits produced for the body of the write-up:
#   fig1_data_tension.png      the default rate is slow and bounded, GDP is not
#   fig2_gdp_stability.png     the GDP coefficient stays near zero everywhere
#   fig3 = country_slopes_forest.png   reused from step 9: dispersion in beta
#   fig4_stress_dispersion.png one common multiplier versus the country range
#   table_main_estimators.*    FE / Mean Group / Swamy / CCE-MG, GDP only
#   table_robustness_main.*    5 columns, not 10
#
# Everything else stays in output/ as appendix material.

source(here::here("R", "00_setup.R"))

panel <- readRDS(file.path(PATH_MODELS, "panel_model_ready.rds"))
base  <- readRDS(file.path(PATH_MODELS, "baselines.rds"))
rb    <- readRDS(file.path(PATH_MODELS, "robustness.rds"))
sh    <- readRDS(file.path(PATH_MODELS, "slope_het.rds"))
st    <- readRDS(file.path(PATH_MODELS, "stress_scenario.rds"))
dg    <- readRDS(file.path(PATH_MODELS, "diagnostics.rds"))

dat <- panel |> dplyr::filter(is_main_sample)

# The sample label is built from the data, so captions cannot drift away from
# the panel they describe. They did exactly that once: the captions read
# "2020Q1-2026Q1" for a while after the panel had been extended back to 2015Q4.
SAMPLE_LABEL <- sprintf("%d countries, %s-%s",
                        dplyr::n_distinct(dat$iso2),
                        gsub("-", "", min(dat$quarter)),
                        gsub("-", "", max(dat$quarter)))

COVID_START <- as.Date("2020-01-01")
COVID_END   <- as.Date("2021-12-31")

# Signed two-decimal formatter that rounds BEFORE it takes the sign, so a value
# of -0.0027 prints as "0.00" rather than the nonsensical "-0.00" the plain
# sprintf("%+.2f") produced in the fig4 annotation.
signed <- function(x, digits = 2) {
  r <- round(x, digits)
  sprintf(paste0("%+.", digits, "f"), if (isTRUE(all.equal(r, 0))) 0 else r)
}

# =============================================================================
# FIGURE 1: the tension in the data
# =============================================================================
# Message: the default rate moves inside a narrow band and slowly, while GDP
# growth swings by tens of percentage points within a few quarters. The reader
# should see why a single common slope comes out near zero before being told.

# Both sds on the ESTIMATION sample (rows with DR observed), so the two numbers
# quoted in the subtitle are computed on the same observations. The plots below
# still show the full main-sample panel (GDP lines unbroken).
within_sd <- dat |>
  dplyr::filter(!is.na(default_rate)) |>
  dplyr::group_by(iso2) |>
  dplyr::mutate(
    dr_dm  = default_rate    - mean(default_rate),
    gdp_dm = gdp_growth_yoy  - mean(gdp_growth_yoy)
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    sd_dr  = sd(dr_dm),
    sd_gdp = sd(gdp_dm)
  )

med_dr <- dat |>
  dplyr::group_by(quarter_date) |>
  dplyr::summarise(v = median(default_rate, na.rm = TRUE), .groups = "drop")

med_gdp <- dat |>
  dplyr::group_by(quarter_date) |>
  dplyr::summarise(v = median(gdp_growth_yoy, na.rm = TRUE), .groups = "drop")

covid_band <- annotate("rect",
  xmin = COVID_START, xmax = COVID_END, ymin = -Inf, ymax = Inf,
  fill = "#d62728", alpha = 0.07
)

N_MAIN <- dplyr::n_distinct(dat$iso2)

p1a <- ggplot(dat, aes(quarter_date, default_rate, group = iso2)) +
  covid_band +
  geom_line(colour = "grey65", linewidth = 0.3, alpha = 0.55, na.rm = TRUE) +
  geom_line(data = med_dr, aes(quarter_date, v, group = 1),
            colour = "black", linewidth = 1.1, na.rm = TRUE) +
  labs(
    title    = "The default rate moves slowly and within a narrow band",
    # Country count read off the data, like SAMPLE_LABEL above: a hand-written
    # "26 countries" is the same failure mode as a hand-written coefficient.
    subtitle = paste0(
      "Corporate default rate, ", N_MAIN, " countries (grey) and cross-country median (black). ",
      "Within-country sd = ", sprintf("%.2f", within_sd$sd_dr), " pp"
    ),
    x = NULL, y = "Default rate (pp)"
  )

p1b <- ggplot(dat, aes(quarter_date, gdp_growth_yoy, group = iso2)) +
  covid_band +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(colour = "grey65", linewidth = 0.3, alpha = 0.55, na.rm = TRUE) +
  geom_line(data = med_gdp, aes(quarter_date, v, group = 1),
            colour = "#1f77b4", linewidth = 1.1, na.rm = TRUE) +
  labs(
    subtitle = paste0(
      "Real GDP growth YoY, same countries and median (blue). ",
      "Within-country sd = ", sprintf("%.2f", within_sd$sd_gdp), " pp",
      "  —  ", sprintf("%.0f", within_sd$sd_gdp / within_sd$sd_dr),
      "x the variation of the default rate"
    ),
    x = NULL, y = "Real GDP growth, YoY (pp)",
    caption = "Shaded: 2020Q1-2021Q4. Source: EBA Credit Risk Parameters Annex, Eurostat."
  )

fig1 <- p1a / p1b
ggsave(file.path(PATH_FIGURES, "fig1_data_tension.png"), fig1,
       width = 9, height = 6.5, dpi = 300)
message("Saved fig1_data_tension.png")

# =============================================================================
# FIGURE 2: the common-slope result is not a modelling choice
# =============================================================================
# Message: beta_GDP sits on top of zero in every specification we ran. This is
# the antidote to the reading that the heterogeneous-slope section is an attempt
# to rescue GDP after the baseline failed. It replaces the bulk of the 10-column
# robustness table.

spec_ci <- function(model, coef_name, label) {
  ci <- confint(model)
  i  <- which(rownames(ci) == coef_name)
  stopifnot(length(i) == 1)
  tibble::tibble(
    label = label,
    beta  = unname(coef(model)[coef_name]),
    lo    = ci[i, 1],
    hi    = ci[i, 2],
    n     = model$nobs
  )
}

# CR2 (Bell-McCaffrey) on the FE baseline: same point estimate, different SE.
cr2_fe  <- as.data.frame(dg$cr2$fe)
cr2_row <- cr2_fe[cr2_fe$Coef == "gdp_growth_yoy", ]
cr2_ci  <- tibble::tibble(
  label = "Country FE, CR2 small-G standard errors",
  beta  = cr2_row$beta,
  lo    = cr2_row$beta - qt(0.975, cr2_row$df_Satt) * cr2_row$SE,
  hi    = cr2_row$beta + qt(0.975, cr2_row$df_Satt) * cr2_row$SE,
  n     = rb$baseline$nobs
)

stability <- dplyr::bind_rows(
  spec_ci(base$pooled,      "gdp_growth_yoy", "Pooled OLS (benchmark)"),
  spec_ci(rb$baseline,      "gdp_growth_yoy", "Country FE (baseline)"),
  cr2_ci,
  spec_ci(rb$covid,         "gdp_growth_yoy", "Country FE + COVID dummy"),
  spec_ci(rb$no_covid,      "gdp_growth_yoy", "Country FE, COVID period excluded"),
  spec_ci(rb$time_fe,       "gdp_growth_yoy", "Country FE + time FE"),
  spec_ci(base$fe_l1,       "gdp_l1",         "Country FE, GDP lagged one quarter"),
  spec_ci(sh$fe_window$l2,  "gdp_l2",         "Country FE, GDP lagged two quarters"),
  # This is the one specification in the whole lineup that rejects, and it
  # rejects with the wrong sign.
  #
  # It is included precisely because it is inconvenient. A figure captioned
  # "every interval covers zero" that quietly omitted the one interval that does
  # not would be showing less than the evidence, and the text discusses this
  # exception anyway. Better that the figure and the text agree.
  spec_ci(sh$fe_window$l4,  "gdp_l4",         "Country FE, GDP lagged four quarters"),
  spec_ci(base$fe_dl,       "gdp_growth_yoy", "Country FE, distributed lag (L0+L1)"),
  spec_ci(rb$pref,          "gdp_growth_yoy", "Country FE, GDP + inflation (L1)"),
  spec_ci(rb$no_pt,         "gdp_growth_yoy", "Country FE, Portugal dropped"),
  spec_ci(rb$no_peri,       "gdp_growth_yoy", "Country FE, periphery dropped"),
  spec_ci(sh$fe_shared,     "gdp_ma4",        "Country FE, four-quarter average GDP"),
  # The two specifications that remove the common time path: one by fitting a
  # trend, one by dropping the overlapping windows. They are quoted in the text,
  # so they belong on the figure too. Same principle as above: the figure should
  # not look narrower than the evidence standing behind it.
  spec_ci(rb$trend,         "gdp_growth_yoy", "Country FE + common linear trend"),
  spec_ci(rb$q4_only,       "gdp_growth_yoy", "Country FE, Q4-only (non-overlapping)")
) |>
  dplyr::mutate(
    label   = factor(label, levels = rev(label)),
    covers0 = lo < 0 & hi > 0
  )

n_cover <- sum(stability$covers0)
excepts <- stability |> dplyr::filter(!covers0)

fig2 <- ggplot(stability, aes(beta, label)) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = "grey45",
                 linewidth = 0.5) +
  geom_point(aes(shape = covers0), size = 2.2, colour = "black", fill = "white") +
  scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 21), guide = "none") +
  geom_text(aes(label = sprintf("%+.3f", beta)), vjust = -1.0, size = 2.9,
            colour = "grey25") +
  labs(
    title    = "Under a common slope, GDP sensitivity is indistinguishable from zero",
    subtitle = paste0("Coefficient on GDP growth, 95% confidence interval. Standard errors ",
                      "clustered by country.\n", n_cover, " of ", nrow(stability),
                      " intervals cover zero."),
    x        = "beta on GDP growth (pp of default rate per pp of GDP growth)",
    y        = NULL,
    # Written from the data rather than typed. If a future sample makes one of
    # these significant, the caption will say so, instead of going on claiming a
    # clean sweep that no longer holds.
    caption  = paste0(
      "Main sample: ", SAMPLE_LABEL, ". ",
      if (nrow(excepts) == 0) "Every interval covers zero." else paste0(
        "Open circle: the ", nrow(excepts), " specification",
        if (nrow(excepts) > 1) "s that reject" else " that rejects", " — ",
        paste(sprintf("%s (%+.3f)", excepts$label, excepts$beta), collapse = "; "),
        ".\nThe sign is positive, i.e. the opposite of the macro-sensitivity prior; ",
        "see the report, Section 7.1.")
    )
  ) +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(PATH_FIGURES, "fig2_gdp_stability.png"), fig2,
       width = 9, height = 5.5, dpi = 300)
message("Saved fig2_gdp_stability.png")

# =============================================================================
# FIGURE 3: the four estimators do not agree
# =============================================================================
# This figure exists because the central finding was only visible in a table.
#
# The finding is that the four estimators all cover zero AND do not agree with
# each other, either on a point estimate or on the sign of the average. A reader
# skimming the figures would have seen dispersion across countries (fig 4) and
# a stable near-zero common slope (fig 2), and would have missed the
# disagreement between estimators entirely, which is the part that matters
# most.
#
# How to read the rows. Every row uses the same sample and the same
# specification. What changes is the set of assumptions named on the left: slope
# homogeneity, weighting, and whether countries are treated as independent.
#
# One caveat about interpretation: because those assumptions differ in more than
# one respect, the rows are related but not identical objects. The comparison
# shows that the answer depends on the assumptions; it does not isolate the
# effect of aggregation alone.

est_order <- c("Country FE (MA4)", "Mean Group", "Swamy RCM", "CCE-MG")
stopifnot(setequal(sh$comparison$estimator, est_order))

est_range <- sh$comparison |>
  dplyr::mutate(
    estimator = factor(estimator, levels = rev(est_order)),
    flag      = ifelse(p < 0.10, "p < 0.10", "p >= 0.10")
  )

span_lo <- min(est_range$beta)
span_hi <- max(est_range$beta)

# The clause about signs is derived from the numbers, not asserted.
#
# The hand-written version read "the two free-slope averages do not even agree
# on the sign". It was wrong twice over: there are three free-slope rows, not
# two, and the sentence would have carried on printing on a sample where they
# did agree.
free_slope <- sh$comparison$beta[sh$comparison$estimator != "Country FE (MA4)"]
sign_clause <- if (length(unique(sign(free_slope))) > 1) {
  " — the free-slope averages do not even agree on the sign."
} else {
  " — the free-slope averages share a sign but not a magnitude."
}

# Same treatment for the "every interval covers zero" clause: counted, not claimed.
n_cover_est <- sum(sh$comparison$ci_lo < 0 & sh$comparison$ci_hi > 0)
cover_clause <- if (n_cover_est == nrow(sh$comparison)) {
  "Every interval covers zero, and the point estimates span\n"
} else {
  sprintf("%d of %d intervals cover zero, and the point estimates span\n",
          n_cover_est, nrow(sh$comparison))
}

fig3 <- ggplot(est_range, aes(beta, estimator)) +
  annotate("rect", xmin = span_lo, xmax = span_hi, ymin = -Inf, ymax = Inf,
           fill = "#1f77b4", alpha = 0.08) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0, colour = "grey45",
                 linewidth = 0.5) +
  geom_point(aes(shape = flag), size = 2.4, colour = "black", fill = "white") +
  scale_shape_manual(values = c("p < 0.10" = 19, "p >= 0.10" = 21), name = NULL) +
  geom_text(aes(label = sprintf("%+.4f", beta)), vjust = -1.1, size = 2.9,
            colour = "grey25") +
  labs(
    title    = "Same sample, same specification, four sets of maintained assumptions",
    subtitle = paste0(
      "Average GDP coefficient with 95% interval. ", cover_clause,
      sprintf("%+.4f", span_lo), " to ", sprintf("%+.4f", span_hi), sign_clause
    ),
    x = "beta on GDP growth, four-quarter average (pp of default rate per pp of GDP growth)",
    y = NULL,
    caption = paste0(
      "Main sample: ", SAMPLE_LABEL, ". Shaded: the span of the four point estimates, which is ",
      "model uncertainty, not a confidence band.\nCCE-MG is the only row consistent with the ",
      "Pesaran CD test and also the least stable (leave-one-country-out ",
      sprintf("%+.3f to %+.3f", min(sh$ccemg_loo), max(sh$ccemg_loo)), ")."
    )
  ) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())

ggsave(file.path(PATH_FIGURES, "fig3_estimator_range.png"), fig3,
       width = 9, height = 4.6, dpi = 300)
message("Saved fig3_estimator_range.png")

# =============================================================================
# FIGURE 4: one European multiplier vs 26 jurisdictions
# =============================================================================
# The practical point of the whole analysis, in one picture.
#
# A single pooled coefficient turns the same macro deterioration into ONE number
# for every country. The country-specific slopes spread that same shock over an
# interquartile range an order of magnitude wider. If you were using this for
# stress testing, that difference is the whole story.
#
# The exact figures are computed below and printed in the subtitle rather than
# quoted in this comment, because they moved once already when the sample was
# extended.
#
# Figure 3 shows the same dispersion in coefficient space. This one puts it in
# the units a stress test is actually read in: percentage points of default rate.

bc <- st$by_country |>
  dplyr::mutate(iso2 = forcats::fct_reorder(iso2, delta_dr))

q  <- quantile(bc$delta_dr, c(0.25, 0.5, 0.75))
sc <- st$scenario

fe_delta    <- sc$delta_dr[sc$estimator == "Country FE (MA4)"]
mg_delta    <- sc$delta_dr[sc$estimator == "Mean Group"]
swamy_delta <- sc$delta_dr[sc$estimator == "Swamy RCM"]
cce_delta   <- sc$delta_dr[sc$estimator == "CCE-MG"]

# Flag the least precisely estimated slopes so the tails are not over-read.
imprecise <- bc |>
  dplyr::slice_max(se_hac, n = 3) |>
  dplyr::pull(iso2) |>
  as.character()

bc <- bc |>
  dplyr::mutate(precision = ifelse(iso2 %in% imprecise,
                                   "least precisely estimated",
                                   "remaining countries"))

# Clipping the axis, carefully.
#
# The widest confidence intervals run past +/- 3 pp. Showing them in full would
# compress the region where most countries actually sit into an unreadable
# sliver. So the axis is clipped to the range of the POINT ESTIMATES plus a
# small margin.
#
# Clipping a plot is a way to mislead, so two rules apply, and the previously
# hand-set limits broke both:
#
#   - Every point estimate must be inside the frame. This is asserted below,
#     not eyeballed. The old constants (-1.45, 1.05) cut below Bulgaria's
#     -1.53 pp, the most extreme implied effect in the panel; only ggplot's
#     default 5% expansion kept it on the canvas at all.
#
#   - The caption must name the intervals that genuinely leave the frame, on
#     BOTH sides. The old caption named three countries and mentioned only the
#     upper bound, when seven intervals ran off the plot and the binding cut
#     was the lower one.
pad    <- 0.08 * diff(range(bc$delta_dr))
x_lo   <- min(bc$delta_dr) - pad
x_hi   <- max(bc$delta_dr) + pad
n_ctry <- nrow(bc)

stopifnot(all(bc$delta_dr >= x_lo & bc$delta_dr <= x_hi))

clipped <- bc |>
  dplyr::filter(ci_lo_d < x_lo | ci_hi_d > x_hi) |>
  dplyr::arrange(dplyr::desc(abs(delta_dr)))
clipped_iso <- as.character(clipped$iso2)

fig4 <- ggplot(bc, aes(delta_dr, iso2)) +
  annotate("rect", xmin = q[1], xmax = q[3], ymin = -Inf, ymax = Inf,
           fill = "#1f77b4", alpha = 0.10) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_vline(xintercept = fe_delta, colour = "#d62728", linewidth = 0.7) +
  geom_vline(xintercept = mg_delta, colour = "#1f77b4", linewidth = 0.7,
             linetype = "dashed") +
  geom_errorbar(aes(xmin = ci_lo_d, xmax = ci_hi_d), orientation = "y",
                width = 0, colour = "grey60", linewidth = 0.4) +
  geom_point(aes(shape = precision), size = 2.1, colour = "black", fill = "white") +
  scale_shape_manual(
    values = c("least precisely estimated" = 21, "remaining countries" = 19),
    name = NULL
  ) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2.4))) +
  annotate("text", x = fe_delta - 0.03, y = n_ctry + 1.9,
           label = paste0("Single common slope (", signed(fe_delta), " pp)"),
           colour = "#d62728", hjust = 1, size = 3) +
  annotate("text", x = mg_delta + 0.03, y = n_ctry + 0.9,
           label = paste0("Mean Group (", signed(mg_delta), " pp)"),
           colour = "#1f77b4", hjust = 0, size = 3) +
  coord_cartesian(xlim = c(x_lo, x_hi)) +
  labs(
    title    = "A single European multiplier hides the jurisdictions inside it",
    subtitle = paste0(
      "Implied change in the corporate default rate if the four-quarter average of annual GDP growth ",
      "falls by 5 pp.\nShaded: interquartile range across countries (",
      sprintf("%+.2f", q[1]), " to ", sprintf("%+.2f", q[3]), " pp). Median ",
      sprintf("%+.2f", q[2]), " pp. Bars are 95% intervals."
    ),
    x = "Change in default rate (pp)", y = NULL,
    caption = paste0(
      "Point-estimate illustration, not a forecast. Swamy implies ",
      signed(swamy_delta), " pp and CCE-MG ", signed(cce_delta),
      " pp on average; the range across\nestimators is model uncertainty, not a ",
      "confidence band. Open circles: the three least precisely estimated slopes (",
      paste(imprecise, collapse = ", "), ").\n",
      "Every point estimate is inside the frame; the extremes are ",
      as.character(bc$iso2[which.min(bc$delta_dr)]), " at ", signed(min(bc$delta_dr)),
      " pp and ", as.character(bc$iso2[which.max(bc$delta_dr)]), " at ",
      signed(max(bc$delta_dr)), " pp.\nAxis clipped at ", signed(x_lo), " / ",
      signed(x_hi), " pp: the intervals for ", paste(clipped_iso, collapse = ", "),
      " run beyond it."
    )
  ) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        plot.caption = element_text(hjust = 0, size = 7.5))

ggsave(file.path(PATH_FIGURES, "fig4_stress_dispersion.png"), fig4,
       width = 9, height = 7.4, dpi = 300)
message("Saved fig4_stress_dispersion.png")

# =============================================================================
# TABLE: main results table (the one table of the write-up)
# =============================================================================
# Four columns, GDP only.
#
# Inflation is deliberately not in this table, and the reason should be stated
# rather than left to inference. Under the MA4 specification the FE inflation
# coefficient is borderline at the 5% level. Putting it in the headline table
# would give it more weight than the full evidence supports, since the baseline
# and robustness tables show the relationship does not hold up once the common
# time path is controlled for.
#
# It is not hidden: inflation is reported in those tables, where it can be read
# alongside the specifications that undo it.

main_tab <- sh$comparison |>
  dplyr::transmute(
    Estimator      = estimator,
    Assumption     = dplyr::recode(assumption,
      "common slope"                = "single common slope",
      "free slopes, simple mean"    = "country-specific slopes, equal country weights",
      "free slopes, GLS weighted"   = "country-specific slopes, precision weighting",
      "free slopes + common factor" = "country-specific slopes, common factor removed"),
    `GDP beta`     = sprintf("%.3f", beta),
    `Std. error`   = sprintf("%.3f", se),
    `p-value`      = sprintf("%.3f", p),
    # Derived from the estimates, not hand-written. An earlier version of this
    # column labelled the rows "approximately zero / negative but imprecise /
    # supporting negative estimate". On the current sample the Swamy estimate is
    # positive, so those fixed labels would have printed a sign the table right
    # next to them contradicts.
    Interpretation = dplyr::case_when(
      p < 0.05 & beta < 0 ~ "negative, significant at 5%",
      p < 0.05 & beta > 0 ~ "positive, significant at 5%",
      abs(beta) < 0.01    ~ "indistinguishable from zero",
      beta < 0            ~ "negative but imprecise",
      TRUE                ~ "positive but imprecise"
    )
  )

main_note <- paste0(
  "All ", nrow(sh$comparison), " estimators use the same estimation sample (N = ",
  sh$comparison$N[1], ", ", sh$comparison$countries[1], " countries) and the same ",
  "specification: the default rate on a four-quarter average of annual GDP growth and ",
  "inflation lagged one quarter. Country FE standard errors clustered by country; ",
  "intervals use the t critical value with G-1 degrees of freedom. The rows differ in ",
  "their maintained assumptions about slope homogeneity, weighting and common-factor ",
  "dependence, so they estimate related but not identical objects: none yields a ",
  "precisely estimated average GDP sensitivity at the 5% level, and they do not ",
  "agree on a point estimate or on the sign of the average. CCE-MG augments each country ",
  "regression with cross-section averages and is the only row that addresses the ",
  "cross-sectional dependence flagged by the Pesaran CD test, which rejects on this sample; it is ",
  "also the least stable point estimate (leave-one-country-out range ",
  sprintf("%+.3f to %+.3f", min(sh$ccemg_loo), max(sh$ccemg_loo)),
  "), so it widens the reported uncertainty rather than settling the question."
)

# save_kable() shells out to pandoc, which is not on the PATH outside RStudio,
# so it works when you run this interactively and fails from the command line.
# Writing the rendered HTML string directly removes the dependency entirely.
writeLines(
  as.character(
    kableExtra::kbl(main_tab, format = "latex", booktabs = TRUE,
                    caption = main_note) |>
      kableExtra::kable_styling(latex_options = "scale_down")
  ),
  file.path(PATH_TABLES, "table_main_estimators.tex")
)

writeLines(
  as.character(
    kableExtra::kbl(main_tab, format = "html", caption = main_note) |>
      kableExtra::kable_styling(full_width = FALSE)
  ),
  file.path(PATH_TABLES, "table_main_estimators.html")
)

message("Saved table_main_estimators.{tex,html}")
print(main_tab)

# =============================================================================
# TABLE: robustness, trimmed from 10 specifications to 5
# =============================================================================

rob_models <- list(
  "(1) Baseline FE"       = rb$baseline,
  "(2) + COVID dummy"     = rb$covid,
  "(3) COVID excluded"    = rb$no_covid,
  "(4) + Time FE"         = rb$time_fe,
  "(5) Periphery dropped" = rb$no_peri
)

# Which columns the inflation coefficient survives, plus the two within-R2
# figures, all read off the fitted models rather than typed into the caption.
# The surviving columns are identified from the p-values, so if a future sample
# flips one of them the note follows automatically instead of going on
# describing a pattern that no longer holds.
inf_p       <- vapply(rob_models, function(m) unname(m$coeftable["inflation_yoy", "Pr(>|t|)"]), numeric(1))
inf_survive <- names(inf_p)[inf_p < 0.05 & names(inf_p) != "(1) Baseline FE"]
inf_die     <- names(inf_p)[inf_p >= 0.05]
wr2_base    <- fixest::r2(rb$baseline, "wr2")
wr2_time    <- fixest::r2(rb$time_fe,  "wr2")

rob_note <- list(
  "Dependent variable: corporate default rate (pp). Standard errors clustered by country.",
  paste0("The GDP coefficient stays near zero in every column. The inflation ",
         "coefficient survives ", paste(inf_survive, collapse = ", "),
         if (length(inf_die)) paste0(" and collapses under ", paste(inf_die, collapse = ", ")) else "",
         ", where within-R2 falls from ", sprintf("%.2f", wr2_base), " to ",
         sprintf("%.2f", wr2_time), ". Read as level co-movement with a shared time ",
         "path rather than within-quarter sensitivity: not robust, not spurious."),
  "Full set of specifications, alternative standard errors and the logit and recovery specifications are in the appendix."
)

modelsummary::modelsummary(
  rob_models,
  coef_map = c(
    gdp_growth_yoy = "GDP growth YoY",
    inflation_yoy  = "HICP inflation YoY",
    covid          = "COVID (2020Q1-2021Q4)"
  ),
  gof_map = c("nobs", "r.squared", "r2.within"),
  stars   = c("*" = .1, "**" = .05, "***" = .01),
  # A worked example of why nothing in a caption is typed by hand.
  #
  # This note used to read "the inflation coefficient does not survive any of
  # them". That was true on the earlier 25-quarter sample. On the current sample
  # it is contradicted by the very table it sits under: inflation survives
  # columns 2, 3 and 5, and fails only under the common time control in column 4.
  #
  # And that distinction is the entire inflation argument. "It never survives"
  # and "it survives everything except a common time control" point to different
  # conclusions. The second one says the relationship is shared trend rather
  # than fragility.
  #
  # So the note is generated from what the columns actually show, including the
  # two within-R2 figures, which were the last hand-typed numbers left in a
  # caption anywhere in the project.
  notes   = rob_note,
  output  = file.path(PATH_TABLES, "table_robustness_main.tex")
)

modelsummary::modelsummary(
  rob_models,
  coef_map = c(
    gdp_growth_yoy = "GDP growth YoY",
    inflation_yoy  = "HICP inflation YoY",
    covid          = "COVID (2020Q1-2021Q4)"
  ),
  gof_map = c("nobs", "r.squared", "r2.within"),
  stars   = c("*" = .1, "**" = .05, "***" = .01),
  # Same note as the .tex: the two formats are the same table and must not
  # disagree about what the columns show.
  notes   = rob_note,
  output  = file.path(PATH_TABLES, "table_robustness_main.html")
)

message("Saved table_robustness_main.{tex,html}")

# =============================================================================
# Numbers the write-up quotes in prose
# =============================================================================
# This is where the rule stated in the header is enforced.
#
# Everything the write-up says in words is computed here and saved to
# report_numbers.rds. The prose reads those values; it never restates them.
#
# Several bugs during development came from hand-written constants that kept
# asserting an earlier sample's conclusion right next to a number contradicting
# it. Proof-reading did not catch them, because each sentence was individually
# plausible. The fix is structural rather than editorial: one place computes,
# and the write-up only formats.

# --- the GDP coefficient across every level specification of the default rate --
gdp_specs <- dplyr::tribble(
  ~label,                                    ~model,             ~coef,
  "FE baseline (GDP L0)",                    rb$baseline,        "gdp_growth_yoy",
  "FE + COVID dummy",                        rb$covid,           "gdp_growth_yoy",
  "FE, COVID excluded",                      rb$no_covid,        "gdp_growth_yoy",
  "FE + time FE",                            rb$time_fe,         "gdp_growth_yoy",
  "FE + common linear trend",                rb$trend,           "gdp_growth_yoy",
  "FE, Portugal dropped",                    rb$no_pt,           "gdp_growth_yoy",
  "FE, periphery dropped",                   rb$no_peri,         "gdp_growth_yoy",
  "FE, GDP L1",                              base$fe_l1,         "gdp_l1",
  "FE, distributed lag L0+L1",               base$fe_dl,         "gdp_growth_yoy",
  "FE, GDP L2",                              sh$fe_window$l2,    "gdp_l2",
  "FE, GDP L4",                              sh$fe_window$l4,    "gdp_l4",
  "FE, GDP + inflation L1",                  rb$pref,            "gdp_growth_yoy",
  "FE, GDP + inflation L1 + COVID",          rb$pref_covid,      "gdp_growth_yoy",
  "FE, GDP + inflation L1 + time FE",        rb$pref_timefe,     "gdp_growth_yoy",
  "FE, GDP L0 + L1 + inflation L1",          rb$hicp_l1only,     "gdp_growth_yoy",
  "FE, GDP MA4 + inflation L1",              rb$ma4_l1,          "gdp_ma4",
  "FE, GDP MA4 + inflation MA4",             rb$ma4_ma4,         "gdp_ma4",
  "FE, Q4-only (non-overlapping)",           rb$q4_only,         "gdp_growth_yoy",
  "FE, Q4-only, GDP MA4",                    rb$q4_only_ma4,     "gdp_ma4",
  "FE, pre-2020 subsample",                  rb$pre2020,         "gdp_growth_yoy",
  "FE, post-2020 subsample",                 rb$post2020,        "gdp_growth_yoy"
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    beta = unname(coef(model)[coef]),
    se   = unname(model$coeftable[coef, "Std. Error"]),
    p    = unname(model$coeftable[coef, "Pr(>|t|)"]),
    N    = model$nobs
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-model)

# One specification in the set does reject at 5%: GDP lagged four quarters, and
# with a POSITIVE sign, the opposite of what the credit-risk story would
# predict.
#
# It is reported rather than smoothed over. A write-up claiming "not significant
# in any specification" would be contradicted by its own appendix table, and a
# reader who noticed would be right to distrust everything around it.
gdp_sig <- gdp_specs |> dplyr::filter(p < 0.05)

# --- the same coefficient outside the level specification ---------------------
aux_specs <- dplyr::tribble(
  ~label,                          ~model,           ~coef,
  "logit(DR)",                     rb$logit,         "gdp_growth_yoy",
  "logit(DR) + COVID",             rb$logit_covid,   "gdp_growth_yoy",
  "First differences",             rb$fd,            "d_gdp",
  "First differences + time FE",   rb$fd_time,       "d_gdp",
  "First differences, MA4 spec",   rb$fd_ma4,        "d_gdp_ma4",
  "LGD-implied recovery proxy",    rb$recovery,      "gdp_growth_yoy",
  "Loss-rate recovery proxy",      rb$recovery_lr,   "gdp_growth_yoy",
  "Loss-rate proxy + COVID",       rb$recovery_lr_cv,"gdp_growth_yoy"
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    beta = unname(coef(model)[coef]),
    p    = unname(model$coeftable[coef, "Pr(>|t|)"]),
    N    = model$nobs
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-model)

# --- sample construction, read off the panel rather than typed ----------------
cov_by_country <- panel |>
  dplyr::group_by(iso2) |>
  dplyr::summarise(n_valid  = sum(!is.na(default_rate)),
                   coverage = dplyr::first(coverage_pct),
                   passes   = dplyr::first(passes_coverage),
                   main     = dplyr::first(is_main_sample), .groups = "drop")

dropped_cov <- cov_by_country |> dplyr::filter(!passes) |> dplyr::arrange(coverage)
dropped_dq  <- cov_by_country |> dplyr::filter(passes, !main)

t_per_country <- dat |>
  dplyr::filter(!is.na(default_rate)) |>
  dplyr::count(iso2)

# --- period descriptives: the shared time path the inflation argument rests on -
periods <- dat |>
  dplyr::mutate(period = dplyr::case_when(
    quarter_date <  as.Date("2020-01-01") ~ "pre-COVID 2015Q4-2019Q4",
    quarter_date <= as.Date("2021-10-01") ~ "COVID 2020Q1-2021Q4",
    TRUE                                  ~ "post-COVID 2022Q1-2026Q1")) |>
  dplyr::group_by(period) |>
  dplyr::summarise(quarters = dplyr::n_distinct(quarter),
                   obs      = sum(!is.na(default_rate)),
                   dr       = mean(default_rate, na.rm = TRUE),
                   gdp      = mean(gdp_growth_yoy),
                   inf      = mean(inflation_yoy), .groups = "drop") |>
  dplyr::arrange(match(period, c("pre-COVID 2015Q4-2019Q4", "COVID 2020Q1-2021Q4",
                                 "post-COVID 2022Q1-2026Q1")))

# --- raw correlations quoted in the data section -------------------------------
est <- dat |> dplyr::filter(!is.na(default_rate))
cors <- list(
  dr_gdp    = cor(est$default_rate, est$gdp_growth_yoy),
  dr_inf    = cor(est$default_rate, est$inflation_yoy),
  dr_gdpma4 = cor(est$default_rate, est$gdp_ma4),
  inf_inf1  = dg$corr_mat["inflation_yoy", "inf_l1"]
)

# --- FE weight decomposition, top-5 concentration -----------------------------
w <- sh$weights |> dplyr::arrange(dplyr::desc(w_fe))
conc <- list(
  top5      = sum(w$w_fe[1:5]),
  top5_iso  = paste(w$iso2[1:5], collapse = ", "),
  pos_share = sum(w$w_fe[w$beta > 0]),
  n_pos     = sum(w$beta > 0),
  n_neg     = sum(w$beta < 0)
)

# --- minimum detectable effect, on the MG standard error ----------------------
se_mg   <- sh$comparison$se[sh$comparison$estimator == "Mean Group"]
mde     <- (qnorm(0.975) + qnorm(0.80)) * se_mg
cce_beta <- sh$comparison$beta[sh$comparison$estimator == "CCE-MG"]

report_numbers <- list(
  sample = list(
    countries      = dplyr::n_distinct(dat$iso2),
    quarters       = dplyr::n_distinct(dat$quarter),
    q_first        = min(dat$quarter),
    q_last         = max(dat$quarter),
    N              = sum(!is.na(dat$default_rate)),
    dr_missing     = sum(is.na(dat$default_rate)),
    panel_rows     = nrow(panel),
    panel_countries= dplyr::n_distinct(panel$iso2),
    t_min          = min(t_per_country$n),
    t_max          = max(t_per_country$n),
    t_median       = median(t_per_country$n),
    dr_mean        = st$dr_mean,
    label          = SAMPLE_LABEL,
    dropped_cov    = dropped_cov,
    dropped_dq     = dropped_dq
  ),
  data       = list(within_sd_dr = within_sd$sd_dr, within_sd_gdp = within_sd$sd_gdp,
                    sd_ratio = within_sd$sd_gdp / within_sd$sd_dr,
                    cors = cors, periods = periods),
  gdp_specs  = gdp_specs,
  gdp_sig    = gdp_sig,
  aux_specs  = aux_specs,
  fig2       = list(n_specs = nrow(stability), n_cover = n_cover,
                    all_cover_zero = all(stability$covers0),
                    exceptions = excepts$label),
  het        = list(plain = sh$het_main, ols = sh$het_ols, cce = sh$het_cce,
                    by_lag = sh$het_by_lag,
                    n_neg = sum(sh$slopes$beta < 0), n_total = nrow(sh$slopes),
                    median_slope = median(sh$slopes$beta),
                    n_sig10 = sum(sh$slopes$sig10),
                    sig10_iso = paste(sh$slopes$iso2[sh$slopes$sig10], collapse = ", "),
                    cd_country = sh$cd_country, cd_cce = sh$cd_cce,
                    decomp = sh$fe_decomp, conc = conc,
                    loo_lo = min(sh$ccemg_loo), loo_hi = max(sh$ccemg_loo),
                    cce_median = median(sh$ccemg_slopes),
                    cce_n_neg = sum(sh$ccemg_slopes < 0),
                    ar1_median = median(rb$dr_ar1$rho),
                    ar1_min = min(rb$dr_ar1$rho), ar1_max = max(rb$dr_ar1$rho)),
  stress     = list(scenario = st$scenario, delta_gdp = st$delta_gdp,
                    n_up = sum(bc$delta_dr > 0), n_ctry = nrow(bc),
                    median = unname(q[2]), q25 = unname(q[1]), q75 = unname(q[3]),
                    lo_iso = as.character(bc$iso2[which.min(bc$delta_dr)]),
                    hi_iso = as.character(bc$iso2[which.max(bc$delta_dr)]),
                    lo = min(bc$delta_dr), hi = max(bc$delta_dr),
                    imprecise = imprecise,
                    range_lo = min(st$scenario$delta_dr),
                    range_hi = max(st$scenario$delta_dr),
                    tau_band = 1.645 * st$tau * abs(st$delta_gdp),
                    tau_band_cce = 1.645 * sh$het_cce$tau * abs(st$delta_gdp)),
  mde        = list(se_mg = se_mg, beta = mde, dr = mde * abs(st$delta_gdp),
                    cce_dr = abs(cce_beta * st$delta_gdp)),
  # The formatted central table and its note are saved too, not just the raw
  # numbers. That way the write-up renders the exact object the pipeline built,
  # rather than rebuilding the Interpretation column itself and risking a
  # different answer from the same data.
  main_tab   = main_tab,
  main_note  = main_note
)

saveRDS(report_numbers, file.path(PATH_MODELS, "report_numbers.rds"))
message("Saved report_numbers.rds")

cat("\n--- numbers for the write-up ---\n")
cat("Within-country sd, default rate:      ", sprintf("%.2f pp", within_sd$sd_dr), "\n")
cat("Within-country sd, GDP growth:        ", sprintf("%.2f pp", within_sd$sd_gdp), "\n")
cat("Sample mean default rate:             ", sprintf("%.2f pp", st$dr_mean), "\n")
cat("Countries with DR increase under shock:",
    sum(bc$delta_dr > 0), "of", nrow(bc), "\n")
cat("Median country effect:                ", sprintf("%+.2f pp", q[2]), "\n")
cat("IQR of country effects:               ", sprintf("%+.2f to %+.2f pp", q[1], q[3]), "\n")
cat("Least precisely estimated slopes:     ", paste(imprecise, collapse = ", "), "\n")
cat("Stability CIs covering zero:          ",
    sprintf("%d of %d", n_cover, nrow(stability)), "\n")
cat("Estimator span (GDP beta):            ",
    sprintf("%+.4f to %+.4f", span_lo, span_hi), "\n")
cat("Level specs with p < 0.05:            ",
    if (nrow(gdp_sig) == 0) "none" else
      paste(sprintf("%s (%+.4f, p=%.3f)", gdp_sig$label, gdp_sig$beta, gdp_sig$p),
            collapse = "; "), "\n")
cat("MDE (80% power, 5%):                  ",
    sprintf("%.4f -> %.2f pp under a %+g pp shock", mde, mde * abs(st$delta_gdp),
            st$delta_gdp), "\n")
