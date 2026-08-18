# R/10_stress_scenario.R
#
# STEP 10: What would a recession do to default rates?
#
# Read this header before quoting any number from this script.
#
# This is an illustration, not a forecast and not a causal claim. It takes the
# GDP sensitivities estimated in step 9 and applies them to a hypothetical
# downturn, so the coefficients can be read in units that mean something.
#
# WHAT THE SHOCK ACTUALLY IS
# The heterogeneous-slope models are estimated on gdp_ma4, the FOUR-QUARTER
# MOVING AVERAGE of annual GDP growth. The scenario therefore has to be stated
# in those same terms: the four-quarter average of annual growth deteriorates by
# 5 pp, say from +2 pp to -3 pp.
#
# That is not the same thing as GDP falling 5 pp in one quarter, and it must not
# be written up that way. Describing it as a single-quarter shock would overstate
# how severe the scenario is.
#
# WHAT IS REPORTED, AND WHAT IS NOT
# The output is the estimated CHANGE in the default rate. The sample mean default
# rate is shown alongside it, but only to give a sense of scale.
#
# What is deliberately NOT reported is a "stressed default rate" level. Adding a
# marginal effect to a sample mean is not a prediction from the fitted equation,
# and presenting it as one would give the exercise more authority than it has.
#
# WHICH SENSITIVITIES ARE USED
# All four from step 9. None of them is "the" estimate:
#
#   Country FE  one common slope
#   Mean Group  free slopes, every country counting equally
#   Swamy RCM   free slopes, weighted by precision
#   CCE-MG      free slopes with a common factor removed. The only one
#               consistent with the CD test rejecting, and also the least
#               stable point estimate of the four.
#
# The SPREAD across them is the honest output of this exercise. It represents
# MODEL UNCERTAINTY: how much the answer depends on which assumptions you are
# willing to make. It is not a confidence interval and should not be read as one.
#
# So the exercise produces two things worth quoting: the range of average
# effects across estimators, and the spread across countries. The second is what
# the slope-heterogeneity work in step 9 buys: a stress test that says
# different things about different countries.
#
# Inputs:  output/models/slope_het.rds
# Outputs: output/tables/stress_scenario.{tex,html}
#          output/tables/stress_by_country.csv
#          output/figures/stress_scenario.png

source(here::here("R", "00_setup.R"))

sh <- readRDS(file.path(PATH_MODELS, "slope_het.rds"))

# ================================================================
# Scenario definition
# ================================================================
# Levels of the FOUR-QUARTER AVERAGE of annual GDP growth, in pp.
GDP_BASELINE <-  2.0   # stylised "normal" four-quarter-average growth
GDP_ADVERSE  <- -3.0   # stylised recession, same four-quarter-average basis
DELTA_GDP    <- GDP_ADVERSE - GDP_BASELINE   # -5 pp on the four-quarter average

dat_ma  <- sh$dat_ma
DR_MEAN <- mean(dat_ma$default_rate)

# Latest observed quarter, for a second reference point
last_q  <- max(dat_ma$quarter)
DR_LAST <- mean(dat_ma$default_rate[dat_ma$quarter == last_q])

cat("=== SCENARIO ===\n")
cat(sprintf("  Four-quarter average of annual GDP growth: %+.1f pp -> %+.1f pp  (Delta = %+.1f pp)\n",
            GDP_BASELINE, GDP_ADVERSE, DELTA_GDP))
cat(sprintf("  Sample mean DR (reference only):  %.2f pp\n", DR_MEAN))
cat(sprintf("  Mean DR in %s (reference):   %.2f pp\n", last_q, DR_LAST))

# ================================================================
# Average effect under each estimator
# ================================================================
scen <- sh$comparison |>
  dplyr::mutate(
    delta_dr    = beta * DELTA_GDP,
    delta_lo    = ci_hi * DELTA_GDP,   # sign flips: shock is negative
    delta_hi    = ci_lo * DELTA_GDP,
    pct_of_mean = 100 * delta_dr / DR_MEAN
  ) |>
  dplyr::select(estimator, assumption, beta, se, p,
                delta_dr, delta_lo, delta_hi, pct_of_mean)

cat("\n=== AVERAGE EFFECT — Delta DR (pp) for a -5pp shock to the 4Q-average GDP growth ===\n")
print(as.data.frame(scen), row.names = FALSE, digits = 3)

mg <- scen |> dplyr::filter(estimator == "Mean Group")
sw <- scen |> dplyr::filter(estimator == "Swamy RCM")
cc <- scen |> dplyr::filter(estimator == "CCE-MG")

# The direction word is read off the estimate rather than assumed. This is not
# fussiness: on the earlier, shorter sample both estimators implied an increase,
# so "increases by" could safely be hard-coded. On the current sample they
# straddle zero, and a fixed verb would produce the sentence "increases by
# -0.04 pp".
dir_word <- function(x) if (x >= 0) "rises by" else "falls by"

het <- scen |> dplyr::filter(estimator != "Country FE (MA4)")
for (i in seq_len(nrow(het))) {
  cat(sprintf("  %-12s DR %s %.2f pp (%.1f%% of the %.2f pp sample mean).\n",
              het$estimator[i], dir_word(het$delta_dr[i]),
              abs(het$delta_dr[i]), abs(het$pct_of_mean[i]), DR_MEAN))
}
if (length(unique(sign(het$delta_dr))) > 1) {
  cat("  -> the heterogeneous-slope estimators disagree on the SIGN of the average\n")
  cat("     effect. Report the scenario as not pinning down a robust average\n")
  cat("     European response, not as a small positive or negative number.\n")
}
cat(sprintf("Spread across the estimator set: %+.2f to %+.2f pp.\n",
            min(scen$delta_dr), max(scen$delta_dr)))
cat("  -> that spread is model uncertainty, NOT a confidence band.\n")
cat(sprintf("  The widest single implication is %s at %+.2f pp (%.0f%% of the sample\n",
            scen$estimator[which.max(abs(scen$delta_dr))],
            scen$delta_dr[which.max(abs(scen$delta_dr))],
            100 * max(abs(scen$delta_dr)) / DR_MEAN))
cat("  mean). Quoting only the Mean Group figure would understate the range the\n")
cat("  estimator set actually spans.\n")

# ================================================================
# Dispersion across countries: the point of the heterogeneity result
# ================================================================
by_country <- sh$slopes |>
  dplyr::mutate(
    delta_dr = beta * DELTA_GDP,
    ci_lo_d  = ci_hi * DELTA_GDP,
    ci_hi_d  = ci_lo * DELTA_GDP
  ) |>
  dplyr::left_join(
    dat_ma |> dplyr::group_by(iso2) |>
      dplyr::summarise(dr_mean = mean(default_rate), .groups = "drop"),
    by = "iso2"
  ) |>
  dplyr::arrange(dplyr::desc(delta_dr))

# The spread implied by the estimated slope dispersion. Using the
# DerSimonian-Laird tau from step 9 and treating the true country slopes as
# beta_true ~ N(mean, tau^2), 90% of countries fall within mean +/- 1.645*tau.
# This is a spread across countries, not uncertainty about the average.
tau <- sh$het_main$tau
band_lo <- (mg$beta - 1.645 * tau) * DELTA_GDP
band_hi <- (mg$beta + 1.645 * tau) * DELTA_GDP

cat("\n=== DISPERSION ACROSS COUNTRIES ===\n")
cat(sprintf("  Country Delta DR range: %+.2f pp (%s) to %+.2f pp (%s)\n",
            min(by_country$delta_dr), by_country$iso2[nrow(by_country)],
            max(by_country$delta_dr), by_country$iso2[1]))
cat(sprintf("  Median country Delta DR: %.2f pp\n", stats::median(by_country$delta_dr)))
cat(sprintf("  Countries with DR increase: %d / %d\n",
            sum(by_country$delta_dr > 0), nrow(by_country)))
cat(sprintf("  Implied 90%% band from tau=%.4f: %+.2f to %+.2f pp\n",
            tau, min(band_lo, band_hi), max(band_lo, band_hi)))

# The same band, recomputed from the dispersion that survives the common-factor
# augmentation. It comes out much wider, and that is the substantive point: once
# the shared time path is projected out, what remains is MORE country-specific
# variation, not less. The common factor was masking heterogeneity rather than
# creating it.
tau_cce  <- sh$het_cce$tau
band_cce <- sort(c((mg$beta - 1.645 * tau_cce) * DELTA_GDP,
                   (mg$beta + 1.645 * tau_cce) * DELTA_GDP))
cat(sprintf("  Same band from the CCE-augmented tau=%.4f: %+.2f to %+.2f pp\n",
            tau_cce, band_cce[1], band_cce[2]))

# The countries at the extremes of the range are exactly the ones whose slopes
# are least reliable: short series, wide HAC intervals. Quoting the full range
# would therefore be quoting the noise. The interquartile range is reported
# alongside it, and that is what the write-up leans on.
#
# The tail countries are named from the data rather than typed in, because which
# countries sit there depends on the sample.
iqr <- stats::quantile(by_country$delta_dr, c(0.25, 0.75))
cat(sprintf("  Interquartile range of country Delta DR: %+.2f to %+.2f pp\n",
            iqr[1], iqr[2]))

least_precise <- by_country |>
  dplyr::arrange(dplyr::desc(se_hac)) |>
  dplyr::slice_head(n = 3)
cat(sprintf("  Least precise slopes (widest HAC SE): %s — %s of them sit in the\n",
            paste(least_precise$iso2, collapse = ", "),
            sum(abs(least_precise$delta_dr) > max(abs(iqr)))))
cat("  tails of the country range, which is why the IQR is the reported spread.\n")

# ================================================================
# Figure: average effect + country dispersion
# ================================================================
p_est <- scen |>
  dplyr::mutate(estimator = forcats::fct_reorder(estimator, delta_dr)) |>
  ggplot(aes(x = delta_dr, y = estimator)) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_errorbar(aes(xmin = delta_lo, xmax = delta_hi), orientation = "y",
                width = 0, colour = "grey55") +
  geom_point(size = 2.5) +
  labs(title = "Average effect on the corporate default rate",
       subtitle = sprintf(
         "Four-quarter average of annual GDP growth: %+.0f pp -> %+.0f pp (Delta = %+.0f pp). 95%% CI.",
         GDP_BASELINE, GDP_ADVERSE, DELTA_GDP),
       x = "Change in default rate (pp)", y = NULL)

p_ctry <- by_country |>
  dplyr::mutate(iso2 = forcats::fct_reorder(iso2, delta_dr)) |>
  ggplot(aes(x = delta_dr, y = iso2)) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_vline(xintercept = mg$delta_dr, colour = "#1b6ca8", linetype = "dashed") +
  geom_col(aes(fill = delta_dr > 0), width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = "#c0392b", `FALSE` = "#7f8c8d")) +
  labs(title = "Implied effect by country",
       subtitle = sprintf("Country-specific slopes. Dashed = Mean Group (%+.2f pp).",
                          mg$delta_dr),
       x = "Change in default rate (pp)", y = NULL)

p_stress <- p_est / p_ctry + patchwork::plot_layout(heights = c(1, 3))

ggsave(file.path(PATH_FIGURES, "stress_scenario.png"), p_stress,
       width = 8, height = 9, dpi = 300)

# ================================================================
# Tables
# ================================================================
scen_out <- scen |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))) |>
  dplyr::rename(`Delta DR (pp)` = delta_dr,
                `CI low` = delta_lo, `CI high` = delta_hi,
                `% of mean DR` = pct_of_mean)

cap <- sprintf(
  paste0("Stylised stress scenario: the four-quarter average of annual GDP growth ",
         "deteriorates from %+.0f pp to %+.0f pp (Delta = %+.0f pp). Reported as the ",
         "estimated change in the default rate; sample mean DR = %.2f pp is a ",
         "reference for magnitude only. Illustrative, not a forecast; no causal claim. ",
         "The spread across the four estimators reflects model uncertainty, not a ",
         "confidence band; CCE-MG is the only row consistent with the rejected ",
         "Pesaran CD test and also the least stable point estimate."),
  GDP_BASELINE, GDP_ADVERSE, DELTA_GDP, DR_MEAN)

writeLines(
  kableExtra::kbl(scen_out, format = "latex", booktabs = TRUE, caption = cap) |>
    kableExtra::kable_styling(latex_options = "scale_down") |> as.character(),
  file.path(PATH_TABLES, "stress_scenario.tex")
)
writeLines(
  kableExtra::kbl(scen_out, format = "html", caption = cap) |>
    kableExtra::kable_styling() |> as.character(),
  file.path(PATH_TABLES, "stress_scenario.html")
)

readr::write_csv(
  by_country |>
    dplyr::select(iso2, t_eff, beta, se_hac, p_hac, delta_dr, ci_lo_d, ci_hi_d,
                  dr_mean),
  file.path(PATH_TABLES, "stress_by_country.csv")
)

saveRDS(list(scenario = scen, by_country = by_country,
             delta_gdp = DELTA_GDP, dr_mean = DR_MEAN, tau = tau),
        file.path(PATH_MODELS, "stress_scenario.rds"))

# ================================================================
# Write-up sentences
# ================================================================
cat("\n=== SENTENCES FOR THE WRITE-UP ===\n")
cat(sprintf(
"I consider a stylised scenario in which the four-quarter average of annual GDP
growth deteriorates by %.0f percentage points (from %+.0f pp to %+.0f pp). Against a
sample mean default rate of %.2f pp, the estimator set implies changes of %+.2f pp
(Mean Group), %+.2f pp (Swamy) and %+.2f pp (CCE-MG, which projects out a common
factor). None of the underlying slopes is distinguishable from zero at the 5%%
level, and the implied changes do not share a sign, so the exercise does not pin
down a robust average European response to the shock. The range across the four
estimators — %+.2f to %+.2f pp, or up to %.0f%% of the sample mean — is model
uncertainty rather than a confidence band, and it is wider than the Mean Group
figure alone would suggest.\n\n",
  abs(DELTA_GDP), GDP_BASELINE, GDP_ADVERSE, DR_MEAN,
  mg$delta_dr, sw$delta_dr, cc$delta_dr,
  min(scen$delta_dr), max(scen$delta_dr),
  100 * max(abs(scen$delta_dr)) / DR_MEAN))

cat(sprintf(
"What the country-level estimates do show is dispersion: the implied changes have
an interquartile range of %+.2f to %+.2f pp (I2 = %.0f%%, tau = %.4f), and %d of %d
countries move up. The extreme countries are the least precisely estimated slopes
and the write-up does not lean on them. The business reading is that a single
European multiplier is not supported by this sample — not that the multiplier is
small.\n",
  iqr[1], iqr[2], 100 * sh$het_main$I2, sh$het_main$tau,
  sum(by_country$delta_dr > 0), nrow(by_country)))

cat("\nSaved:\n")
cat("  ", file.path(PATH_TABLES,  "stress_scenario.{tex,html}"), "\n")
cat("  ", file.path(PATH_TABLES,  "stress_by_country.csv"), "\n")
cat("  ", file.path(PATH_FIGURES, "stress_scenario.png"), "\n")
cat("  ", file.path(PATH_MODELS,  "stress_scenario.rds"), "\n")
