# R/03b_compat_check.R
#
# STEP 3b: Is the extended panel one series, or two glued together?
#
# The panel runs from 2015Q4 to 2026Q1, but it is not one continuous
# publication. It is stitched together from three: the pre-2020 releases, the
# PDF-only quarters in early 2017, and the 2020-onwards releases. Sticking them
# end to end is only legitimate if the default rate does not jump at the joins
# for reasons that have nothing to do with corporate credit.
#
# There are three joins, of two different kinds.
#
#   2019Q4 -> 2020Q1  The EBA Risk Dashboard switched to an EU27 presentation
#                     around this point. That change affects the dashboard's
#                     headline aggregates, not this by-country annex: the table
#                     title, the regulatory source (COREP C 9.02) and the
#                     ">3 banks reporting" rule are the same in every vintage
#                     from 2015 to 2026. So no break is expected here. But "not
#                     expected" is an argument, not evidence, which is the
#                     reason this script exists.
#
#   2016Q4 -> 2017Q1  These are format joins rather than institutional ones.
#   2017Q2 -> 2017Q3  The quarters on either side come from spreadsheets, the
#                     ones in between from PDFs. Step 2 already showed the two
#                     readers agree cell for cell on 2017Q4, so what is being
#                     tested here is the assembled panel, not the reader.
#
# How it is tested: take the one-quarter change in the default rate, per
# country, at every boundary in the panel. A join counts as a break only if its
# changes sit outside the range the other 38 boundaries already cover.
#
# This is more sensitive than it sounds. Because the default rate is a rolling
# four-quarter figure, ordinary quarter-on-quarter changes are small by
# construction, so a genuine discontinuity would stand out clearly.
#
# The whole thing runs twice. The full 29-country panel is reported for
# completeness, but the number that matters is the second one, restricted to the
# countries the analysis actually uses. A scary-looking join driven entirely by
# a country the coverage filter throws out is not a problem for the estimates.
#
# Nothing here is imputed, adjusted or corrected. The script measures and
# reports, and that is all. It also writes out the country-level detail at each
# join, so a reader can check for themselves which countries move and whether
# they move together. A genuine regime change would push most countries the
# same way, whereas one country jumping on its own is just that country.
#
# Input:  data/clean/panel_country_quarter.csv
# Output: output/tables/compat_seam_jumps.csv        every boundary, both samples
#         output/tables/compat_country_at_seams.csv  country detail at the seams
#         output/figures/compat_seams.png

source(here::here("R", "00_setup.R"))

panel <- readr::read_csv(PATH_PANEL, show_col_types = FALSE)

SEAMS <- c("2017-Q1", "2017-Q3", "2020-Q1")
SEAM_WHY <- c("2017-Q1" = "xlsx -> pdf",
              "2017-Q3" = "pdf -> xlsx",
              "2020-Q1" = "pre-2020 release -> 2020+ release")

quarters <- sort(unique(panel$quarter))

# ---- One-quarter changes ----------------------------------------------------
# Only genuinely consecutive quarters count. If a country has an unpublished
# quarter in the middle, the change across that gap spans two quarters, and
# letting it in would inflate the spread of "normal" changes and make a real
# break at a join easier to hide.

deltas <- panel |>
  dplyr::arrange(iso2, quarter) |>
  dplyr::group_by(iso2) |>
  dplyr::mutate(prev_q  = dplyr::lag(quarter),
                prev_dr = dplyr::lag(default_rate),
                delta   = default_rate - prev_dr) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(delta),
                match(quarter, quarters) - match(prev_q, quarters) == 1L)

cat(sprintf("Usable one-quarter changes: %s across %d boundaries\n",
            format(nrow(deltas), big.mark = ","), dplyr::n_distinct(deltas$quarter)))

# The same coverage filter the analysis layer uses, recomputed here from
# scratch. Duplicating it is deliberate: it means this script can be run on its
# own, straight after the panel is built, without step 4 having run first.
coverage <- panel |>
  dplyr::group_by(iso2) |>
  dplyr::summarise(pct = 100 * mean(!is.na(default_rate)), .groups = "drop")
passes <- sort(coverage$iso2[coverage$pct >= 60])

# ---- Seams against the ordinary boundaries ---------------------------------

seam_report <- function(d, label) {
  per_boundary <- d |>
    dplyr::group_by(quarter) |>
    dplyr::summarise(n_countries = dplyr::n(),
                     mean_delta   = mean(delta),
                     median_delta = stats::median(delta),
                     sd_delta     = stats::sd(delta),
                     max_abs      = max(abs(delta)),
                     share_up     = mean(delta > 0),
                     .groups = "drop") |>
    dplyr::mutate(sample = label,
                  is_seam = quarter %in% SEAMS,
                  seam_why = unname(SEAM_WHY[quarter]))

  ord <- per_boundary |> dplyr::filter(!is_seam)
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("%d ordinary boundaries span: mean delta [%+.3f, %+.3f]  sd [%.3f, %.3f]  share up [%.2f, %.2f]\n",
              nrow(ord), min(ord$mean_delta), max(ord$mean_delta),
              min(ord$sd_delta), max(ord$sd_delta),
              min(ord$share_up), max(ord$share_up)))

  seams <- per_boundary |>
    dplyr::filter(is_seam) |>
    dplyr::mutate(
      # Does each join's statistic fall inside the range the ordinary boundaries
      # already cover?
      #
      # This is a containment check, not a hypothesis test. With 38 boundaries
      # there is not enough data to support anything stronger, so nothing
      # stronger is claimed. What it can tell you is whether a join looks like
      # the other boundaries or stands apart from all of them.
      inside = mean_delta >= min(ord$mean_delta) & mean_delta <= max(ord$mean_delta) &
               sd_delta   >= min(ord$sd_delta)   & sd_delta   <= max(ord$sd_delta) &
               share_up   >= min(ord$share_up)   & share_up   <= max(ord$share_up))

  print(as.data.frame(seams |> dplyr::transmute(
    boundary = quarter, why = seam_why, n = n_countries,
    mean = round(mean_delta, 4), median = round(median_delta, 4),
    sd = round(sd_delta, 4), max_abs = round(max_abs, 3),
    share_up = round(share_up, 2),
    within_ordinary_range = inside)), row.names = FALSE)

  list(per_boundary = per_boundary, all_inside = all(seams$inside))
}

full <- seam_report(deltas, "all 29 countries")
main <- seam_report(dplyr::filter(deltas, iso2 %in% setdiff(passes, "CY")),
                    sprintf("coverage >= 60%% minus CY (%d countries)",
                            length(setdiff(passes, "CY"))))

readr::write_csv(dplyr::bind_rows(full$per_boundary, main$per_boundary),
                 file.path(PATH_TABLES, "compat_seam_jumps.csv"))

cat("\n---------------------------------------------------------------------\n")
if (main$all_inside) {
  cat("VERDICT: on the estimation sample, every seam statistic falls inside the\n",
      "range the ordinary quarter boundaries already span. No evidence of a\n",
      "publication break; the extended panel is treated as one series.\n", sep = "")
} else {
  cat("VERDICT: at least one seam statistic falls outside the ordinary range on\n",
      "the estimation sample. A pre/post-2020 sensitivity check is required\n",
      "before the extended panel is used as the baseline.\n", sep = "")
}
cat("---------------------------------------------------------------------\n")

# ---- Country detail at the seams -------------------------------------------

at_seams <- deltas |>
  dplyr::filter(quarter %in% SEAMS) |>
  dplyr::transmute(iso2, boundary = quarter, before = prev_dr, after = default_rate,
                   delta, in_main_sample = iso2 %in% setdiff(passes, "CY")) |>
  dplyr::arrange(boundary, dplyr::desc(abs(delta)))

readr::write_csv(at_seams, file.path(PATH_TABLES, "compat_country_at_seams.csv"))

for (s in SEAMS) {
  x <- at_seams |> dplyr::filter(boundary == s)
  cat(sprintf("\n--- %s (%s): %d countries, %d up / %d down; five largest moves ---\n",
              s, SEAM_WHY[[s]], nrow(x), sum(x$delta > 0), sum(x$delta < 0)))
  print(as.data.frame(utils::head(x, 5) |>
    dplyr::transmute(iso2, before = round(before, 3), after = round(after, 3),
                     delta = round(delta, 3), in_main_sample)), row.names = FALSE)
}

# Which countries are behind the extreme values. Worth printing, because on the
# full panel the join statistics are largely just describing one or two thin
# countries rather than anything about the panel as a whole.
cat("\n--- Ten largest one-quarter moves anywhere in the panel ---\n")
print(as.data.frame(deltas |> dplyr::arrange(dplyr::desc(abs(delta))) |> utils::head(10) |>
  dplyr::transmute(iso2, quarter, before = round(prev_dr, 2), after = round(default_rate, 2),
                   delta = round(delta, 2),
                   in_main_sample = iso2 %in% setdiff(passes, "CY"))), row.names = FALSE)

# ---- Figure -----------------------------------------------------------------
# Drawn on the estimation sample rather than the full panel. On the full panel
# the Malta and Cyprus outliers stretch the axis so far that every other country
# collapses into a flat line, and the plot stops communicating anything.

plot_d <- deltas |> dplyr::filter(iso2 %in% setdiff(passes, "CY"))

p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = quarter, y = delta)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  ggplot2::geom_vline(xintercept = SEAMS, linetype = "22", colour = "#c0392b", linewidth = 0.5) +
  ggplot2::geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.5, linewidth = 0.3,
                        fill = "grey92", colour = "grey35") +
  ggplot2::labs(
    title = "One-quarter change in the corporate default rate, estimation sample",
    subtitle = paste("Red lines mark the publication seams: 2017Q1 and 2017Q3 (pdf boundaries),",
                     "2020Q1 (pre-2020 to 2020+ releases)"),
    x = NULL, y = "change vs previous quarter (pp)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 6))

ggplot2::ggsave(file.path(PATH_FIGURES, "compat_seams.png"), p,
                width = 11, height = 5, dpi = 300)

cat(sprintf("\nSaved:\n  %s\n  %s\n  %s\n",
            file.path(PATH_TABLES, "compat_seam_jumps.csv"),
            file.path(PATH_TABLES, "compat_country_at_seams.csv"),
            file.path(PATH_FIGURES, "compat_seams.png")))
