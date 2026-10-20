# ========================================
## plot_night() -- the election-night figure, shared by the TX 2024 backtest and the
## simulated-night driver so the two cannot drift.
## Vector PDF; the filename carries the config tag and a run timestamp.
# ========================================
# ---- election-night chart ----------------------------------------------------------
# The point of this figure is the SHAPE over the night: whether the estimate converges on
# the truth and whether the interval narrows monotonically as votes land. Both of those
# are things the stage table makes you reconstruct in your head.
#
# Plotted against the naive count-what-is-in line, because that is the benchmark the
# model has to beat -- it starts +8.9pp too Democratic at 18:45.
#
# "Timestamped" twice over: the x axis is election-night wall clock (local), and the
# FILENAME carries the run time so successive runs do not overwrite each other.
#
# Output is vector PDF.
# `tag` distinguishes ablation runs. Without it every arm writes
# tx24_night_{ARM}_{time}.pdf and the configurations are indistinguishable except by
# timestamp, which makes a set of plots useless for the comparison they exist for.
plot_night <- function(res, arm = ARM, truth_2p = T_2p, outdir = "quality_reports/figures",
                       tag = paste0(if (USE_COMPOSE) "compose" else "indep",
                                    if (USE_CENSOR) "+censor" else "",
                                    if (SD_UNREP > 0) "" else "+nounrep"),
                       title = NULL, night_end = NULL) {
  need <- c("ggplot2", "patchwork", "scales")
  miss <- need[!vapply(need, requireNamespace, TRUE, quietly = TRUE)]
  if (length(miss)) { cli_alert_warning("plot skipped, missing: {miss}"); return(invisible(NULL)) }
  suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
  `%||%` <- function(a, b) if (is.null(a)) b else a
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  # The terminal canvass stage sits days or weeks after election night, so including it on
  # a time axis would compress the whole night into a sliver. It is excluded from the axis
  # and reported in the subtitle instead.
  # DERIVED from the data, not hardcoded: this function is shared by the TX 2024 backtest
  # and the 2026 simulation, and a fixed 2024 date silently filtered every sim row out,
  # producing an empty figure that still wrote a valid PDF.
  NIGHT_END <- night_end %||% (min(res$ts, na.rm = TRUE) + as.difftime(14, units = "hours"))
  # The zone comes from the DATA, not a constant: the TX 2024 backtest runs on Texas
  # local time but Ethan's sim uses America/New_York (his SIM_TZ), so a hardcoded "CST"
  # mislabels every sim figure by an hour.
  TZ_LAB <- {
    z <- attr(res$ts, "tzone")
    ab <- tryCatch(format(min(res$ts, na.rm = TRUE), "%Z"), error = function(e) NULL)
    if (!is.null(ab) && nzchar(ab)) sprintf("local time (%s)", ab)
    else if (!is.null(z) && nzchar(z)) sprintf("local time (%s)", z)
    else "local time"
  }
  d <- res |> filter(ts <= NIGHT_END)
  if (!nrow(d)) { cli_alert_danger("plot_night: no stages inside the night window"); return(invisible(NULL)) }
  fin <- res |> filter(ts > NIGHT_END) |> slice_tail(n = 1)

  COL_EST <- "#1f3b73"; COL_NAIVE <- "#b2182b"; COL_TRUTH <- "grey25"

  p_top <- ggplot(d, aes(x = ts)) +
    geom_ribbon(aes(ymin = lo_2p, ymax = hi_2p, fill = "95% credible interval"),
                alpha = 0.22) +
    geom_hline(aes(yintercept = 100 * truth_2p, colour = "Final result"),
               linetype = "dashed", linewidth = 0.6) +
    geom_line(aes(y = naive_2p, colour = "Naive: count what is in"),
              linetype = "dotted", linewidth = 0.8) +
    geom_point(aes(y = naive_2p, colour = "Naive: count what is in"),
               size = 1.6, shape = 1) +
    geom_line(aes(y = est_2p, colour = "Model estimate"), linewidth = 0.9) +
    geom_point(aes(y = est_2p, colour = "Model estimate"), size = 2.0) +
    scale_colour_manual(values = c("Model estimate" = COL_EST,
                                   "Naive: count what is in" = COL_NAIVE,
                                   "Final result" = COL_TRUTH), name = NULL) +
    scale_fill_manual(values = c("95% credible interval" = COL_EST), name = NULL) +
    # Limits span the ribbon AND the naive line, with padding. Without this the first
    # stage's lower bound runs off the panel floor and the widest interval in the figure
    # is the one you cannot see -- which is precisely the one worth seeing.
    scale_y_continuous(
      labels = scales::label_number(suffix = "%", accuracy = 1),
      limits = range(c(d$lo_2p, d$hi_2p, d$naive_2p, 100 * truth_2p), na.rm = TRUE) +
               c(-0.6, 0.6)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M",
                     expand = expansion(mult = 0.02)) +
    labs(y = "Two-party Dem share",
         title = title %||% sprintf("TX 2024 US Senate -- election-night estimate [%s], ARM=%s",
                                    tag, arm),
         subtitle = sprintf(
           "Final result %.2f%%. Terminal canvass stage (off-axis): estimate %.2f%%, error %+.2fpp. Coverage %d/%d stages.",
           100 * truth_2p,
           if (nrow(fin)) fin$est_2p else NA_real_,
           if (nrow(fin)) fin$err_2p else NA_real_,
           sum(res$cov_2p), nrow(res))) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", legend.box = "horizontal",
          axis.title.x = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))

  p_bot <- ggplot(d, aes(x = ts, y = pct_votes)) +
    geom_area(fill = COL_EST, alpha = 0.18) +
    geom_line(colour = COL_EST, linewidth = 0.7) +
    geom_point(colour = COL_EST, size = 1.4) +
    # modelled turnout as % of the true final, on the same panel: this is the quantity
    # the bounded-total reparameterization changed most (184% -> 88% at 20:00), and it
    # belongs next to the reporting curve that drives it
    geom_hline(yintercept = 100, linetype = "dashed", colour = COL_TRUTH, linewidth = 0.4) +
    geom_line(aes(y = pmin(turn, 200)), colour = COL_NAIVE, linewidth = 0.7,
              linetype = "longdash") +
    geom_point(aes(y = pmin(turn, 200)), colour = COL_NAIVE, size = 1.3, shape = 1) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::label_number(suffix = "%", accuracy = 1)) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
    labs(x = sprintf("Election night, %s", TZ_LAB),
         y = "% of final vote\nreported (solid), modelled (dashed)") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  pp <- p_top / p_bot + patchwork::plot_layout(heights = c(2.6, 1))
  f <- file.path(outdir, sprintf("tx24_night_%s_%s_%s.pdf", arm,
                                 gsub("[^A-Za-z0-9]", "", tag),
                                 format(Sys.time(), "%Y%m%d-%H%M%S")))
  ggsave(f, pp, width = 9, height = 7.2, device = grDevices::pdf)
  cli_alert_success("wrote {f}")
  invisible(f)
}

