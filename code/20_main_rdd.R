bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "rddensity", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

cutoff_2018 <- as.Date("2018-12-21")
cutoff_2020 <- as.Date("2020-07-01")

dt[, days_from_2018 := as.integer(publish_date - cutoff_2018)]
dt[, days_from_2020 := as.integer(publish_date - cutoff_2020)]

dt[, one_click_rate := like_rate + look_rate]

family_gloss <- c(
  "public_service"   = "Public Service",
  "soft_propaganda"  = "Soft Propaganda",
  "state_governance" = "State Governance",
  "hard_propaganda"  = "Hard Propaganda"
)
family_order <- c("public_service", "soft_propaganda", "hard_propaganda", "state_governance")

max_window <- 30L

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

run_rdd <- function(y, x, p = 1, label = "") {
  fit <- rdrobust::rdrobust(y = y, x = x, p = p, kernel = "triangular",
                            bwselect = "mserd", all = TRUE)
  data.table::data.table(
    label = label, tau_rb = fit$coef[3], se_rb = fit$se[3],
    ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
    bw_l = fit$bws[1, 1], bw_r = fit$bws[1, 2],
    n_l = fit$N_h[1], n_r = fit$N_h[2], p = p
  )
}

# ── 2018 RDD: H2 ─────────────────────────────────────────────
# Treatment: one-click button goes from low-visibility (like) to
# high-visibility (haokan). The button is the SAME slot.
# Main outcome: one_click_rate (like + look, only one nonzero at a time pre-2020)
# H2 predicts: one-click engagement DROPS because visibility cost rises.

message("=== 2018 RDD: One-click engagement under visibility shock ===")

results_2018 <- rbind(
  run_rdd(art_2018$one_click_rate, art_2018$days_from_2018, 1,
          "One-Click Rate (linear)"),
  run_rdd(art_2018$one_click_rate, art_2018$days_from_2018, 2,
          "One-Click Rate (quadratic)"),
  run_rdd(art_2018$share_rate, art_2018$days_from_2018, 1,
          "Share/Forward Rate (placebo)")
)
results_2018[, cutoff := "2018-12-21"]

# ── 2020 RDD: H3 ─────────────────────────────────────────────
# Treatment: one button splits into two (like = low-vis, zaikan = high-vis)
# + share button added to footer.
# H3 predicts: approval shifts BACK to like; zaikan may drop.

message("=== 2020 RDD: Unbundling low-vis and high-vis channels ===")

outcomes_2020 <- list(
  list(var = "one_click_rate", lbl = "One-Click Rate (Like + Zaikan)"),
  list(var = "like_rate",      lbl = "Like Rate (low-visibility)"),
  list(var = "look_rate",      lbl = "Zaikan Rate (high-visibility)"),
  list(var = "share_rate",     lbl = "Share/Forward Rate")
)

results_2020 <- data.table::rbindlist(lapply(outcomes_2020, function(o) {
  message("  ", o$lbl)
  rbind(
    run_rdd(art_2020[[o$var]], art_2020$days_from_2020, 1,
            paste0(o$lbl, " (linear)")),
    run_rdd(art_2020[[o$var]], art_2020$days_from_2020, 2,
            paste0(o$lbl, " (quadratic)"))
  )
}))
results_2020[, cutoff := "2020-07-01"]

# ── Heterogeneity by content family: H4 ──────────────────────
# H4: reallocation strongest for hard propaganda, weakest for public service.

message("=== 2020 heterogeneity by content family (H4) ===")

het_outcomes <- list(
  list(var = "one_click_rate", lbl = "One-Click"),
  list(var = "like_rate",      lbl = "Like"),
  list(var = "look_rate",      lbl = "Zaikan")
)

het_results <- data.table::rbindlist(lapply(names(family_gloss), function(fam) {
  fl <- family_gloss[[fam]]
  sub <- art_2020[content_family == fam]

  data.table::rbindlist(lapply(het_outcomes, function(o) {
    message("  ", fl, " - ", o$lbl)
    run_rdd(sub[[o$var]], sub$days_from_2020, 1,
            paste0(o$lbl, " [", fl, "]"))
  }))
}))
het_results[, cutoff := "2020 (by family)"]

# ── Tables ────────────────────────────────────────────────────

fmt_tex <- function(res, caption, label, filepath) {
  tex_dt <- res[, .(
    Specification = label,
    Estimate  = tau_rb,
    SE        = se_rb,
    `p-value` = pvalue,
    `BW (days)` = round((bw_l + bw_r) / 2),
    `N (L)`   = n_l,
    `N (R)`   = n_r
  )]
  write_tex_table(
    tex_dt, caption = caption, label = label, path = filepath,
    digits = c(Estimate = 4L, SE = 4L, `p-value` = 3L,
               `BW (days)` = 0L, `N (L)` = 0L, `N (R)` = 0L),
    align = paste0("l", paste(rep("r", 6), collapse = ""))
  )
}

fmt_tex(results_2018,
  "RDD at December 2018: effect of raising one-click visibility (H2). One-Click Rate = Like + Haokan/Zaikan, capturing the same footer button across regimes. Article-level outcomes, triangular kernel, MSE-optimal bandwidth, robust bias-corrected inference.",
  "tab:rdd-2018",
  file.path(paths$tables, "rdd_2018_main.tex"))

fmt_tex(results_2020,
  "RDD at July 2020: unbundling low-visibility and high-visibility approval (H3). Like Rate captures restored low-visibility approval; Zaikan Rate captures continued high-visibility endorsement. Article-level outcomes, triangular kernel, MSE-optimal bandwidth.",
  "tab:rdd-2020",
  file.path(paths$tables, "rdd_2020_main.tex"))

fmt_tex(het_results,
  "Heterogeneous RDD at July 2020 by content family (H4). Article-level outcomes. Political-risk gradient predicts strongest reallocation for hard propaganda.",
  "tab:rdd-2020-het",
  file.path(paths$tables, "rdd_2020_heterogeneity.tex"))

# ── RD Plots ─────────────────────────────────────────────────

message("=== RD Plots (publication-style graphics) ===")

rd_palette <- list(
  ink = "#1A1A1A",
  axis = "#2F2F2F",
  cutoff = "#7A7A7A",
  ci = "#BBBBBB",
  point_fill = "white",
  point_outline = "#3A3A3A",
  combined = "#7A7A7A",
  like = "#1A1A1A",
  zaikan = "#5A5A5A"
)

rd_percent_accuracy <- function(values) {
  values <- values[is.finite(values)]

  if (!length(values)) {
    return(0.1)
  }

  span <- diff(range(values))

  if (!is.finite(span) || span >= 0.03) {
    return(0.5)
  }

  if (span >= 0.01) {
    return(0.2)
  }

  if (span >= 0.004) {
    return(0.1)
  }

  0.05
}

rd_theme <- function(show_legend = FALSE) {
  ggplot2::theme_classic(base_size = 11, base_family = "serif") +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 10.5, color = rd_palette$ink),
      axis.text = ggplot2::element_text(size = 9.5, color = rd_palette$axis),
      axis.line = ggplot2::element_line(linewidth = 0.4, color = rd_palette$axis),
      axis.ticks = ggplot2::element_line(linewidth = 0.4, color = rd_palette$axis),
      axis.ticks.length = grid::unit(0.12, "cm"),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9, color = rd_palette$ink),
      legend.key.height = grid::unit(0.35, "cm"),
      legend.key.width = grid::unit(0.8, "cm"),
      plot.margin = ggplot2::margin(t = 8, r = 10, b = 6, l = 6)
    )
}

make_rd_data <- function(y, x, binselect = "esmv", p = 1) {
  out <- rdrobust::rdplot(y = y, x = x, binselect = binselect, p = p,
                          ci = 95, hide = TRUE)
  bins <- data.table::as.data.table(out$vars_bins)
  poly <- data.table::as.data.table(out$vars_poly)
  poly[, side := ifelse(rdplot_x < 0, "left", "right")]
  list(bins = bins, poly = poly)
}

save_rd_figure <- function(y, x, y_label, filename, binselect = "esmv", p = 1) {
  rd <- make_rd_data(y, x, binselect = binselect, p = p)
  bins <- rd$bins
  fit_side <- function(x_sub, y_sub, degree = 1L, side_label = "left") {
    sub <- data.table::data.table(x = x_sub, y = y_sub)[is.finite(x) & is.finite(y)]

    if (nrow(sub) <= degree) {
      return(data.table::data.table())
    }

    fit <- stats::lm(y ~ stats::poly(x, degree, raw = TRUE), data = sub)
    grid <- data.table::data.table(x = seq(min(sub$x), max(sub$x), length.out = 200))
    pred <- stats::predict(fit, newdata = grid, se.fit = TRUE)
    grid[, `:=`(
      yhat = pred$fit,
      ci_l = pred$fit - 1.96 * pred$se.fit,
      ci_r = pred$fit + 1.96 * pred$se.fit,
      side = side_label
    )]
    grid
  }

  fit_left <- fit_side(x[x < 0], y[x < 0], degree = p, side_label = "left")
  fit_right <- fit_side(x[x > 0], y[x > 0], degree = p, side_label = "right")
  fit_all <- data.table::rbindlist(list(fit_left, fit_right), use.names = TRUE, fill = TRUE)
  plot_values <- c(
    bins$rdplot_mean_y,
    fit_all$yhat,
    fit_all$ci_l,
    fit_all$ci_r
  )

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fit_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = side),
      fill = "#D8D8D8", alpha = 0.45
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y),
      shape = 21, size = 2.15, stroke = 0.45,
      fill = rd_palette$point_fill, color = rd_palette$point_outline
    ) +
    ggplot2::geom_line(
      data = fit_all,
      ggplot2::aes(x = x, y = yhat, group = side),
      color = rd_palette$ink, linewidth = 0.75, lineend = "round"
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "22", linewidth = 0.45, color = rd_palette$cutoff
    ) +
    ggplot2::scale_x_continuous(
      breaks = pretty(range(x, na.rm = TRUE), n = 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = pretty(range(plot_values, na.rm = TRUE), n = 4),
      labels = scales::label_percent(accuracy = rd_percent_accuracy(plot_values)),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::labs(
      x = "Days relative to cutoff",
      y = y_label
    ) +
    rd_theme()

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 6.5, height = 4.4, units = "in",
                  device = "pdf", bg = "white", useDingbats = FALSE)
}

save_rd_figure(art_2018$one_click_rate, art_2018$days_from_2018,
               "One-Click Engagement Rate",
               "rdd_2018_one_click_rate.pdf")

save_rd_figure(art_2018$share_rate, art_2018$days_from_2018,
               "Share/Forward Rate",
               "rdd_2018_share_rate.pdf")

save_rd_figure(art_2020$one_click_rate, art_2020$days_from_2020,
               "One-Click Rate (Like + Zaikan)",
               "rdd_2020_one_click_rate.pdf")

save_rd_figure(art_2020$look_rate, art_2020$days_from_2020,
               "Zaikan Rate (high-visibility)",
               "rdd_2020_look_rate.pdf")

save_rd_figure(art_2020$share_rate, art_2020$days_from_2020,
               "Share/Forward Rate",
               "rdd_2020_share_rate.pdf")

message("=== 2020 Unbundling Plot ===")

make_unbundling <- function(art, filename, nbins = 15L) {
  article <- art[, .(
    x = days_from_2020,
    one_click = one_click_rate,
    like = like_rate,
    look = look_rate
  )]

  dl <- article[x < 0]
  dr <- article[x >= 0]
  dr_fit <- article[x > 0]

  bin_it <- function(sub, yvar, nb) {
    sub <- data.table::copy(sub)
    sub[, y := get(yvar)]
    sub[, bin := cut(x, breaks = nb, labels = FALSE)]
    sub[, {
      ci_half <- if (.N > 1L) 1.96 * stats::sd(y) / sqrt(.N) else 0
      .(bx = mean(x), by = mean(y),
        ci_l = mean(y) - ci_half,
        ci_r = mean(y) + ci_half)
    }, by = bin]
  }

  fit_it <- function(sub, yvar) {
    sub <- data.table::copy(sub)
    sub[, y := get(yvar)]
    fit <- stats::lm(y ~ stats::poly(x, 1, raw = TRUE), data = sub)
    grid <- data.table::data.table(x = seq(min(sub$x), max(sub$x), length.out = 200))
    pred <- stats::predict(fit, grid, se.fit = TRUE)
    grid[, `:=`(yhat = pred$fit,
                ci_l = pred$fit - 1.96 * pred$se.fit,
                ci_r = pred$fit + 1.96 * pred$se.fit)]
    grid
  }

  bins_l  <- bin_it(dl, "one_click", nbins)[, metric := "Combined"]
  bins_rl <- bin_it(dr, "like", nbins)[, metric := "Like"]
  bins_rz <- bin_it(dr, "look", nbins)[, metric := "Zaikan"]
  bins_all <- rbind(bins_l, bins_rl, bins_rz)

  fit_l  <- fit_it(dl, "one_click")[, metric := "Combined"]
  fit_rl <- fit_it(dr_fit, "like")[, metric := "Like"]
  fit_rz <- fit_it(dr_fit, "look")[, metric := "Zaikan"]
  fit_all <- rbind(fit_l, fit_rl, fit_rz)

  ms <- c("Combined" = 1, "Like" = 2, "Zaikan" = 0)
  plot_values <- c(bins_all$by, fit_all$yhat, fit_all$ci_l, fit_all$ci_r)
  label_dt <- data.table::data.table(
    x = c(-25, 25, 25),
    y = c(0.060, 0.045, 0.030),
    metric = c("Combined", "Like", "Zaikan")
  )

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fit_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = metric),
      fill = "#D8D8D8", alpha = 0.38, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = bins_all,
      ggplot2::aes(x = bx, y = by, shape = metric),
      size = 2.1, stroke = 0.4, color = rd_palette$ink, alpha = 0.95
    ) +
    ggplot2::geom_line(
      data = fit_all,
      ggplot2::aes(x = x, y = yhat, group = metric),
      linewidth = 0.8, lineend = "round", color = rd_palette$ink
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "22", linewidth = 0.45, color = rd_palette$cutoff
    ) +
    ggplot2::geom_text(
      data = label_dt,
      ggplot2::aes(x = x, y = y, label = metric),
      size = 3.2, family = "serif", hjust = 0.5,
      color = rd_palette$ink, show.legend = FALSE
    ) +
    ggplot2::scale_shape_manual(values = ms) +
    ggplot2::scale_x_continuous(
      breaks = pretty(article$x, n = 5),
      limits = range(article$x),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = pretty(range(plot_values, na.rm = TRUE), n = 4),
      labels = scales::label_percent(accuracy = rd_percent_accuracy(plot_values)),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::guides(shape = "none") +
    ggplot2::labs(
      x = "Days relative to cutoff",
      y = "Engagement rate"
    ) +
    rd_theme()

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 6.5, height = 4.8, units = "in",
                  device = "pdf", bg = "white", useDingbats = FALSE)
}

make_unbundling(art_2020, "rdd_2020_unbundling.pdf")

make_bin_summary <- function(sub, nbins = 10L) {
  sub <- data.table::copy(sub[is.finite(x) & is.finite(y)])

  if (!nrow(sub)) {
    return(data.table::data.table())
  }

  sub[, bin := cut(x, breaks = nbins, labels = FALSE)]
  sub[!is.na(bin), {
    ci_half <- if (.N > 1L) 1.96 * stats::sd(y) / sqrt(.N) else 0
    .(
      bx = mean(x),
      by = mean(y),
      ci_l = mean(y) - ci_half,
      ci_r = mean(y) + ci_half
    )
  }, by = bin]
}

make_fit_band <- function(sub, degree = 1L, grid_n = 200L) {
  sub <- data.table::copy(sub[is.finite(x) & is.finite(y)])

  if (!nrow(sub)) {
    return(data.table::data.table())
  }

  fit <- stats::lm(y ~ stats::poly(x, degree, raw = TRUE), data = sub)
  grid <- data.table::data.table(x = seq(min(sub$x), max(sub$x), length.out = grid_n))
  pred <- stats::predict(fit, newdata = grid, se.fit = TRUE)
  grid[, `:=`(
    yhat = pred$fit,
    ci_l = pred$fit - 1.96 * pred$se.fit,
    ci_r = pred$fit + 1.96 * pred$se.fit
  )]
  grid
}

facet_family_theme <- function(show_legend = FALSE) {
  rd_theme(show_legend = show_legend) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 9.5, color = rd_palette$ink),
      panel.spacing = grid::unit(0.7, "lines")
    )
}

save_family_2018_panels <- function(art, filename, nbins = 10L) {
  plot_dt <- art[content_family %in% family_order, .(
    family = factor(content_family, levels = family_order, labels = family_gloss[family_order]),
    x = days_from_2018,
    y = one_click_rate
  )]

  bins_all <- plot_dt[, make_bin_summary(.SD, nbins = nbins), by = family]
  fit_all <- plot_dt[, {
    left <- make_fit_band(.SD[x < 0])[, side := "left"]
    right <- make_fit_band(.SD[x > 0])[, side := "right"]
    data.table::rbindlist(list(left, right), use.names = TRUE, fill = TRUE)
  }, by = family]

  plot_values <- c(
    bins_all$by,
    fit_all$yhat, fit_all$ci_l, fit_all$ci_r
  )

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fit_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = side),
      fill = "#D6D6D6", alpha = 0.45
    ) +
    ggplot2::geom_line(
      data = fit_all,
      ggplot2::aes(x = x, y = yhat, group = side),
      color = rd_palette$ink, linewidth = 0.8, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = bins_all,
      ggplot2::aes(x = bx, y = by),
      shape = 21, size = 1.95, stroke = 0.4,
      fill = rd_palette$point_fill, color = rd_palette$point_outline
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "22", linewidth = 0.45, color = rd_palette$cutoff
    ) +
    ggplot2::facet_wrap(~family, ncol = 2) +
    ggplot2::scale_x_continuous(
      breaks = pretty(plot_dt$x, n = 5),
      limits = range(plot_dt$x),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = pretty(range(plot_values, na.rm = TRUE), n = 4),
      labels = scales::label_percent(accuracy = rd_percent_accuracy(plot_values)),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::labs(
      x = "Days relative to cutoff",
      y = "One-click engagement rate"
    ) +
    facet_family_theme()

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 7.2, height = 5.8, units = "in",
                  device = "pdf", bg = "white", useDingbats = FALSE)
}

save_family_2020_panels <- function(art, filename, nbins = 8L) {
  panel_dt <- data.table::rbindlist(list(
    art[content_family %in% family_order & days_from_2020 < 0, .(
      family = factor(content_family, levels = family_order, labels = family_gloss[family_order]),
      x = days_from_2020,
      y = one_click_rate,
      metric = "Combined"
    )],
    art[content_family %in% family_order & days_from_2020 >= 0, .(
      family = factor(content_family, levels = family_order, labels = family_gloss[family_order]),
      x = days_from_2020,
      y = like_rate,
      metric = "Like"
    )],
    art[content_family %in% family_order & days_from_2020 >= 0, .(
      family = factor(content_family, levels = family_order, labels = family_gloss[family_order]),
      x = days_from_2020,
      y = look_rate,
      metric = "Zaikan"
    )]
  ), use.names = TRUE)

  bins_all <- panel_dt[, make_bin_summary(.SD, nbins = nbins), by = .(family, metric)]
  fit_all <- panel_dt[, make_fit_band(.SD), by = .(family, metric)]

  plot_values <- c(
    bins_all$by,
    fit_all$yhat, fit_all$ci_l, fit_all$ci_r
  )

  fill_map <- c(
    "Combined" = "#D2D2D2",
    "Like" = "#E2E2E2",
    "Zaikan" = "#BEBEBE"
  )
  shape_map <- c("Combined" = 1, "Like" = 2, "Zaikan" = 0)

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = fit_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = metric, fill = metric),
      alpha = 0.28
    ) +
    ggplot2::geom_line(
      data = fit_all,
      ggplot2::aes(x = x, y = yhat, group = metric),
      color = rd_palette$ink, linewidth = 0.75, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = bins_all,
      ggplot2::aes(x = bx, y = by, shape = metric),
      size = 1.9, stroke = 0.4, color = rd_palette$ink, fill = "white"
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "22", linewidth = 0.45, color = rd_palette$cutoff
    ) +
    ggplot2::facet_wrap(~family, ncol = 2) +
    ggplot2::scale_fill_manual(values = fill_map) +
    ggplot2::scale_shape_manual(values = shape_map) +
    ggplot2::scale_x_continuous(
      breaks = pretty(panel_dt$x, n = 5),
      limits = range(panel_dt$x),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = pretty(range(plot_values, na.rm = TRUE), n = 4),
      labels = scales::label_percent(accuracy = rd_percent_accuracy(plot_values)),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      x = "Days relative to cutoff",
      y = "Engagement rate"
    ) +
    facet_family_theme(show_legend = TRUE)

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 7.2, height = 5.9, units = "in",
                  device = "pdf", bg = "white", useDingbats = FALSE)
}

save_family_2018_panels(art_2018, "rdd_2018_one_click_by_family.pdf")
save_family_2020_panels(art_2020, "rdd_2020_unbundling_by_family.pdf")

# ── Validity checks ──────────────────────────────────────────

message("=== Density test ===")

dens_2018 <- rddensity::rddensity(art_2018$days_from_2018, c = 0)
dens_2020 <- rddensity::rddensity(art_2020$days_from_2020, c = 0)

density_dt <- data.table::data.table(
  Cutoff = c("2018-12-21", "2020-07-01"),
  `T-statistic` = c(dens_2018$test$t_jk, dens_2020$test$t_jk),
  `p-value` = c(dens_2018$test$p_jk, dens_2020$test$p_jk)
)
write_tex_table(density_dt, file.path(paths$tables, "rdd_density_test.tex"),
  caption = "McCrary density test for manipulation of the running variable.",
  label = "tab:rdd-density",
  digits = c(`T-statistic` = 3L, `p-value` = 3L))

message("=== Balance tests ===")

balance_all <- rbind(
  run_rdd(art_2018$read_num, art_2018$days_from_2018, 1, "Reads (2018)"),
  run_rdd(art_2020$read_num, art_2020$days_from_2020, 1, "Reads (2020)"),
  run_rdd(as.numeric(art_2018$content_family == "hard_propaganda"), art_2018$days_from_2018, 1, "Hard propaganda (2018)"),
  run_rdd(as.numeric(art_2020$content_family == "hard_propaganda"), art_2020$days_from_2020, 1, "Hard propaganda (2020)"),
  run_rdd(as.numeric(art_2018$content_family == "public_service"), art_2018$days_from_2018, 1, "Public service (2018)"),
  run_rdd(as.numeric(art_2020$content_family == "public_service"), art_2020$days_from_2020, 1, "Public service (2020)")
)

fmt_tex(balance_all,
  "Covariate balance at each cutoff. No controls, MSE-optimal bandwidth.",
  "tab:rdd-balance",
  file.path(paths$tables, "rdd_balance.tex"))

message("All RDD outputs saved.")
