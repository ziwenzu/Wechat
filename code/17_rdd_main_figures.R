bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)

dt <- load_rdd_data(paths)
max_window <- 30L

family_gloss <- c(
  "public_service"   = "Public Service",
  "soft_propaganda"  = "Soft Propaganda",
  "state_governance" = "State Governance",
  "hard_propaganda"  = "Hard Propaganda"
)
family_order <- c("public_service", "soft_propaganda", "hard_propaganda", "state_governance")

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

agg_daily <- function(sub, rv) {
  sub[, .(n = .N,
          one_click_rate = mean(one_click_rate, na.rm = TRUE),
          like_rate = mean(like_rate, na.rm = TRUE),
          look_rate = mean(look_rate, na.rm = TRUE),
          share_rate = mean(share_rate, na.rm = TRUE)
  ), by = .(x = get(rv))]
}

daily_2018 <- agg_daily(art_2018, "days_from_2018")
daily_2020 <- agg_daily(art_2020, "days_from_2020")

rd_x_breaks <- seq(-max_window, max_window, by = 10)
rd_x_scale  <- ggplot2::scale_x_continuous(breaks = rd_x_breaks)

# ── Single-panel RD plots ────────────────────────────────────

save_rd <- function(daily, yvar, y_label, filename) {
  d <- data.table::copy(daily)
  d[, y := get(yvar)]

  rd <- rdrobust::rdplot(d$y, d$x, binselect = "esmv", p = 1, ci = 95, hide = TRUE)
  bins <- data.table::as.data.table(rd$vars_bins)
  poly <- data.table::as.data.table(rd$vars_poly)
  poly_left  <- poly[rdplot_x < -0.5]
  poly_right <- poly[rdplot_x >  0.5]

  ribbon <- rbind(
    make_ci_ribbon(d[x < 0], yvar)[, side := "left"],
    make_ci_ribbon(d[x > 0], yvar)[, side := "right"]
  )

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = ribbon,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = side),
      fill = "grey75", alpha = 0.4) +
    ggplot2::geom_point(data = bins,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y),
      shape = 21, size = 2.2, fill = "white", color = "grey30", stroke = 0.5) +
    ggplot2::geom_line(data = poly_left,
      ggplot2::aes(x = rdplot_x, y = rdplot_y), color = "black", linewidth = 0.9) +
    ggplot2::geom_line(data = poly_right,
      ggplot2::aes(x = rdplot_x, y = rdplot_y), color = "black", linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey40") +
    rd_x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Days relative to cutoff", y = y_label) +
    rd_theme

  save_figure(
    path = file.path(paths$figures, filename),
    plot = g,
    width = 6.5,
    height = 4.5,
    units = "in"
  )
}

message("=== Single-panel RD plots ===")

save_rd(daily_2018, "one_click_rate", "One-Click Engagement Rate", "rdd_2018_one_click_rate.pdf")
save_rd(daily_2018, "share_rate", "Share/Forward Rate", "rdd_2018_share_rate.pdf")
save_rd(daily_2020, "one_click_rate", "One-Click Rate (Like + Zaikan)", "rdd_2020_one_click_rate.pdf")
save_rd(daily_2020, "look_rate", "Zaikan Rate (high-visibility)", "rdd_2020_look_rate.pdf")
save_rd(daily_2020, "share_rate", "Share/Forward Rate", "rdd_2020_share_rate.pdf")

# ── 2020 Unbundling plot ─────────────────────────────────────

message("=== 2020 Unbundling ===")

dl20 <- data.table::copy(daily_2020[x < 0])
dr20 <- data.table::copy(daily_2020[x > 0])

ub_bin <- function(d, yvar, nbins = 10) {
  d <- data.table::copy(d)
  d[, y := get(yvar)]
  d[, bin := cut(x, breaks = nbins, labels = FALSE)]
  d[!is.na(bin), .(rdplot_mean_x = mean(x), rdplot_mean_y = mean(y)), by = bin]
}

bins_ub_all <- rbind(
  ub_bin(dl20, "one_click_rate")[, metric := "Combined"],
  ub_bin(dr20, "like_rate")[, metric := "Like"],
  ub_bin(dr20, "look_rate")[, metric := "Zaikan"]
)

ribbon_ub <- rbind(
  make_ci_ribbon(dl20, "one_click_rate")[, metric := "Combined"],
  make_ci_ribbon(dr20, "like_rate")[, metric := "Like"],
  make_ci_ribbon(dr20, "look_rate")[, metric := "Zaikan"]
)

ub_fit <- function(d, yvar) {
  d <- data.table::copy(d)
  d[, y := get(yvar)]
  fit <- lm(y ~ x, data = d, weights = n)
  grid <- data.table::data.table(x = seq(min(d$x), max(d$x), length.out = 100))
  pred <- predict(fit, grid)
  grid[, `:=`(rdplot_y = pred, rdplot_x = x)]
  grid
}

poly_ub <- rbind(
  ub_fit(dl20, "one_click_rate")[, metric := "Combined"],
  ub_fit(dr20, "like_rate")[, metric := "Like"],
  ub_fit(dr20, "look_rate")[, metric := "Zaikan"]
)

shape_map <- c("Combined" = 21, "Like" = 24, "Zaikan" = 22)
lty_map   <- c("Combined" = "solid", "Like" = "solid", "Zaikan" = "dashed")

g_ub <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = ribbon_ub,
    ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = metric),
    fill = "grey75", alpha = 0.35) +
  ggplot2::geom_point(data = bins_ub_all,
    ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y, shape = metric),
    size = 2.2, fill = "white", color = "grey30", stroke = 0.5) +
  ggplot2::geom_line(data = poly_ub,
    ggplot2::aes(x = rdplot_x, y = rdplot_y, linetype = metric, group = metric),
    color = "black", linewidth = 0.9) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey40") +
  ggplot2::scale_shape_manual(values = shape_map, name = NULL) +
  ggplot2::scale_linetype_manual(values = lty_map, name = NULL) +
  rd_x_scale +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::labs(x = "Days relative to cutoff", y = "Engagement rate") +
  rd_theme + ggplot2::theme(legend.position = "bottom")

save_figure(
  path = file.path(paths$figures, "rdd_2020_unbundling.pdf"),
  plot = g_ub,
  width = 6.5,
  height = 5,
  units = "in"
)

# ── By-family panels ─────────────────────────────────────────

message("=== By-family panels ===")

save_family_panels <- function(art, rv, yvar, y_label, filename) {
  daily_fam <- art[content_family %in% family_order, .(
    n = .N, y = mean(get(yvar), na.rm = TRUE)
  ), by = .(x = get(rv),
            family = factor(content_family, levels = family_order,
                            labels = family_gloss[family_order]))]

  fam_levels <- levels(daily_fam$family)
  all_bins <- list(); all_poly <- list(); all_ribbon <- list()

  for (f in fam_levels) {
    fd <- daily_fam[family == f]
    rd_f <- tryCatch(
      rdrobust::rdplot(fd$y, fd$x, binselect = "esmv", p = 1, ci = 95, hide = TRUE),
      error = function(e) NULL)
    if (is.null(rd_f)) next

    b <- data.table::as.data.table(rd_f$vars_bins)[, family := f]
    p <- data.table::as.data.table(rd_f$vars_poly)[, family := f]
    p <- p[abs(rdplot_x) > 0.5]
    p[, side := ifelse(rdplot_x < 0, "left", "right")]

    rl <- make_ci_ribbon(fd[x < 0], "y")[, `:=`(side = "left", family = f)]
    rr <- make_ci_ribbon(fd[x > 0], "y")[, `:=`(side = "right", family = f)]

    all_bins[[f]] <- b; all_poly[[f]] <- p; all_ribbon[[f]] <- rbind(rl, rr)
  }

  bins_all   <- data.table::rbindlist(all_bins)
  poly_all   <- data.table::rbindlist(all_poly)
  ribbon_all <- data.table::rbindlist(all_ribbon)
  for (d in list(bins_all, poly_all, ribbon_all)) d[, family := factor(family, levels = fam_levels)]

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = ribbon_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = side),
      fill = "grey75", alpha = 0.4) +
    ggplot2::geom_point(data = bins_all,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y),
      shape = 21, size = 1.8, fill = "white", color = "grey30", stroke = 0.45) +
    ggplot2::geom_line(data = poly_all,
      ggplot2::aes(x = rdplot_x, y = rdplot_y, group = side),
      color = "black", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey40") +
    ggplot2::facet_wrap(~family, ncol = 2, scales = "free_y") +
    rd_x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Days relative to cutoff", y = y_label) +
    rd_theme +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = 10))

  save_figure(
    path = file.path(paths$figures, filename),
    plot = g,
    width = 7,
    height = 5.5,
    units = "in"
  )
}

save_family_panels(art_2018, "days_from_2018", "one_click_rate",
                   "One-Click Engagement Rate", "rdd_2018_one_click_by_family.pdf")
save_family_panels(art_2020, "days_from_2020", "one_click_rate",
                   "One-Click Rate (Like + Zaikan)", "rdd_2020_one_click_by_family.pdf")

# ── Unbundling by family ─────────────────────────────────────

message("=== Unbundling by family ===")

save_unbundling_by_family <- function(art, filename, nbins = 8) {
  fam_labels <- family_gloss[family_order]
  all_bins <- list(); all_ribbon <- list(); all_poly <- list()

  for (i in seq_along(fam_labels)) {
    fl <- fam_labels[i]
    sub <- art[content_family == family_order[i]]
    daily_f <- sub[, .(n = .N,
      one_click_rate = mean(one_click_rate, na.rm = TRUE),
      like_rate = mean(like_rate, na.rm = TRUE),
      look_rate = mean(look_rate, na.rm = TRUE)
    ), by = .(x = days_from_2020)]
    dl <- daily_f[x < 0]; dr <- daily_f[x > 0]

    all_bins[[paste0(fl,"_c")]] <- ub_bin(dl,"one_click_rate",nbins)[,`:=`(metric="Combined",family=fl)]
    all_bins[[paste0(fl,"_l")]] <- ub_bin(dr,"like_rate",nbins)[,`:=`(metric="Like",family=fl)]
    all_bins[[paste0(fl,"_z")]] <- ub_bin(dr,"look_rate",nbins)[,`:=`(metric="Zaikan",family=fl)]
    all_ribbon[[paste0(fl,"_c")]] <- make_ci_ribbon(dl,"one_click_rate")[,`:=`(metric="Combined",family=fl)]
    all_ribbon[[paste0(fl,"_l")]] <- make_ci_ribbon(dr,"like_rate")[,`:=`(metric="Like",family=fl)]
    all_ribbon[[paste0(fl,"_z")]] <- make_ci_ribbon(dr,"look_rate")[,`:=`(metric="Zaikan",family=fl)]
    all_poly[[paste0(fl,"_c")]] <- ub_fit(dl,"one_click_rate")[,`:=`(metric="Combined",family=fl)]
    all_poly[[paste0(fl,"_l")]] <- ub_fit(dr,"like_rate")[,`:=`(metric="Like",family=fl)]
    all_poly[[paste0(fl,"_z")]] <- ub_fit(dr,"look_rate")[,`:=`(metric="Zaikan",family=fl)]
  }

  bins_all <- data.table::rbindlist(all_bins, fill = TRUE)
  ribbon_all <- data.table::rbindlist(all_ribbon, fill = TRUE)
  poly_all <- data.table::rbindlist(all_poly, fill = TRUE)
  fac <- function(d) { d[, family := factor(family, levels = fam_labels)]; d }
  bins_all <- fac(bins_all); ribbon_all <- fac(ribbon_all); poly_all <- fac(poly_all)

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = ribbon_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = metric),
      fill = "grey75", alpha = 0.35) +
    ggplot2::geom_point(data = bins_all,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y, shape = metric),
      size = 1.8, fill = "white", color = "grey30", stroke = 0.45) +
    ggplot2::geom_line(data = poly_all,
      ggplot2::aes(x = rdplot_x, y = rdplot_y, linetype = metric, group = metric),
      color = "black", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey40") +
    ggplot2::facet_wrap(~family, ncol = 2, scales = "free_y") +
    ggplot2::scale_shape_manual(values = shape_map, name = NULL) +
    ggplot2::scale_linetype_manual(values = lty_map, name = NULL) +
    rd_x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Days relative to cutoff", y = "Engagement rate") +
    rd_theme +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = 10),
                   legend.position = "bottom")

  save_figure(
    path = file.path(paths$figures, filename),
    plot = g,
    width = 7,
    height = 6,
    units = "in"
  )
}

save_unbundling_by_family(art_2020, "rdd_2020_unbundling_by_family.pdf")

message("Done: all RDD figures saved.")
