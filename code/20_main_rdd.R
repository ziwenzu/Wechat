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

agg_daily <- function(sub, running_var) {
  sub[, .(
    n              = .N,
    one_click_rate = mean(one_click_rate, na.rm = TRUE),
    like_rate      = mean(like_rate, na.rm = TRUE),
    look_rate      = mean(look_rate, na.rm = TRUE),
    share_rate     = mean(share_rate, na.rm = TRUE),
    read_num       = mean(read_num, na.rm = TRUE),
    pct_hard       = mean(content_family == "hard_propaganda"),
    pct_service    = mean(content_family == "public_service")
  ), by = .(x = get(running_var))]
}

daily_2018 <- agg_daily(art_2018, "days_from_2018")
daily_2020 <- agg_daily(art_2020, "days_from_2020")

# ── H1: Attention and Utility ─────────────────────────────────
# Public service should attract more reads than propaganda.

message("=== H1: Attention and Utility ===")

h1_dt <- dt[content_family %in% names(family_gloss), .(
  mean_reads = mean(read_num, na.rm = TRUE),
  median_reads = as.double(stats::median(read_num, na.rm = TRUE)),
  n = .N
), by = .(Family = unname(family_gloss[content_family]))]
data.table::setorder(h1_dt, -mean_reads)

write_tex_table(
  h1_dt,
  file.path(paths$tables, "h1_reads_by_family.tex"),
  caption = "Mean and median article reads by content family (H1). Public service content attracts more routine consumption than propaganda.",
  label = "tab:h1-reads",
  digits = c(mean_reads = 0L, median_reads = 0L, n = 0L)
)

h1_test <- stats::lm(read_num ~ relevel(factor(content_family), ref = "hard_propaganda"),
                      data = dt[content_family %in% names(family_gloss)])
h1_coefs <- summary(h1_test)$coefficients
h1_rows <- data.table::data.table(
  Comparison = sub(".*\\)", "", rownames(h1_coefs)[-1]),
  Difference = h1_coefs[-1, 1],
  SE         = h1_coefs[-1, 2],
  `p-value`  = h1_coefs[-1, 4]
)

write_tex_table(
  h1_rows,
  file.path(paths$tables, "h1_reads_test.tex"),
  caption = "OLS differences in mean reads relative to hard propaganda (H1).",
  label = "tab:h1-reads-test",
  digits = c(Difference = 0L, SE = 0L, `p-value` = 3L)
)

# ── Estimation: article-level rdrobust, clustered at account ──

run_rdd <- function(y, x, cluster = NULL, p = 1, h = NULL, label = "") {
  args <- list(y = y, x = x, p = p, kernel = "triangular", all = TRUE)
  if (!is.null(cluster)) args$cluster <- cluster
  if (!is.null(h)) {
    args$h <- h
    args$b <- h
  } else {
    args$bwselect <- "mserd"
  }
  fit <- do.call(rdrobust::rdrobust, args)
  data.table::data.table(
    label = label, tau_conv = fit$coef[1], tau_bc = fit$coef[2],
    tau_rb = fit$coef[3], se_conv = fit$se[1], se_rb = fit$se[3],
    ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
    bw_l = fit$bws[1, 1], bw_r = fit$bws[1, 2],
    n_l = fit$N_h[1], n_r = fit$N_h[2], p = p
  )
}

cl_2018 <- art_2018$account_id
cl_2020 <- art_2020$account_id

message("=== 2018 RDD (H2): one-click visibility shock ===")

results_2018 <- rbind(
  run_rdd(art_2018$one_click_rate, art_2018$days_from_2018, cl_2018, 1, label = "One-Click (linear, clustered)"),
  run_rdd(art_2018$one_click_rate, art_2018$days_from_2018, cl_2018, 2, label = "One-Click (quadratic, clustered)"),
  run_rdd(art_2018$one_click_rate, art_2018$days_from_2018, NULL,    1, label = "One-Click (linear, robust HC)"),
  run_rdd(art_2018$share_rate,     art_2018$days_from_2018, cl_2018, 1, label = "Share Rate (placebo, clustered)")
)
results_2018[, cutoff := "2018-12-21"]

message("=== 2020 RDD (H3): unbundling ===")

outcomes_2020 <- list(
  list(var = "one_click_rate", lbl = "One-Click (Like+Zaikan)"),
  list(var = "look_rate",      lbl = "Zaikan (high-vis)"),
  list(var = "share_rate",     lbl = "Share/Forward")
)

results_2020 <- data.table::rbindlist(lapply(outcomes_2020, function(o) {
  message("  ", o$lbl)
  tryCatch(
    run_rdd(art_2020[[o$var]], art_2020$days_from_2020, cl_2020, 1,
            label = paste0(o$lbl, " (linear, clustered)")),
    error = function(e) { message("    SKIP: ", e$message); NULL }
  )
}))
results_2020[, cutoff := "2020-07-01"]

message("=== H4: heterogeneity by content family ===")

het_outcomes <- list(
  list(var = "one_click_rate", lbl = "One-Click"),
  list(var = "like_rate",      lbl = "Like"),
  list(var = "look_rate",      lbl = "Zaikan")
)

het_results <- data.table::rbindlist(lapply(names(family_gloss), function(fam) {
  fl <- family_gloss[[fam]]
  sub <- art_2020[content_family == fam]
  cl_sub <- sub$account_id
  data.table::rbindlist(lapply(het_outcomes, function(o) {
    message("  ", fl, " - ", o$lbl)
    tryCatch(
      run_rdd(sub[[o$var]], sub$days_from_2020, cl_sub, 1, label = paste0(o$lbl, " [", fl, "]")),
      error = function(e) { message("    SKIP: ", e$message); NULL }
    )
  }))
}))
het_results[, cutoff := "2020 (by family)"]

message("=== Robustness: bandwidth sensitivity (2018, one-click) ===")

bw_main_2018 <- rdrobust::rdrobust(art_2018$one_click_rate, art_2018$days_from_2018,
                                   cluster = cl_2018, bwselect = "mserd")$bws[1, 1]

bw_mult <- c(0.5, 0.75, 1.0, 1.5, 2.0)
robust_bw_2018 <- data.table::rbindlist(lapply(bw_mult, function(m) {
  h <- bw_main_2018 * m
  tryCatch(
    run_rdd(art_2018$one_click_rate, art_2018$days_from_2018, cl_2018, 1, h = h,
            label = paste0("2018 BW = ", round(h, 1), " (", m, "x)")),
    error = function(e) { message("    SKIP: ", e$message); NULL }
  )
}))

bw_main_2020 <- rdrobust::rdrobust(art_2020$one_click_rate, art_2020$days_from_2020,
                                   cluster = cl_2020, bwselect = "mserd")$bws[1, 1]

robust_bw_2020 <- data.table::rbindlist(lapply(bw_mult, function(m) {
  h <- bw_main_2020 * m
  tryCatch(
    run_rdd(art_2020$one_click_rate, art_2020$days_from_2020, cl_2020, 1, h = h,
            label = paste0("2020 BW = ", round(h, 1), " (", m, "x)")),
    error = function(e) { message("    SKIP: ", e$message); NULL }
  )
}))

message("=== Placebo cutoffs ===")

placebo_dates_2018 <- as.Date(c("2018-06-21", "2018-09-21", "2019-03-21", "2019-06-21"))
placebo_2018 <- data.table::rbindlist(lapply(placebo_dates_2018, function(d) {
  dt_tmp <- dt[abs(as.integer(publish_date - d)) <= max_window]
  x_tmp <- as.integer(dt_tmp$publish_date - d)
  tryCatch(
    run_rdd(dt_tmp$one_click_rate, x_tmp, dt_tmp$account_id, 1,
            label = format(d, "%Y-%m-%d")),
    error = function(e) { message("    SKIP ", d, ": ", e$message); NULL }
  )
}))

message("=== Polynomial order sensitivity ===")

poly_robust <- data.table::rbindlist(lapply(c(0L, 1L, 2L), function(pp) {
  rbind(
    tryCatch(run_rdd(art_2018$one_click_rate, art_2018$days_from_2018, cl_2018, pp,
                     label = paste0("2018 One-Click (p=", pp, ")")),
             error = function(e) NULL),
    tryCatch(run_rdd(art_2020$one_click_rate, art_2020$days_from_2020, cl_2020, pp,
                     label = paste0("2020 One-Click (p=", pp, ")")),
             error = function(e) NULL)
  )
}))

message("=== Donut hole (exclude days near cutoff) ===")

donut_sizes <- c(1L, 2L, 3L)
donut_robust <- data.table::rbindlist(lapply(donut_sizes, function(d) {
  a18 <- art_2018[abs(days_from_2018) > d]
  a20 <- art_2020[abs(days_from_2020) > d]
  rbind(
    tryCatch(run_rdd(a18$one_click_rate, a18$days_from_2018, a18$account_id, 1,
                     label = paste0("2018 donut +/-", d)),
             error = function(e) NULL),
    tryCatch(run_rdd(a20$one_click_rate, a20$days_from_2020, a20$account_id, 1,
                     label = paste0("2020 donut +/-", d)),
             error = function(e) NULL)
  )
}))

message("=== Alternative kernels ===")

run_rdd_kernel <- function(y, x, cl, kernel_name, label) {
  tryCatch({
    fit <- rdrobust::rdrobust(y = y, x = x, cluster = cl, p = 1,
                              kernel = kernel_name, bwselect = "mserd", all = TRUE)
    data.table::data.table(
      label = label, tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
      bw_l = fit$bws[1, 1], bw_r = fit$bws[1, 2],
      n_l = fit$N_h[1], n_r = fit$N_h[2], p = 1L
    )
  }, error = function(e) NULL)
}

kernel_robust <- data.table::rbindlist(list(
  run_rdd_kernel(art_2018$one_click_rate, art_2018$days_from_2018, cl_2018, "triangular", "2018 triangular"),
  run_rdd_kernel(art_2018$one_click_rate, art_2018$days_from_2018, cl_2018, "uniform",     "2018 uniform"),
  run_rdd_kernel(art_2018$one_click_rate, art_2018$days_from_2018, cl_2018, "epanechnikov","2018 epanechnikov"),
  run_rdd_kernel(art_2020$one_click_rate, art_2020$days_from_2020, cl_2020, "triangular",  "2020 triangular"),
  run_rdd_kernel(art_2020$one_click_rate, art_2020$days_from_2020, cl_2020, "uniform",     "2020 uniform"),
  run_rdd_kernel(art_2020$one_click_rate, art_2020$days_from_2020, cl_2020, "epanechnikov","2020 epanechnikov")
))

message("=== CER-optimal bandwidth ===")

cer_robust <- data.table::rbindlist(list(
  tryCatch({
    fit <- rdrobust::rdrobust(art_2018$one_click_rate, art_2018$days_from_2018,
                              cluster = cl_2018, p = 1, bwselect = "cerrd", all = TRUE)
    data.table::data.table(label = "2018 CER-optimal", tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo = fit$ci[3,1], ci_hi = fit$ci[3,2], pvalue = fit$pv[3],
      bw_l = fit$bws[1,1], bw_r = fit$bws[1,2], n_l = fit$N_h[1], n_r = fit$N_h[2], p = 1L)
  }, error = function(e) NULL),
  tryCatch({
    fit <- rdrobust::rdrobust(art_2020$one_click_rate, art_2020$days_from_2020,
                              cluster = cl_2020, p = 1, bwselect = "cerrd", all = TRUE)
    data.table::data.table(label = "2020 CER-optimal", tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo = fit$ci[3,1], ci_hi = fit$ci[3,2], pvalue = fit$pv[3],
      bw_l = fit$bws[1,1], bw_r = fit$bws[1,2], n_l = fit$N_h[1], n_r = fit$N_h[2], p = 1L)
  }, error = function(e) NULL)
))

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
  "RDD at December 2018 (H2). Article-level, account-clustered SEs, triangular kernel, MSE-optimal bandwidth, robust bias-corrected.",
  "tab:rdd-2018", file.path(paths$tables, "rdd_2018_main.tex"))

fmt_tex(results_2020,
  "RDD at July 2020 (H3). Article-level, account-clustered SEs, triangular kernel, MSE-optimal bandwidth, robust bias-corrected.",
  "tab:rdd-2020", file.path(paths$tables, "rdd_2020_main.tex"))

fmt_tex(het_results,
  "Heterogeneous RDD at July 2020 by content family (H4). Account-clustered SEs.",
  "tab:rdd-2020-het", file.path(paths$tables, "rdd_2020_heterogeneity.tex"))

all_robustness <- data.table::rbindlist(list(
  robust_bw_2018, robust_bw_2020,
  poly_robust, donut_robust, kernel_robust, cer_robust
), fill = TRUE)

fmt_tex(all_robustness,
  "Robustness checks for one-click engagement rate. Panel A: bandwidth sensitivity (0.5x--2x MSE-optimal). Panel B: polynomial order (p=0,1,2). Panel C: donut hole (drop 1--3 days). Panel D: alternative kernels. Panel E: CER-optimal bandwidth. All account-clustered.",
  "tab:rdd-robustness", file.path(paths$tables, "rdd_robustness_all.tex"))

fmt_tex(placebo_2018,
  "Placebo cutoff tests for one-click rate at non-event dates. Account-clustered SEs.",
  "tab:rdd-placebo", file.path(paths$tables, "rdd_placebo_cutoffs.tex"))

# ── Figures: all use daily-aggregated data ────────────────────

message("=== RD Figures ===")

rd_x_breaks <- seq(-max_window, max_window, by = 10)

rd_theme <- ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "grey92"),
    axis.title       = ggplot2::element_text(size = 10.5),
    plot.margin      = ggplot2::margin(t = 6, r = 8, b = 6, l = 6)
  )

rd_x_scale <- ggplot2::scale_x_continuous(breaks = rd_x_breaks)

make_ci_ribbon <- function(d, yvar, p_order = 1) {
  d <- data.table::copy(d)
  d[, y := get(yvar)]
  if (nrow(d) < p_order + 2) return(data.table::data.table())
  fit <- lm(y ~ poly(x, p_order, raw = TRUE), data = d, weights = n)
  grid <- data.table::data.table(x = seq(min(d$x), max(d$x), length.out = 200))
  pred <- predict(fit, grid, se.fit = TRUE)
  grid[, `:=`(ci_l = pred$fit - 1.96 * pred$se.fit,
              ci_r = pred$fit + 1.96 * pred$se.fit)]
  grid
}

save_rd <- function(daily, yvar, y_label, filename) {
  d <- data.table::copy(daily)
  d[, y := get(yvar)]

  rd <- rdrobust::rdplot(d$y, d$x, binselect = "esmv", p = 1, ci = 95, hide = TRUE)
  bins <- data.table::as.data.table(rd$vars_bins)
  poly <- data.table::as.data.table(rd$vars_poly)
  poly_left  <- poly[rdplot_x < -0.5]
  poly_right <- poly[rdplot_x >  0.5]

  ribbon_l <- make_ci_ribbon(d[x < 0], yvar)[, side := "left"]
  ribbon_r <- make_ci_ribbon(d[x > 0], yvar)[, side := "right"]
  ribbon <- rbind(ribbon_l, ribbon_r)

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = ribbon,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = side),
      fill = "grey75", alpha = 0.4
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y),
      shape = 21, size = 2.2, fill = "white", color = "grey30", stroke = 0.5
    ) +
    ggplot2::geom_line(data = poly_left,
      ggplot2::aes(x = rdplot_x, y = rdplot_y),
      color = "black", linewidth = 0.9
    ) +
    ggplot2::geom_line(data = poly_right,
      ggplot2::aes(x = rdplot_x, y = rdplot_y),
      color = "black", linewidth = 0.9
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey40") +
    rd_x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Days relative to cutoff", y = y_label) +
    rd_theme

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 6.5, height = 4.5, units = "in", device = "pdf")
}

save_rd(daily_2018, "one_click_rate", "One-Click Engagement Rate", "rdd_2018_one_click_rate.pdf")
save_rd(daily_2018, "share_rate", "Share/Forward Rate", "rdd_2018_share_rate.pdf")
save_rd(daily_2020, "one_click_rate", "One-Click Rate (Like + Zaikan)", "rdd_2020_one_click_rate.pdf")
save_rd(daily_2020, "look_rate", "Zaikan Rate (high-visibility)", "rdd_2020_look_rate.pdf")
save_rd(daily_2020, "share_rate", "Share/Forward Rate", "rdd_2020_share_rate.pdf")

message("=== 2020 Unbundling Plot ===")

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
  grid[, rdplot_y := pred]
  grid[, rdplot_x := x]
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
  ggplot2::geom_ribbon(
    data = ribbon_ub,
    ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = metric),
    fill = "grey75", alpha = 0.35
  ) +
  ggplot2::geom_point(
    data = bins_ub_all,
    ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y, shape = metric),
    size = 2.2, fill = "white", color = "grey30", stroke = 0.5
  ) +
  ggplot2::geom_line(
    data = poly_ub,
    ggplot2::aes(x = rdplot_x, y = rdplot_y, linetype = metric, group = metric),
    color = "black", linewidth = 0.9
  ) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey40") +
  ggplot2::scale_shape_manual(values = shape_map, name = NULL) +
  ggplot2::scale_linetype_manual(values = lty_map, name = NULL) +
  rd_x_scale +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::labs(x = "Days relative to cutoff", y = "Engagement rate") +
  rd_theme +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(file.path(paths$figures, "rdd_2020_unbundling.pdf"), g_ub,
                width = 6.5, height = 5, units = "in", device = "pdf")

message("=== By-family panels ===")

save_family_panels <- function(art, rv, yvar, y_label, filename) {
  daily_fam <- art[content_family %in% family_order, .(
    n = .N, y = mean(get(yvar), na.rm = TRUE)
  ), by = .(
    x = get(rv),
    family = factor(content_family, levels = family_order, labels = family_gloss[family_order])
  )]

  fam_levels <- levels(daily_fam$family)
  all_bins <- list()
  all_poly <- list()
  all_ribbon <- list()

  for (f in fam_levels) {
    fd <- daily_fam[family == f]
    rd_f <- tryCatch(
      rdrobust::rdplot(fd$y, fd$x, binselect = "esmv", p = 1, ci = 95, hide = TRUE),
      error = function(e) NULL
    )
    if (is.null(rd_f)) next

    b <- data.table::as.data.table(rd_f$vars_bins)[, family := f]
    p <- data.table::as.data.table(rd_f$vars_poly)[, family := f]
    p <- p[abs(rdplot_x) > 0.5]
    p[, side := ifelse(rdplot_x < 0, "left", "right")]

    rl <- make_ci_ribbon(fd[x < 0], "y")[, `:=`(side = "left", family = f)]
    rr <- make_ci_ribbon(fd[x > 0], "y")[, `:=`(side = "right", family = f)]

    all_bins[[f]] <- b
    all_poly[[f]] <- p
    all_ribbon[[f]] <- rbind(rl, rr)
  }

  bins_all   <- data.table::rbindlist(all_bins)
  poly_all   <- data.table::rbindlist(all_poly)
  ribbon_all <- data.table::rbindlist(all_ribbon)

  bins_all[, family := factor(family, levels = fam_levels)]
  poly_all[, family := factor(family, levels = fam_levels)]
  ribbon_all[, family := factor(family, levels = fam_levels)]

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = ribbon_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = side),
      fill = "grey75", alpha = 0.4
    ) +
    ggplot2::geom_point(
      data = bins_all,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y),
      shape = 21, size = 1.8, fill = "white", color = "grey30", stroke = 0.45
    ) +
    ggplot2::geom_line(
      data = poly_all,
      ggplot2::aes(x = rdplot_x, y = rdplot_y, group = side),
      color = "black", linewidth = 0.8
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey40") +
    ggplot2::facet_wrap(~family, ncol = 2, scales = "free_y") +
    rd_x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Days relative to cutoff", y = y_label) +
    rd_theme +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = 10))

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 7, height = 5.5, units = "in", device = "pdf")
}

save_family_panels(art_2018, "days_from_2018", "one_click_rate",
                   "One-Click Engagement Rate", "rdd_2018_one_click_by_family.pdf")
save_family_panels(art_2020, "days_from_2020", "one_click_rate",
                   "One-Click Rate (Like + Zaikan)", "rdd_2020_one_click_by_family.pdf")

message("=== 2020 Unbundling by family ===")

save_unbundling_by_family <- function(art, filename, nbins = 8) {
  fam_labels <- family_gloss[family_order]

  all_bins <- list()
  all_ribbon <- list()
  all_poly <- list()

  for (i in seq_along(fam_labels)) {
    fam_code <- family_order[i]
    fl <- fam_labels[i]
    sub <- art[content_family == fam_code]
    daily_f <- sub[, .(
      n = .N,
      one_click_rate = mean(one_click_rate, na.rm = TRUE),
      like_rate = mean(like_rate, na.rm = TRUE),
      look_rate = mean(look_rate, na.rm = TRUE)
    ), by = .(x = days_from_2020)]

    dl <- daily_f[x < 0]
    dr <- daily_f[x > 0]

    all_bins[[paste0(fl, "_c")]] <- ub_bin(dl, "one_click_rate", nbins)[, `:=`(metric = "Combined", family = fl)]
    all_bins[[paste0(fl, "_l")]] <- ub_bin(dr, "like_rate", nbins)[, `:=`(metric = "Like", family = fl)]
    all_bins[[paste0(fl, "_z")]] <- ub_bin(dr, "look_rate", nbins)[, `:=`(metric = "Zaikan", family = fl)]

    all_ribbon[[paste0(fl, "_c")]] <- make_ci_ribbon(dl, "one_click_rate")[, `:=`(metric = "Combined", family = fl)]
    all_ribbon[[paste0(fl, "_l")]] <- make_ci_ribbon(dr, "like_rate")[, `:=`(metric = "Like", family = fl)]
    all_ribbon[[paste0(fl, "_z")]] <- make_ci_ribbon(dr, "look_rate")[, `:=`(metric = "Zaikan", family = fl)]

    all_poly[[paste0(fl, "_c")]] <- ub_fit(dl, "one_click_rate")[, `:=`(metric = "Combined", family = fl)]
    all_poly[[paste0(fl, "_l")]] <- ub_fit(dr, "like_rate")[, `:=`(metric = "Like", family = fl)]
    all_poly[[paste0(fl, "_z")]] <- ub_fit(dr, "look_rate")[, `:=`(metric = "Zaikan", family = fl)]
  }

  bins_all   <- data.table::rbindlist(all_bins, fill = TRUE)
  ribbon_all <- data.table::rbindlist(all_ribbon, fill = TRUE)
  poly_all   <- data.table::rbindlist(all_poly, fill = TRUE)

  fac <- function(d) { d[, family := factor(family, levels = fam_labels)]; d }
  bins_all   <- fac(bins_all)
  ribbon_all <- fac(ribbon_all)
  poly_all   <- fac(poly_all)

  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = ribbon_all,
      ggplot2::aes(x = x, ymin = ci_l, ymax = ci_r, group = metric),
      fill = "grey75", alpha = 0.35
    ) +
    ggplot2::geom_point(
      data = bins_all,
      ggplot2::aes(x = rdplot_mean_x, y = rdplot_mean_y, shape = metric),
      size = 1.8, fill = "white", color = "grey30", stroke = 0.45
    ) +
    ggplot2::geom_line(
      data = poly_all,
      ggplot2::aes(x = rdplot_x, y = rdplot_y, linetype = metric, group = metric),
      color = "black", linewidth = 0.8
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, color = "grey40") +
    ggplot2::facet_wrap(~family, ncol = 2, scales = "free_y") +
    ggplot2::scale_shape_manual(values = shape_map, name = NULL) +
    ggplot2::scale_linetype_manual(values = lty_map, name = NULL) +
    rd_x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Days relative to cutoff", y = "Engagement rate") +
    rd_theme +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      legend.position = "bottom"
    )

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 7, height = 6, units = "in", device = "pdf")
}

save_unbundling_by_family(art_2020, "rdd_2020_unbundling_by_family.pdf")

# ── Validity ──────────────────────────────────────────────────

message("=== McCrary density plots ===")

mccrary_window <- 365L

mccrary_density <- function(x, c = 0, bin = NULL, bw = NULL) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (is.null(bin)) bin <- 2 * sd(x) * n^(-0.5)
  breaks_l <- rev(seq(c, min(x) - bin, by = -bin))
  breaks_r <- seq(c, max(x) + bin, by = bin)
  breaks <- sort(unique(c(breaks_l, breaks_r)))
  mids <- (breaks[-length(breaks)] + breaks[-1]) / 2
  counts <- as.integer(table(cut(x, breaks = breaks, include.lowest = TRUE)))
  cellval <- counts / (n * bin)

  if (is.null(bw)) {
    bw <- rddensity::rddensity(x, c = c)$h$left
    bw <- max(bw, 3 * bin)
  }

  fit_side <- function(mp, cv, side_flag) {
    idx <- side_flag
    if (sum(idx) < 4) return(list(grid = numeric(0), fitted = numeric(0)))
    mp_s <- mp[idx]; cv_s <- cv[idx]
    grid <- seq(min(mp_s), max(mp_s), length.out = 200)
    w_mat <- sapply(grid, function(g) {
      u <- (mp_s - g) / bw
      k <- ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
      k / max(sum(k), 1e-10)
    })
    fitted <- colSums(w_mat * cv_s)
    list(grid = grid, fitted = fitted)
  }

  fl <- fit_side(mids, cellval, mids < c)
  fr <- fit_side(mids, cellval, mids >= c)

  list(
    cellmp = mids, cellval = cellval,
    fit_l = fl, fit_r = fr,
    bw = bw, bin = bin
  )
}

save_mccrary_plot <- function(cutoff_date, cutoff_label, filename) {
  x_all <- as.integer(dt$publish_date - cutoff_date)
  x_wide <- x_all[abs(x_all) <= mccrary_window]

  dens_test <- rddensity::rddensity(x_wide, c = 0)
  pval <- dens_test$test$p_jk

  mc <- mccrary_density(x_wide, c = 0)

  bar_dt <- data.table::data.table(mid = mc$cellmp, height = mc$cellval)
  fit_l_dt <- data.table::data.table(x = mc$fit_l$grid, y = mc$fit_l$fitted)
  fit_r_dt <- data.table::data.table(x = mc$fit_r$grid, y = mc$fit_r$fitted)

  g <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = bar_dt,
      ggplot2::aes(x = mid, y = height),
      fill = "grey80", color = "grey50", width = mc$bin * 0.9
    ) +
    ggplot2::geom_line(data = fit_l_dt, ggplot2::aes(x = x, y = y),
                       color = "black", linewidth = 1.1) +
    ggplot2::geom_line(data = fit_r_dt, ggplot2::aes(x = x, y = y),
                       color = "black", linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.6) +
    ggplot2::scale_x_continuous(breaks = seq(-mccrary_window, mccrary_window, by = 90)) +
    ggplot2::labs(
      x = "Days relative to cutoff",
      y = "Density",
      title = paste0(cutoff_label,
                     ":  p = ", formatC(pval, format = "f", digits = 3))
    ) +
    rd_theme +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 7, height = 4.5, units = "in", device = "pdf")
}

save_mccrary_plot(cutoff_2018, "Dec 2018", "rdd_mccrary_2018.pdf")
save_mccrary_plot(cutoff_2020, "Jul 2020", "rdd_mccrary_2020.pdf")

message("=== Balance tests ===")

safe_rdd <- function(...) tryCatch(run_rdd(...), error = function(e) { message("    SKIP: ", e$message); NULL })

balance_all <- data.table::rbindlist(list(
  safe_rdd(art_2018$read_num, art_2018$days_from_2018, cl_2018, 1, label = "Reads (2018)"),
  safe_rdd(art_2020$read_num, art_2020$days_from_2020, cl_2020, 1, label = "Reads (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "hard_propaganda"), art_2018$days_from_2018, cl_2018, 1, label = "Hard prop. (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "hard_propaganda"), art_2020$days_from_2020, cl_2020, 1, label = "Hard prop. (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "public_service"), art_2018$days_from_2018, cl_2018, 1, label = "Pub. service (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "public_service"), art_2020$days_from_2020, cl_2020, 1, label = "Pub. service (2020)")
))

fmt_tex(balance_all, "Covariate balance at each cutoff. Account-clustered SEs.",
  "tab:rdd-balance", file.path(paths$tables, "rdd_balance.tex"))

message("All RDD outputs saved.")
