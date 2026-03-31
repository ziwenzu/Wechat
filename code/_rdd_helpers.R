safe_rdd <- function(y, x, cl = NULL, p = 1, h = NULL, label = "") {
  tryCatch({
    args <- list(y = y, x = x, p = p, kernel = "triangular", all = TRUE)
    if (!is.null(cl)) args$cluster <- cl
    if (!is.null(h)) { args$h <- h; args$b <- h } else { args$bwselect <- "mserd" }
    fit <- do.call(rdrobust::rdrobust, args)
    data.table::data.table(
      label = label, tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
      bw_l = fit$bws[1, 1], bw_r = fit$bws[1, 2],
      n_l = fit$N_h[1], n_r = fit$N_h[2], p = p
    )
  }, error = function(e) { message("  SKIP: ", label, " - ", e$message); NULL })
}

fmt_rdd_tex <- function(res, caption, label, filepath) {
  tex_dt <- res[, .(
    Specification = label, Estimate = tau_rb, SE = se_rb, `p-value` = pvalue,
    BW = round((bw_l + bw_r) / 2), `N (L)` = n_l, `N (R)` = n_r
  )]
  write_tex_table(tex_dt, caption = caption, label = label, path = filepath,
    digits = c(Estimate = 4L, SE = 4L, `p-value` = 3L, BW = 0L, `N (L)` = 0L, `N (R)` = 0L),
    align = paste0("l", paste(rep("r", 6), collapse = "")))
}

rd_theme <- ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(color = "grey92"),
                 axis.title = ggplot2::element_text(size = 10.5),
                 plot.margin = ggplot2::margin(t = 6, r = 8, b = 6, l = 6))

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

safe_rdd_covs <- function(y, x, cl = NULL, covs = NULL, p = 1, h = NULL, label = "") {
  tryCatch({
    args <- list(y = y, x = x, p = p, kernel = "triangular", all = TRUE)
    if (!is.null(cl)) args$cluster <- cl
    if (!is.null(covs)) args$covs <- covs
    if (!is.null(h)) { args$h <- h; args$b <- h } else { args$bwselect <- "mserd" }
    fit <- do.call(rdrobust::rdrobust, args)
    data.table::data.table(
      label = label, tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
      bw_l = fit$bws[1, 1], bw_r = fit$bws[1, 2],
      n_l = fit$N_h[1], n_r = fit$N_h[2], p = p
    )
  }, error = function(e) { message("  SKIP: ", label, " - ", e$message); NULL })
}

load_rdd_data <- function(paths) {
  dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))
  dt[, days_from_2018 := as.integer(publish_date - as.Date("2018-12-21"))]
  dt[, days_from_2020 := as.integer(publish_date - as.Date("2020-07-01"))]
  dt[, one_click_rate := like_rate + look_rate]
  dt
}
