bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "rddensity"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

dt <- load_rdd_data(paths)
cutoff_2018 <- as.Date("2018-12-21")
cutoff_2020 <- as.Date("2020-07-01")
max_window <- 30L
covid_pat <- "疫情|新冠|肺炎|防控|核酸|隔离|口罩|疫苗|确诊|无症状|健康码|行程码"

dt[, article_rank := seq_len(.N), by = .(account_id, publish_date)]
dt[, is_head := article_rank == 1L]
dt[, title_length := nchar(title)]
dt[, covid_keyword_flag := grepl(covid_pat, paste(title, keywords), perl = TRUE)]
dt[, emergency_risk_flag := category == "应急管理与风险沟通"]

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]
cl_2018 <- art_2018$account_id
cl_2020 <- art_2020$account_id

# ── McCrary density plots (DCdensity-style, ±30 days) ────────

message("=== McCrary density plots ===")

mccrary_plot_bin <- 2

mccrary_density <- function(x, c = 0, bin = NULL, bw = NULL) {
  x <- x[stats::complete.cases(x)]
  n <- length(x)
  x_sd <- stats::sd(x)
  x_min <- min(x); x_max <- max(x)
  if (is.null(bin)) bin <- 2 * x_sd * n^(-0.5)

  left_edge <- floor((x_min - c) / bin) * bin + bin / 2 + c
  right_edge <- floor((x_max - c) / bin) * bin + bin / 2 + c
  n_bins <- floor((x_max - x_min) / bin) + 2L

  bin_num <- round((((floor((x - c) / bin) * bin + bin / 2 + c) - left_edge) / bin) + 1)
  cellval <- tabulate(bin_num, nbins = n_bins)
  cellval <- (cellval / n) / bin
  cellmp <- seq_len(n_bins)
  cellmp <- floor(((left_edge + (cellmp - 1) * bin) - c) / bin) * bin + bin / 2 + c

  if (is.null(bw)) {
    calc_side_bw <- function(side_flag, support_length) {
      mp_side <- cellmp[side_flag]; val_side <- cellval[side_flag]
      if (length(mp_side) < 5) return(NA_real_)
      fit <- stats::lm(val_side ~ poly(mp_side, degree = 4, raw = TRUE))
      coefs <- stats::coef(fit); coefs[is.na(coefs)] <- 0
      coefs <- c(coefs, rep(0, max(0, 5 - length(coefs))))
      mse4 <- summary(fit)$sigma^2
      fpp <- 2*coefs[3] + 6*coefs[4]*mp_side + 12*coefs[5]*mp_side*mp_side
      denom <- sum(fpp*fpp)
      if (!is.finite(mse4) || !is.finite(denom) || denom <= 0) return(NA_real_)
      3.348 * (mse4 * support_length / denom)^(1/5)
    }
    hleft <- calc_side_bw(cellmp < c, c - left_edge)
    hright <- calc_side_bw(cellmp >= c, right_edge - c)
    bw <- mean(c(hleft, hright), na.rm = TRUE)
    if (!is.finite(bw) || bw <= 0) bw <- max(3 * bin, x_sd / 4)
  }

  tri_kernel <- function(dist) { u <- abs(dist/bw); ifelse(u <= 1, 1-u, 0) }

  fit_side <- function(side_flag) {
    side_dt <- data.table::data.table(cellmp = cellmp[side_flag], cellval = cellval[side_flag])
    side_dt$est <- NA_real_; side_dt$lwr <- NA_real_; side_dt$upr <- NA_real_
    if (nrow(side_dt) < 2) return(side_dt)
    for (i in seq_len(nrow(side_dt))) {
      dist <- side_dt$cellmp - side_dt$cellmp[i]
      weights <- tri_kernel(dist)
      if (sum(weights > 0) < 3) next
      fit <- tryCatch(stats::lm(cellval ~ dist, weights = weights,
                                data = data.frame(cellval = side_dt$cellval, dist = dist)),
                      error = function(e) NULL)
      if (is.null(fit)) next
      pred <- tryCatch(stats::predict(fit, interval = "confidence", newdata = data.frame(dist = 0)),
                       error = function(e) NULL)
      if (is.null(pred)) next
      side_dt$est[i] <- pred[1,1]; side_dt$lwr[i] <- pred[1,2]; side_dt$upr[i] <- pred[1,3]
    }
    side_dt
  }

  d_left <- fit_side(cellmp < c); d_right <- fit_side(cellmp >= c)

  y_vals <- c(cellval, d_left$est, d_left$lwr, d_left$upr, d_right$est, d_right$lwr, d_right$upr)
  y_vals <- y_vals[is.finite(y_vals)]

  list(points = data.table::data.table(cellmp = cellmp, cellval = cellval),
       fit_left = d_left, fit_right = d_right, bin = bin, bw = bw,
       ylim = range(y_vals))
}

save_mccrary_plot <- function(cutoff_date, cutoff_label, filename) {
  x_all <- as.integer(dt$publish_date - cutoff_date)
  x_wide <- x_all[abs(x_all) <= max_window]

  mc <- mccrary_density(x_wide, c = 0, bin = mccrary_plot_bin)
  point_dt <- mc$points[mc$points$cellval > 0]
  dens_test <- rddensity::rddensity(x_wide, c = 0)
  p_val <- dens_test$test$p_jk
  p_lab <- if (is.finite(p_val) && p_val < 0.001) "p < 0.001" else paste0("p = ", formatC(p_val, format = "f", digits = 3))

  open_pdf_figure(file.path(paths$figures, filename), width = 5.2, height = 5.2)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(old_par); grDevices::dev.off() }, add = TRUE)

  graphics::par(mar = c(4.2, 4.2, 0.8, 1.0))
  graphics::plot(mc$fit_left$cellmp, mc$fit_left$est, type = "l", lty = 1, lwd = 1.8, col = "black",
                 xlim = c(-max_window, max_window), ylim = mc$ylim,
                 xlab = "Days relative to cutoff", ylab = "Density", main = "")
  graphics::lines(mc$fit_left$cellmp, mc$fit_left$lwr, lty = 2, lwd = 0.9, col = "black")
  graphics::lines(mc$fit_left$cellmp, mc$fit_left$upr, lty = 2, lwd = 0.9, col = "black")
  graphics::lines(mc$fit_right$cellmp, mc$fit_right$est, lty = 1, lwd = 1.8, col = "black")
  graphics::lines(mc$fit_right$cellmp, mc$fit_right$lwr, lty = 2, lwd = 0.9, col = "black")
  graphics::lines(mc$fit_right$cellmp, mc$fit_right$upr, lty = 2, lwd = 0.9, col = "black")
  graphics::points(point_dt$cellmp, point_dt$cellval, pch = 20, cex = 0.8)
  usr <- graphics::par("usr")
  graphics::text(x = usr[1] + 0.04*(usr[2]-usr[1]), y = usr[4] - 0.04*(usr[4]-usr[3]),
                 labels = p_lab, adj = c(0,1), cex = 0.95)
}

save_mccrary_plot(cutoff_2018, "Dec 2018", "rdd_mccrary_2018.pdf")
save_mccrary_plot(cutoff_2020, "Jul 2020", "rdd_mccrary_2020.pdf")

# ── Balance tests ─────────────────────────────────────────────

message("=== Balance tests ===")

balance_core <- data.table::rbindlist(list(
  safe_rdd(art_2018$read_num, art_2018$days_from_2018, cl_2018, 1, label = "Reads (2018)"),
  safe_rdd(art_2020$read_num, art_2020$days_from_2020, cl_2020, 1, label = "Reads (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "hard_propaganda"), art_2018$days_from_2018, cl_2018, 1, label = "Hard prop. (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "hard_propaganda"), art_2020$days_from_2020, cl_2020, 1, label = "Hard prop. (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "public_service"), art_2018$days_from_2018, cl_2018, 1, label = "Pub. service (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "public_service"), art_2020$days_from_2020, cl_2020, 1, label = "Pub. service (2020)")
))

fmt_rdd_tex(balance_core, "Covariate balance at each cutoff. Account-clustered SEs.",
  "tab:rdd-balance", file.path(paths$tables, "rdd_balance.tex"))

balance_extended <- data.table::rbindlist(list(
  safe_rdd(art_2018$read_num, art_2018$days_from_2018, cl_2018, 1, label = "Reads (2018)"),
  safe_rdd(art_2020$read_num, art_2020$days_from_2020, cl_2020, 1, label = "Reads (2020)"),
  safe_rdd(art_2018$title_length, art_2018$days_from_2018, cl_2018, 1, label = "Title length (2018)"),
  safe_rdd(art_2020$title_length, art_2020$days_from_2020, cl_2020, 1, label = "Title length (2020)"),
  safe_rdd(as.numeric(art_2018$is_head), art_2018$days_from_2018, cl_2018, 1, label = "Head article share (2018)"),
  safe_rdd(as.numeric(art_2020$is_head), art_2020$days_from_2020, cl_2020, 1, label = "Head article share (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "hard_propaganda"), art_2018$days_from_2018, cl_2018, 1, label = "Hard prop. (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "hard_propaganda"), art_2020$days_from_2020, cl_2020, 1, label = "Hard prop. (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "soft_propaganda"), art_2018$days_from_2018, cl_2018, 1, label = "Soft prop. (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "soft_propaganda"), art_2020$days_from_2020, cl_2020, 1, label = "Soft prop. (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "state_governance"), art_2018$days_from_2018, cl_2018, 1, label = "State gov. (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "state_governance"), art_2020$days_from_2020, cl_2020, 1, label = "State gov. (2020)"),
  safe_rdd(as.numeric(art_2018$content_family == "public_service"), art_2018$days_from_2018, cl_2018, 1, label = "Pub. service (2018)"),
  safe_rdd(as.numeric(art_2020$content_family == "public_service"), art_2020$days_from_2020, cl_2020, 1, label = "Pub. service (2020)"),
  safe_rdd(as.numeric(art_2018$emergency_risk_flag), art_2018$days_from_2018, cl_2018, 1, label = "Emergency/risk share (2018)"),
  safe_rdd(as.numeric(art_2020$emergency_risk_flag), art_2020$days_from_2020, cl_2020, 1, label = "Emergency/risk share (2020)"),
  safe_rdd(as.numeric(art_2018$covid_keyword_flag), art_2018$days_from_2018, cl_2018, 1, label = "COVID keyword share (2018)"),
  safe_rdd(as.numeric(art_2020$covid_keyword_flag), art_2020$days_from_2020, cl_2020, 1, label = "COVID keyword share (2020)")
))

fmt_rdd_tex(balance_extended,
  "Extended balance and composition diagnostics at each cutoff. Account-clustered SEs.",
  "tab:rdd-balance-extended", file.path(paths$tables, "appendix_rdd_balance_extended.tex"))

# ── Placebo outcomes ──────────────────────────────────────────

message("=== Placebo outcomes ===")

placebo_outcomes <- data.table::rbindlist(list(
  safe_rdd(art_2018$read_num,     art_2018$days_from_2018, cl_2018, 1, label = "Reads (2018)"),
  safe_rdd(art_2020$read_num,     art_2020$days_from_2020, cl_2020, 1, label = "Reads (2020)"),
  safe_rdd(art_2018$collect_rate, art_2018$days_from_2018, cl_2018, 1, label = "Collect Rate (2018)"),
  safe_rdd(art_2020$collect_rate, art_2020$days_from_2020, cl_2020, 1, label = "Collect Rate (2020)"),
  safe_rdd(art_2018$share_rate,   art_2018$days_from_2018, cl_2018, 1, label = "Share Rate (2018)"),
  safe_rdd(art_2020$share_rate,   art_2020$days_from_2020, cl_2020, 1, label = "Share Rate (2020)")
))

fmt_rdd_tex(placebo_outcomes,
  "Placebo outcomes at each cutoff. Reads and collect rate should be unaffected by footer-button reforms.",
  "tab:placebo-outcomes", file.path(paths$tables, "appendix_placebo_outcomes.tex"))

message("Saved McCrary density plots, balance table, and placebo outcome table.")
