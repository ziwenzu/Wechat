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
cutoff_2019 <- as.Date("2019-03-15")
max_window  <- 30L

dt[, days_from_2018 := as.integer(publish_date - cutoff_2018)]
dt[, days_from_2020 := as.integer(publish_date - cutoff_2020)]
dt[, days_from_2019 := as.integer(publish_date - cutoff_2019)]
dt[, one_click_rate := like_rate + look_rate]

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]
art_2019 <- dt[abs(days_from_2019) <= max_window]

label_short <- c(
  "\u610f\u8bc6\u5f62\u6001\u4e0e\u5ba3\u4f20\u6559\u80b2" = "Ideological Edu.",
  "\u65f6\u653f\u4e0e\u9886\u5bfc\u6d3b\u52a8" = "Leadership",
  "\u516c\u5171\u670d\u52a1\u4fe1\u606f" = "Public Service",
  "\u793e\u4f1a\u4fdd\u969c\u4e0e\u516c\u5171\u798f\u5229" = "Welfare",
  "\u5e94\u6025\u7ba1\u7406\u4e0e\u98ce\u9669\u6c9f\u901a" = "Emergency",
  "\u653f\u7b56\u4e0e\u653f\u52a1\u516c\u5f00" = "Policy Discl.",
  "\u793e\u4f1a\u6cbb\u7406\u4e0e\u6267\u6cd5\u901a\u62a5" = "Governance",
  "\u7fa4\u4f17\u52a8\u5458\u4e0e\u793e\u4f1a\u53c2\u4e0e" = "Mobilization",
  "\u7ecf\u6d4e\u4e0e\u53d1\u5c55\u5efa\u8bbe" = "Econ. Dev.",
  "\u57ce\u5e02\u5f62\u8c61\u4e0e\u6587\u5316\u6d3b\u52a8" = "City Image"
)

safe_rdd <- function(y, x, cl = NULL, p = 1, label = "") {
  tryCatch({
    args <- list(y = y, x = x, p = p, kernel = "triangular", bwselect = "mserd", all = TRUE)
    if (!is.null(cl)) args$cluster <- cl
    fit <- do.call(rdrobust::rdrobust, args)
    data.table::data.table(
      label = label, tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
      bw_l = fit$bws[1, 1], bw_r = fit$bws[1, 2],
      n_l = fit$N_h[1], n_r = fit$N_h[2], p = p
    )
  }, error = function(e) { message("  SKIP: ", label, " - ", e$message); NULL })
}

fmt_tex <- function(res, caption, label, filepath) {
  tex_dt <- res[, .(
    Specification = label, Estimate = tau_rb, SE = se_rb, `p-value` = pvalue,
    `BW` = round((bw_l + bw_r) / 2), `N (L)` = n_l, `N (R)` = n_r
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
rd_x_scale <- ggplot2::scale_x_continuous(breaks = seq(-max_window, max_window, 10))

# ═══════════════════════════════════════════════════════════════
# 1. PLACEBO OUTCOMES: reads + collect at both cutoffs
# ═══════════════════════════════════════════════════════════════

message("=== 1. Placebo outcomes ===")

placebo_outcomes <- data.table::rbindlist(list(
  safe_rdd(art_2018$read_num,      art_2018$days_from_2018, art_2018$account_id, 1, "Reads (2018)"),
  safe_rdd(art_2020$read_num,      art_2020$days_from_2020, art_2020$account_id, 1, "Reads (2020)"),
  safe_rdd(art_2018$collect_rate,  art_2018$days_from_2018, art_2018$account_id, 1, "Collect Rate (2018)"),
  safe_rdd(art_2020$collect_rate,  art_2020$days_from_2020, art_2020$account_id, 1, "Collect Rate (2020)"),
  safe_rdd(art_2018$share_rate,    art_2018$days_from_2018, art_2018$account_id, 1, "Share Rate (2018)"),
  safe_rdd(art_2020$share_rate,    art_2020$days_from_2020, art_2020$account_id, 1, "Share Rate (2020)")
))

fmt_tex(placebo_outcomes,
  "Placebo outcomes at each cutoff. Reads and collect rate should be unaffected by footer-button reforms. Share at 2018 is an additional placebo. Account-clustered SEs.",
  "tab:placebo-outcomes", file.path(paths$tables, "appendix_placebo_outcomes.tex"))

# ═══════════════════════════════════════════════════════════════
# 2. 2019-03-15 PLACEBO RDD (haokan -> zaikan rename)
# ═══════════════════════════════════════════════════════════════

message("=== 2. 2019-03-15 placebo RDD ===")

placebo_2019 <- data.table::rbindlist(list(
  safe_rdd(art_2019$one_click_rate, art_2019$days_from_2019, art_2019$account_id, 1, "One-Click Rate (2019 rename)"),
  safe_rdd(art_2019$share_rate,     art_2019$days_from_2019, art_2019$account_id, 1, "Share Rate (2019 rename)"),
  safe_rdd(art_2019$read_num,       art_2019$days_from_2019, art_2019$account_id, 1, "Reads (2019 rename)")
))

fmt_tex(placebo_2019,
  "Placebo RDD at March 15, 2019 (haokan renamed to zaikan). No functional change; theory predicts no discontinuity.",
  "tab:placebo-2019", file.path(paths$tables, "appendix_placebo_2019.tex"))

# ═══════════════════════════════════════════════════════════════
# 3. CATEGORY-LEVEL (10) RDD HETEROGENEITY AT 2020
# ═══════════════════════════════════════════════════════════════

message("=== 3. Category-level heterogeneity ===")

categories <- sort(unique(art_2020$category))

cat_het <- data.table::rbindlist(lapply(categories, function(cat) {
  cat_en <- label_short[cat]
  if (is.na(cat_en)) cat_en <- cat
  sub <- art_2020[category == cat]
  cl <- sub$account_id
  message("  ", cat_en)
  data.table::rbindlist(list(
    safe_rdd(sub$one_click_rate, sub$days_from_2020, cl, 1, paste0("One-Click [", cat_en, "]")),
    safe_rdd(sub$like_rate,      sub$days_from_2020, cl, 1, paste0("Like [", cat_en, "]")),
    safe_rdd(sub$look_rate,      sub$days_from_2020, cl, 1, paste0("Zaikan [", cat_en, "]"))
  ))
}))

fmt_tex(cat_het,
  "Category-level RDD heterogeneity at July 2020 (10 categories). Account-clustered SEs.",
  "tab:rdd-2020-category", file.path(paths$tables, "appendix_rdd_2020_by_category.tex"))

# ═══════════════════════════════════════════════════════════════
# 4. BW SENSITIVITY COEFFICIENT PLOT
# ═══════════════════════════════════════════════════════════════

message("=== 4. BW sensitivity coefficient plot ===")

bw_grid <- seq(3, 25, by = 1)

bw_coef_2018 <- data.table::rbindlist(lapply(bw_grid, function(h) {
  tryCatch({
    fit <- rdrobust::rdrobust(art_2018$one_click_rate, art_2018$days_from_2018,
                              cluster = art_2018$account_id, h = h, b = h, p = 1, all = TRUE)
    data.table::data.table(bw = h, est = fit$coef[3], ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], cutoff = "2018")
  }, error = function(e) NULL)
}))

bw_coef_2020 <- data.table::rbindlist(lapply(bw_grid, function(h) {
  tryCatch({
    fit <- rdrobust::rdrobust(art_2020$one_click_rate, art_2020$days_from_2020,
                              cluster = art_2020$account_id, h = h, b = h, p = 1, all = TRUE)
    data.table::data.table(bw = h, est = fit$coef[3], ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], cutoff = "2020")
  }, error = function(e) NULL)
}))

bw_coef <- rbind(bw_coef_2018, bw_coef_2020)

g_bw <- ggplot2::ggplot(bw_coef, ggplot2::aes(x = bw, y = est)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lo, ymax = ci_hi), fill = "grey75", alpha = 0.4) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::facet_wrap(~cutoff, scales = "free_y", ncol = 1) +
  ggplot2::labs(x = "Bandwidth (days)", y = "RD Estimate (one-click rate)") +
  rd_theme +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))

ggplot2::ggsave(file.path(paths$figures, "appendix_bw_sensitivity.pdf"), g_bw,
                width = 6.5, height = 6, units = "in", device = "pdf")

# ═══════════════════════════════════════════════════════════════
# 5. HEAD ARTICLE VS NON-HEAD HETEROGENEITY
# ═══════════════════════════════════════════════════════════════

message("=== 5. Head article heterogeneity ===")

dt[, article_rank := seq_len(.N), by = .(account_id, publish_date)]
dt[, is_head := article_rank == 1L]

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

head_het <- data.table::rbindlist(lapply(c(TRUE, FALSE), function(hd) {
  lbl <- if (hd) "Head article" else "Non-head"
  sub18 <- art_2018[is_head == hd]
  sub20 <- art_2020[is_head == hd]
  data.table::rbindlist(list(
    safe_rdd(sub18$one_click_rate, sub18$days_from_2018, sub18$account_id, 1,
             paste0("One-Click 2018 [", lbl, "]")),
    safe_rdd(sub20$one_click_rate, sub20$days_from_2020, sub20$account_id, 1,
             paste0("One-Click 2020 [", lbl, "]")),
    safe_rdd(sub20$like_rate, sub20$days_from_2020, sub20$account_id, 1,
             paste0("Like 2020 [", lbl, "]")),
    safe_rdd(sub20$look_rate, sub20$days_from_2020, sub20$account_id, 1,
             paste0("Zaikan 2020 [", lbl, "]"))
  ))
}))

fmt_tex(head_het,
  "RDD heterogeneity by article position: head (first push) vs non-head articles. Account-clustered SEs.",
  "tab:rdd-head-article", file.path(paths$tables, "appendix_rdd_head_article.tex"))

# ═══════════════════════════════════════════════════════════════
# 6. HIGH-CONFIDENCE SUBSAMPLE ROBUSTNESS
# ═══════════════════════════════════════════════════════════════

message("=== 6. High-confidence subsample ===")

conf_cut <- stats::quantile(dt$confidence, 0.5, na.rm = TRUE)

hi_conf_2018 <- art_2018[confidence >= conf_cut]
hi_conf_2020 <- art_2020[confidence >= conf_cut]

conf_robust <- data.table::rbindlist(list(
  safe_rdd(hi_conf_2018$one_click_rate, hi_conf_2018$days_from_2018, hi_conf_2018$account_id, 1,
           "One-Click 2018 (high conf.)"),
  safe_rdd(hi_conf_2020$one_click_rate, hi_conf_2020$days_from_2020, hi_conf_2020$account_id, 1,
           "One-Click 2020 (high conf.)"),
  safe_rdd(hi_conf_2020$look_rate, hi_conf_2020$days_from_2020, hi_conf_2020$account_id, 1,
           "Zaikan 2020 (high conf.)")
))

fmt_tex(conf_robust,
  "RDD on high-confidence classified articles only (above median LLM confidence). Account-clustered SEs.",
  "tab:rdd-high-conf", file.path(paths$tables, "appendix_rdd_high_confidence.tex"))

# ═══════════════════════════════════════════════════════════════
# 7. EVENT-STUDY STYLE DAILY RAW MEANS
# ═══════════════════════════════════════════════════════════════

message("=== 7. Event-study raw daily means ===")

make_event_study <- function(art, rv, filename, y_label = "One-Click Engagement Rate") {
  daily <- art[, .(y = mean(one_click_rate, na.rm = TRUE), n = .N), by = .(x = get(rv))]
  data.table::setorder(daily, x)

  g <- ggplot2::ggplot(daily, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 1.5, color = "grey30", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "red") +
    ggplot2::geom_smooth(data = daily[x < 0], method = "loess", span = 0.5,
                         se = TRUE, color = "black", fill = "grey80", linewidth = 0.8) +
    ggplot2::geom_smooth(data = daily[x > 0], method = "loess", span = 0.5,
                         se = TRUE, color = "black", fill = "grey80", linewidth = 0.8) +
    rd_x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Days relative to cutoff", y = y_label) +
    rd_theme

  ggplot2::ggsave(file.path(paths$figures, filename), g,
                  width = 6.5, height = 4.5, units = "in", device = "pdf")
}

make_event_study(art_2018, "days_from_2018", "appendix_event_study_2018.pdf")
make_event_study(art_2020, "days_from_2020", "appendix_event_study_2020.pdf")

# ═══════════════════════════════════════════════════════════════
# 8. CITY-LEVEL HETEROGENEITY (population proxy: big vs small)
# ═══════════════════════════════════════════════════════════════

message("=== 8. City-level heterogeneity ===")

city_posts <- dt[, .N, by = city]
city_median <- stats::median(city_posts$N)
big_cities <- city_posts[N >= city_median]$city

dt[, big_city := city %in% big_cities]
art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

city_het <- data.table::rbindlist(lapply(c(TRUE, FALSE), function(bc) {
  lbl <- if (bc) "Large city" else "Small city"
  sub18 <- art_2018[big_city == bc]
  sub20 <- art_2020[big_city == bc]
  data.table::rbindlist(list(
    safe_rdd(sub18$one_click_rate, sub18$days_from_2018, sub18$account_id, 1,
             paste0("One-Click 2018 [", lbl, "]")),
    safe_rdd(sub20$one_click_rate, sub20$days_from_2020, sub20$account_id, 1,
             paste0("One-Click 2020 [", lbl, "]")),
    safe_rdd(sub20$like_rate, sub20$days_from_2020, sub20$account_id, 1,
             paste0("Like 2020 [", lbl, "]")),
    safe_rdd(sub20$look_rate, sub20$days_from_2020, sub20$account_id, 1,
             paste0("Zaikan 2020 [", lbl, "]"))
  ))
}))

fmt_tex(city_het,
  "RDD heterogeneity by city size (above/below median total posts as proxy for population and digital engagement). Account-clustered SEs.",
  "tab:rdd-city-size", file.path(paths$tables, "appendix_rdd_city_size.tex"))

message("All appendix analyses saved.")
