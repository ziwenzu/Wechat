bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

dt <- load_rdd_data(paths)

max_window <- 30L
art_2020 <- dt[abs(days_from_2020) <= max_window]

label_short <- c(
  "\u610f\u8bc6\u5f62\u6001\u4e0e\u5ba3\u4f20\u6559\u80b2" = "Ideological Education",
  "\u65f6\u653f\u4e0e\u9886\u5bfc\u6d3b\u52a8"             = "Leadership & Political Events",
  "\u516c\u5171\u670d\u52a1\u4fe1\u606f"                     = "Public Service Info",
  "\u793e\u4f1a\u4fdd\u969c\u4e0e\u516c\u5171\u798f\u5229"   = "Welfare & Social Protection",
  "\u5e94\u6025\u7ba1\u7406\u4e0e\u98ce\u9669\u6c9f\u901a"   = "Emergency & Risk",
  "\u653f\u7b56\u4e0e\u653f\u52a1\u516c\u5f00"               = "Policy & Disclosure",
  "\u793e\u4f1a\u6cbb\u7406\u4e0e\u6267\u6cd5\u901a\u62a5"   = "Governance & Enforcement",
  "\u7fa4\u4f17\u52a8\u5458\u4e0e\u793e\u4f1a\u53c2\u4e0e"   = "Mobilization & Participation",
  "\u7ecf\u6d4e\u4e0e\u53d1\u5c55\u5efa\u8bbe"               = "Economic Development",
  "\u57ce\u5e02\u5f62\u8c61\u4e0e\u6587\u5316\u6d3b\u52a8"   = "City Image & Culture"
)

# ── Category-level RDD (2020 cutoff) ─────────────────────────

categories <- sort(unique(art_2020$category))

cat_rows <- data.table::rbindlist(lapply(categories, function(cat) {
  sub <- art_2020[category == cat]
  if (nrow(sub) < 40) return(NULL)

  cat_en <- unname(label_short[cat])
  if (is.na(cat_en)) cat_en <- cat

  outcomes <- c("one_click_rate", "like_rate", "look_rate")
  outcome_labels <- c("One-click", "Like", "Zaikan")

  data.table::rbindlist(lapply(seq_along(outcomes), function(i) {
    r <- safe_rdd(
      y  = sub[[outcomes[i]]],
      x  = sub$days_from_2020,
      cl = sub$account_id,
      label = paste0(cat_en, ": ", outcome_labels[i])
    )
    if (!is.null(r)) r[, Category := cat_en]
    r
  }))
}))

if (nrow(cat_rows) > 0) {
  tex_dt <- cat_rows[, .(
    Category, Specification = label, Estimate = tau_rb, SE = se_rb,
    `p-value` = pvalue, BW = round((bw_l + bw_r) / 2),
    `N (L)` = n_l, `N (R)` = n_r
  )]
  write_tex_table(
    tex_dt,
    file.path(paths$tables, "appendix_rdd_2020_by_category.tex"),
    caption = "RDD estimates at the July 2020 cutoff by content category.",
    label = "tab:rdd-2020-category",
    digits = c(Estimate = 4L, SE = 4L, `p-value` = 3L, BW = 0L, `N (L)` = 0L, `N (R)` = 0L),
    align = paste0("ll", paste(rep("r", 6), collapse = ""))
  )
  message("Saved appendix_rdd_2020_by_category.tex")
}

# ── Head article vs non-head ─────────────────────────────────

art_2020[, article_rank := seq_len(.N), by = .(account_id, publish_date)]
art_2020[, is_head := article_rank == 1L]

head_rows <- data.table::rbindlist(lapply(c(TRUE, FALSE), function(hd) {
  sub <- art_2020[is_head == hd]
  tag <- if (hd) "Head article" else "Non-head article"
  outcomes <- c("one_click_rate", "like_rate", "look_rate")
  outcome_labels <- c("One-click", "Like", "Zaikan")

  data.table::rbindlist(lapply(seq_along(outcomes), function(i) {
    safe_rdd(
      y  = sub[[outcomes[i]]],
      x  = sub$days_from_2020,
      cl = sub$account_id,
      label = paste0(tag, ": ", outcome_labels[i])
    )
  }))
}))

if (nrow(head_rows) > 0) {
  fmt_rdd_tex(
    head_rows,
    caption  = "RDD estimates at the July 2020 cutoff: head vs.\\ non-head articles.",
    label    = "tab:rdd-head-article",
    filepath = file.path(paths$tables, "appendix_rdd_head_article.tex")
  )
  message("Saved appendix_rdd_head_article.tex")
}

# ── City size split (median total posts per city) ────────────

city_posts <- dt[, .(total_posts = .N), by = .(account_id)]
median_posts <- stats::median(city_posts$total_posts)
big_cities <- city_posts[total_posts >= median_posts, account_id]

city_rows <- data.table::rbindlist(lapply(c("Large city", "Small city"), function(grp) {
  sub <- if (grp == "Large city") {
    art_2020[account_id %in% big_cities]
  } else {
    art_2020[!account_id %in% big_cities]
  }
  outcomes <- c("one_click_rate", "like_rate", "look_rate")
  outcome_labels <- c("One-click", "Like", "Zaikan")

  data.table::rbindlist(lapply(seq_along(outcomes), function(i) {
    safe_rdd(
      y  = sub[[outcomes[i]]],
      x  = sub$days_from_2020,
      cl = sub$account_id,
      label = paste0(grp, ": ", outcome_labels[i])
    )
  }))
}))

if (nrow(city_rows) > 0) {
  fmt_rdd_tex(
    city_rows,
    caption  = "RDD estimates at the July 2020 cutoff by city size (median split on total posts).",
    label    = "tab:rdd-city-size",
    filepath = file.path(paths$tables, "appendix_rdd_city_size.tex")
  )
  message("Saved appendix_rdd_city_size.tex")
}

# ── High-confidence subsample ────────────────────────────────

if ("confidence" %in% names(art_2020)) {
  conf_median <- stats::median(art_2020$confidence, na.rm = TRUE)
  hi_conf <- art_2020[!is.na(confidence) & confidence >= conf_median]

  outcomes <- c("one_click_rate", "like_rate", "look_rate")
  outcome_labels <- c("One-click", "Like", "Zaikan")

  conf_rows <- data.table::rbindlist(lapply(seq_along(outcomes), function(i) {
    safe_rdd(
      y  = hi_conf[[outcomes[i]]],
      x  = hi_conf$days_from_2020,
      cl = hi_conf$account_id,
      label = paste0("High confidence: ", outcome_labels[i])
    )
  }))

  if (nrow(conf_rows) > 0) {
    fmt_rdd_tex(
      conf_rows,
      caption  = "RDD estimates at the July 2020 cutoff for above-median classification confidence.",
      label    = "tab:rdd-high-confidence",
      filepath = file.path(paths$tables, "appendix_rdd_high_confidence.tex")
    )
    message("Saved appendix_rdd_high_confidence.tex")
  }
} else {
  message("Column 'confidence' not found; skipping high-confidence subsample.")
}

message("Done: 18_rdd_heterogeneity_appendix.R")
