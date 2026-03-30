bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "ggplot2", "rdrobust", "fs"))

args <- parse_args()
paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)
fs::dir_create(paths$tables, recurse = TRUE)

input_name <- if (is.null(args$input)) "wechat_posts_clean.rds" else args$input
input_path <- file.path(paths$data, input_name)

message("Loading cleaned dataset: ", input_path)
dt <- readRDS(input_path)

daily <- dt[
  ,
  .(
    n_posts = .N,
    total_reads = sum(read_num, na.rm = TRUE),
    total_likes = sum(like_num, na.rm = TRUE),
    total_shares = sum(share_num, na.rm = TRUE),
    total_looks = sum(look_num, na.rm = TRUE),
    total_reactions = sum(total_reaction_num, na.rm = TRUE)
  ),
  by = .(publish_date, content_group, content_family)
]

day_totals <- daily[, .(all_posts = sum(n_posts)), by = publish_date]
daily <- merge(daily, day_totals, by = "publish_date", all.x = TRUE, sort = FALSE)
data.table::setorder(daily, content_group, publish_date)

daily[, reads_per_post := safe_rate(total_reads, n_posts)]
daily[, like_rate_weighted := safe_rate(total_likes, total_reads)]
daily[, look_rate_weighted := safe_rate(total_looks, total_reads)]
daily[, public_signal_rate_weighted := safe_rate(total_shares + total_looks, total_reads)]
daily[, total_reaction_rate_weighted := safe_rate(total_reactions, total_reads)]
daily[, share_posts_day := safe_rate(n_posts, all_posts)]

run_daily_rdd <- function(
  data,
  cutoff,
  group_name,
  outcome,
  window_days = 365L,
  donut_days = 0L,
  min_days = 60L
) {
  cut_date <- as.Date(cutoff)
  window_dt <- data[
    content_group == group_name,
    .(publish_date, n_posts, outcome = get(outcome))
  ]
  window_dt[, running := as.integer(publish_date - cut_date)]
  window_dt <- window_dt[
    abs(running) <= window_days &
      abs(running) > donut_days &
      !is.na(outcome)
  ]

  empty_summary <- data.table::data.table(
    cutoff = as.character(cut_date),
    group = group_name,
    outcome = outcome,
    window_days = window_days,
    donut_days = donut_days,
    n_days = nrow(window_dt),
    estimate_usual = NA_real_,
    estimate_bias_corrected = NA_real_,
    estimate_robust = NA_real_,
    se_usual = NA_real_,
    se_bias_corrected = NA_real_,
    se_robust = NA_real_,
    p_usual = NA_real_,
    p_bias_corrected = NA_real_,
    p_robust = NA_real_,
    ci_usual_lower = NA_real_,
    ci_usual_upper = NA_real_,
    ci_robust_lower = NA_real_,
    ci_robust_upper = NA_real_,
    status = "skipped_insufficient_days"
  )

  if (nrow(window_dt) < min_days) {
    return(list(data = window_dt, fit = NULL, summary = empty_summary))
  }

  fit <- tryCatch(
    rdrobust::rdrobust(
      y = window_dt$outcome,
      x = window_dt$running,
      c = 0,
      p = 1,
      weights = window_dt$n_posts
    ),
    error = function(err) err
  )

  if (inherits(fit, "error")) {
    empty_summary[, status := paste0("rdrobust_error: ", conditionMessage(fit))]
    return(list(data = window_dt, fit = NULL, summary = empty_summary))
  }

  summary_row <- data.table::data.table(
    cutoff = as.character(cut_date),
    group = group_name,
    outcome = outcome,
    window_days = window_days,
    donut_days = donut_days,
    n_days = nrow(window_dt),
    estimate_usual = fit$coef[1, 1],
    estimate_bias_corrected = fit$coef[2, 1],
    estimate_robust = fit$coef[3, 1],
    se_usual = fit$se[1, 1],
    se_bias_corrected = fit$se[2, 1],
    se_robust = fit$se[3, 1],
    p_usual = fit$pv[1, 1],
    p_bias_corrected = fit$pv[2, 1],
    p_robust = fit$pv[3, 1],
    ci_usual_lower = fit$ci[1, 1],
    ci_usual_upper = fit$ci[1, 2],
    ci_robust_lower = fit$ci[3, 1],
    ci_robust_upper = fit$ci[3, 2],
    status = "ok"
  )

  list(data = window_dt, fit = fit, summary = summary_row)
}

make_binned_plot <- function(window_dt, cutoff_label, group_name, outcome, file_name) {
  plot_dt <- data.table::copy(window_dt)
  plot_dt[, bin := floor(running / 14L) * 14L]

  binned <- plot_dt[
    ,
    .(
      outcome = weighted_mean_or_na(outcome, n_posts),
      n_posts = sum(n_posts)
    ),
    by = bin
  ][order(bin)]

  p <- ggplot2::ggplot(binned, ggplot2::aes(x = bin, y = outcome)) +
    ggplot2::geom_point(ggplot2::aes(size = n_posts), alpha = 0.8, color = "#1f4e79") +
    ggplot2::geom_smooth(
      data = binned[binned$bin < 0, ],
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "#2e7d32",
      linewidth = 0.9
    ) +
    ggplot2::geom_smooth(
      data = binned[binned$bin >= 0, ],
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "#c62828",
      linewidth = 0.9
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#444444") +
    ggplot2::labs(
      title = paste0("RDD window around ", cutoff_label),
      subtitle = paste(group_name, "-", outcome),
      x = "Days from cutoff",
      y = outcome,
      size = "Posts in bin"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  ggplot2::ggsave(
    filename = file.path(paths$figures, file_name),
    plot = p,
    width = 9,
    height = 5.5,
    dpi = 300
  )
}

main_specs <- data.table::CJ(
  cutoff = c("2018-12-21", "2020-07-01"),
  group = c("propaganda_hard", "propaganda_soft", "service"),
  outcome = c(
    "reads_per_post",
    "like_rate_weighted",
    "look_rate_weighted",
    "public_signal_rate_weighted",
    "total_reaction_rate_weighted"
  )
)[
  ,
  `:=`(
    window_days = 365L,
    donut_days = 0L,
    spec_type = "main"
  )
]

robustness_base <- main_specs[
  group %in% c("propaganda_hard", "service") &
    outcome %in% c("reads_per_post", "like_rate_weighted", "public_signal_rate_weighted")
]

robustness_180 <- data.table::copy(robustness_base)[
  ,
  `:=`(
    window_days = 180L,
    spec_type = "bandwidth_180"
  )
]

robustness_donut <- data.table::copy(robustness_base)[
  ,
  `:=`(
    donut_days = 7L,
    spec_type = "donut_7"
  )
]

continuity_specs <- data.table::CJ(
  cutoff = c("2018-12-21", "2020-07-01"),
  group = c("propaganda_hard", "propaganda_soft", "service"),
  outcome = c("n_posts", "share_posts_day")
)[
  ,
  `:=`(
    window_days = 365L,
    donut_days = 0L,
    spec_type = "continuity"
  )
]

specs <- data.table::rbindlist(
  list(main_specs, robustness_180, robustness_donut, continuity_specs),
  use.names = TRUE,
  fill = TRUE
)

results <- vector("list", nrow(specs))

for (i in seq_len(nrow(specs))) {
  spec <- specs[i]
  result <- run_daily_rdd(
    data = daily,
    cutoff = spec$cutoff[[1]],
    group_name = spec$group[[1]],
    outcome = spec$outcome[[1]],
    window_days = spec$window_days[[1]],
    donut_days = spec$donut_days[[1]]
  )

  results[[i]] <- cbind(spec[, .(spec_type)], result$summary)

  if (is.null(result$fit) || nrow(result$data) == 0) {
    message(
      "Skipping plot/output for ",
      spec$group[[1]],
      " / ",
      spec$outcome[[1]],
      " at ",
      spec$cutoff[[1]],
      " because the current input does not have enough days."
    )
    next
  }

  should_plot <- spec$window_days[[1]] == 365L &&
    spec$donut_days[[1]] == 0L &&
    spec$group[[1]] %in% c("propaganda_hard", "service") &&
    spec$outcome[[1]] %in% c("reads_per_post", "like_rate_weighted", "public_signal_rate_weighted")

  if (should_plot) {
    plot_name <- sprintf(
      "rdd_%s_%s_%s.pdf",
      gsub("-", "", spec$cutoff[[1]]),
      spec$group[[1]],
      spec$outcome[[1]]
    )

    make_binned_plot(
      window_dt = result$data,
      cutoff_label = spec$cutoff[[1]],
      group_name = spec$group[[1]],
      outcome = spec$outcome[[1]],
      file_name = plot_name
    )
  }

  tex_name <- sprintf(
    "rdd_%s_%s_%s_w%s_d%s.tex",
    gsub("-", "", spec$cutoff[[1]]),
    spec$group[[1]],
    spec$outcome[[1]],
    spec$window_days[[1]],
    spec$donut_days[[1]]
  )

  write_tex_table(
    result$summary,
    file.path(paths$tables, tex_name),
    caption = paste(
      "RDD summary for",
      spec$group[[1]],
      spec$outcome[[1]],
      "around",
      spec$cutoff[[1]]
    ),
    label = sprintf(
      "tab:rdd-%s-%s-%s-w%s-d%s",
      gsub("-", "", spec$cutoff[[1]]),
      spec$group[[1]],
      spec$outcome[[1]],
      spec$window_days[[1]],
      spec$donut_days[[1]]
    ),
    digits = c(
      window_days = 0,
      donut_days = 0,
      n_days = 0,
      estimate_usual = 4,
      estimate_bias_corrected = 4,
      estimate_robust = 4,
      se_usual = 4,
      se_bias_corrected = 4,
      se_robust = 4,
      p_usual = 4,
      p_bias_corrected = 4,
      p_robust = 4,
      ci_usual_lower = 4,
      ci_usual_upper = 4,
      ci_robust_lower = 4,
      ci_robust_upper = 4
    )
  )
}

summary_table <- data.table::rbindlist(results)
write_tex_table(
  summary_table,
  file.path(paths$tables, "rdd_summary.tex"),
  caption = "Summary of RDD specifications across cutoffs, groups, and outcomes.",
  label = "tab:rdd-summary",
  digits = c(
    window_days = 0,
    donut_days = 0,
    n_days = 0,
    estimate_usual = 4,
    estimate_bias_corrected = 4,
    estimate_robust = 4,
    se_usual = 4,
    se_bias_corrected = 4,
    se_robust = 4,
    p_usual = 4,
    p_bias_corrected = 4,
    p_robust = 4,
    ci_usual_lower = 4,
    ci_usual_upper = 4,
    ci_robust_lower = 4,
    ci_robust_upper = 4
  )
)

message("Finished baseline RDD outputs.")
