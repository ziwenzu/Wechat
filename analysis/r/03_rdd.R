bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "ggplot2", "rdrobust", "fs"))

args <- parse_args()
paths <- project_paths()
fs::dir_create(paths$output, recurse = TRUE)

input_name <- if (is.null(args$input)) "wechat_posts_clean.rds" else args$input
input_path <- file.path(paths$output, input_name)

message("Loading cleaned dataset: ", input_path)
dt <- readRDS(input_path)

daily <- dt[
  ,
  .(
    n_posts = .N,
    total_reads = sum(read_num, na.rm = TRUE),
    total_likes = sum(like_num, na.rm = TRUE),
    total_shares = sum(share_num, na.rm = TRUE),
    total_looks = sum(look_num, na.rm = TRUE)
  ),
  by = .(publish_date, content_group, content_family)
][order(content_group, publish_date)]

daily[, like_rate_weighted := safe_rate(total_likes, total_reads)]
daily[, public_signal_rate_weighted := safe_rate(total_shares + total_looks, total_reads)]

run_daily_rdd <- function(
  data,
  cutoff,
  group_name,
  outcome,
  window_days = 365L,
  min_days = 60L
) {
  cut_date <- as.Date(cutoff)
  window_dt <- data[
    content_group == group_name,
    .(publish_date, n_posts, outcome = get(outcome))
  ]
  window_dt[, running := as.integer(publish_date - cut_date)]
  window_dt <- window_dt[abs(running) <= window_days & !is.na(outcome)]

  if (nrow(window_dt) < min_days) {
    return(list(
      data = window_dt,
      fit = NULL,
      summary = data.table::data.table(
        cutoff = as.character(cut_date),
        group = group_name,
        outcome = outcome,
        window_days = window_days,
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
    ))
  }

  fit <- rdrobust::rdrobust(
    y = window_dt$outcome,
    x = window_dt$running,
    c = 0,
    p = 1,
    weights = window_dt$n_posts
  )

  summary_row <- data.table::data.table(
    cutoff = as.character(cut_date),
    group = group_name,
    outcome = outcome,
    window_days = window_days,
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
    filename = file.path(paths$output, file_name),
    plot = p,
    width = 9,
    height = 5.5,
    dpi = 300
  )
}

specs <- list(
  list(cutoff = "2017-05-18", group = "propaganda_hard", outcome = "like_rate_weighted"),
  list(cutoff = "2017-05-18", group = "service", outcome = "like_rate_weighted"),
  list(cutoff = "2020-07-01", group = "propaganda_hard", outcome = "like_rate_weighted"),
  list(cutoff = "2020-07-01", group = "propaganda_hard", outcome = "public_signal_rate_weighted")
)

results <- vector("list", length(specs))

for (i in seq_along(specs)) {
  spec <- specs[[i]]
  result <- run_daily_rdd(
    data = daily,
    cutoff = spec$cutoff,
    group_name = spec$group,
    outcome = spec$outcome
  )

  results[[i]] <- result$summary

  if (is.null(result$fit) || nrow(result$data) == 0) {
    message(
      "Skipping plot/output for ",
      spec$group,
      " / ",
      spec$outcome,
      " at ",
      spec$cutoff,
      " because the current input does not have enough days."
    )
    next
  }

  plot_name <- sprintf(
    "rdd_%s_%s_%s.png",
    gsub("-", "", spec$cutoff),
    spec$group,
    spec$outcome
  )

  make_binned_plot(
    window_dt = result$data,
    cutoff_label = spec$cutoff,
    group_name = spec$group,
    outcome = spec$outcome,
    file_name = plot_name
  )

  txt_name <- sprintf(
    "rdd_%s_%s_%s.txt",
    gsub("-", "", spec$cutoff),
    spec$group,
    spec$outcome
  )

  capture.output(
    print(result$fit),
    file = file.path(paths$output, txt_name)
  )
}

summary_table <- data.table::rbindlist(results)
data.table::fwrite(summary_table, file.path(paths$output, "rdd_summary.csv"))

message("Finished baseline RDD outputs.")
