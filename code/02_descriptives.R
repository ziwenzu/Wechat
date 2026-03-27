bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "ggplot2", "fs"))

args <- parse_args()
paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)
fs::dir_create(paths$tables, recurse = TRUE)

input_name <- if (is.null(args$input)) "wechat_posts_clean.rds" else args$input
input_path <- file.path(paths$data, input_name)

message("Loading cleaned dataset: ", input_path)
dt <- readRDS(input_path)

metric_cols <- c(
  "read_num",
  "like_num",
  "share_num",
  "look_num",
  "comment_count",
  "collect_num"
)

family_summary <- dt[
  ,
  .(
    n_posts = .N,
    n_accounts = data.table::uniqueN(public_account_name),
    mean_reads = mean(read_num, na.rm = TRUE),
    median_reads = stats::median(read_num, na.rm = TRUE),
    mean_likes = mean(like_num, na.rm = TRUE),
    mean_shares = mean(share_num, na.rm = TRUE),
    mean_looks = mean(look_num, na.rm = TRUE),
    mean_comments = mean(comment_count, na.rm = TRUE),
    mean_collects = mean(collect_num, na.rm = TRUE),
    mean_like_rate = mean(like_rate, na.rm = TRUE),
    mean_public_signal_rate = mean(public_signal_rate, na.rm = TRUE),
    share_all_zero = mean(all_metrics_zero, na.rm = TRUE)
  ),
  by = content_family
][order(-n_posts)]

group_summary <- dt[
  ,
  .(
    n_posts = .N,
    n_accounts = data.table::uniqueN(public_account_name),
    mean_reads = mean(read_num, na.rm = TRUE),
    median_reads = stats::median(read_num, na.rm = TRUE),
    mean_like_rate = mean(like_rate, na.rm = TRUE),
    mean_public_signal_rate = mean(public_signal_rate, na.rm = TRUE),
    share_all_zero = mean(all_metrics_zero, na.rm = TRUE)
  ),
  by = .(content_family, content_group)
][order(content_family, -n_posts)]

year_family <- dt[
  ,
  .(
    n_posts = .N,
    mean_reads = mean(read_num, na.rm = TRUE),
    mean_likes = mean(like_num, na.rm = TRUE),
    mean_like_rate = mean(like_rate, na.rm = TRUE),
    mean_public_signal_rate = mean(public_signal_rate, na.rm = TRUE)
  ),
  by = .(year, content_family)
][order(year, content_family)]

zero_profile <- data.table::rbindlist(lapply(metric_cols, function(col) {
  dt[
    ,
    .(
      metric = col,
      zero_share = mean(get(col) == 0, na.rm = TRUE)
    ),
    by = content_family
  ]
}))

data.table::fwrite(
  family_summary,
  file.path(paths$tables, "descriptive_content_family.csv")
)
data.table::fwrite(
  group_summary,
  file.path(paths$tables, "descriptive_content_group.csv")
)
data.table::fwrite(
  year_family,
  file.path(paths$tables, "descriptive_year_family.csv")
)
data.table::fwrite(
  zero_profile,
  file.path(paths$tables, "descriptive_zero_profile.csv")
)

plot_posts <- ggplot2::ggplot(
  year_family,
  ggplot2::aes(x = year, y = n_posts, color = content_family)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::labs(
    title = "Annual post volume by content family",
    x = NULL,
    y = "Posts",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12)

plot_reads <- ggplot2::ggplot(
  year_family,
  ggplot2::aes(x = year, y = mean_reads, color = content_family)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::labs(
    title = "Annual mean reads by content family",
    x = NULL,
    y = "Mean reads",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(paths$figures, "yearly_posts_by_family.pdf"),
  plot = plot_posts,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(paths$figures, "yearly_mean_reads_by_family.pdf"),
  plot = plot_reads,
  width = 9,
  height = 5.5,
  dpi = 300
)

message("Finished descriptive summaries and plots.")
