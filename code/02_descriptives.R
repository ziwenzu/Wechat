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

main_families <- c("hard_propaganda", "soft_propaganda", "public_service")

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

main_family_summary <- family_summary[content_family %in% main_families]
year_main_family <- year_family[content_family %in% main_families]

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

table_digits <- c(
  n_posts = 0,
  n_accounts = 0,
  mean_reads = 3,
  median_reads = 3,
  mean_likes = 3,
  mean_shares = 3,
  mean_looks = 3,
  mean_comments = 3,
  mean_collects = 3,
  mean_like_rate = 4,
  mean_public_signal_rate = 4,
  share_all_zero = 4,
  zero_share = 4,
  year = 0
)

write_tex_table(
  family_summary,
  file.path(paths$tables, "descriptive_content_family.tex"),
  caption = "Descriptive summary by content family.",
  label = "tab:descriptive-content-family",
  digits = table_digits
)
write_tex_table(
  main_family_summary,
  file.path(paths$tables, "descriptive_main_content_family.tex"),
  caption = "Descriptive summary for the three main content families.",
  label = "tab:descriptive-main-content-family",
  digits = table_digits
)
write_tex_table(
  group_summary,
  file.path(paths$tables, "descriptive_content_group.tex"),
  caption = "Descriptive summary by content family and content group.",
  label = "tab:descriptive-content-group",
  digits = table_digits
)
write_tex_table(
  year_family,
  file.path(paths$tables, "descriptive_year_family.tex"),
  caption = "Yearly descriptive summary by content family.",
  label = "tab:descriptive-year-family",
  digits = table_digits
)
write_tex_table(
  year_main_family,
  file.path(paths$tables, "descriptive_year_main_family.tex"),
  caption = "Yearly descriptive summary for the three main content families.",
  label = "tab:descriptive-year-main-family",
  digits = table_digits
)
write_tex_table(
  zero_profile,
  file.path(paths$tables, "descriptive_zero_profile.tex"),
  caption = "Zero-value profile by content family and engagement metric.",
  label = "tab:descriptive-zero-profile",
  digits = c(zero_share = 4)
)

plot_posts <- ggplot2::ggplot(
  year_main_family,
  ggplot2::aes(x = year, y = n_posts, color = content_family)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::labs(
    title = "Annual post volume by main content family",
    x = NULL,
    y = "Posts",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12)

plot_reads <- ggplot2::ggplot(
  year_main_family,
  ggplot2::aes(x = year, y = mean_reads, color = content_family)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::labs(
    title = "Annual mean reads by main content family",
    x = NULL,
    y = "Mean reads",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12)

plot_like_rate <- ggplot2::ggplot(
  year_main_family,
  ggplot2::aes(x = year, y = mean_like_rate, color = content_family)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::labs(
    title = "Annual like rate by main content family",
    x = NULL,
    y = "Mean like rate",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12)

plot_public_signal <- ggplot2::ggplot(
  year_main_family,
  ggplot2::aes(x = year, y = mean_public_signal_rate, color = content_family)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::labs(
    title = "Annual public-signal rate by main content family",
    x = NULL,
    y = "Mean public-signal rate",
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

ggplot2::ggsave(
  filename = file.path(paths$figures, "yearly_like_rate_by_family.pdf"),
  plot = plot_like_rate,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(paths$figures, "yearly_public_signal_by_family.pdf"),
  plot = plot_public_signal,
  width = 9,
  height = 5.5,
  dpi = 300
)

message("Finished descriptive summaries and plots.")
