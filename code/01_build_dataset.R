bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

options(datatable.print.nrows = 50)

args <- parse_args()
paths <- project_paths()
fs::dir_create(paths$data, recurse = TRUE)
fs::dir_create(paths$tables, recurse = TRUE)

selected_cols <- c(
  "id",
  "province",
  "city",
  "public_account_name",
  "title",
  "YEAR",
  "DATE",
  "category",
  "confidence",
  "read_num",
  "like_num",
  "share_num",
  "look_num",
  "comment_count",
  "collect_num"
)

fread_args <- list(
  input = paths$articles_csv,
  select = selected_cols,
  encoding = "UTF-8",
  showProgress = TRUE
)

if (!is.null(args$nrows)) {
  fread_args$nrows <- as.integer(args$nrows)
}

message("Reading source data from: ", paths$articles_csv)
dt <- do.call(data.table::fread, fread_args)

data.table::setnames(dt, old = c("YEAR", "DATE"), new = c("year", "date_mmdd"))

numeric_cols <- c(
  "year",
  "confidence",
  "read_num",
  "like_num",
  "share_num",
  "look_num",
  "comment_count",
  "collect_num"
)

for (col in numeric_cols) {
  dt[, (col) := as.numeric(get(col))]
}

dt[, publish_date := as.Date(sprintf("%04d-%s", year, date_mmdd))]
dt <- dt[!is.na(publish_date)]
dt[, account_id := .GRP, by = public_account_name]

metric_cols <- c(
  "read_num",
  "like_num",
  "share_num",
  "look_num",
  "comment_count",
  "collect_num"
)

for (col in metric_cols) {
  dt[, (col) := coalesce_zero(get(col))]
}

dt[, content_group := classify_content_group(category)]
dt[, content_family := classify_content_family(category)]

dt[, public_signal_num := share_num + look_num]
dt[, total_reaction_num := like_num + share_num + look_num + comment_count + collect_num]
dt[, any_engagement := total_reaction_num > 0 | read_num > 0]
dt[, all_metrics_zero := rowSums(.SD) == 0, .SDcols = metric_cols]

dt[, like_rate := safe_rate(like_num, read_num)]
dt[, share_rate := safe_rate(share_num, read_num)]
dt[, look_rate := safe_rate(look_num, read_num)]
dt[, public_signal_rate := safe_rate(public_signal_num, read_num)]
dt[, total_reaction_rate := safe_rate(total_reaction_num, read_num)]

cut_2017 <- as.Date("2017-05-18")
cut_2020 <- as.Date("2020-07-01")

dt[, post_2017 := publish_date >= cut_2017]
dt[, post_2020 := publish_date >= cut_2020]
dt[, days_from_2017 := as.integer(publish_date - cut_2017)]
dt[, days_from_2020 := as.integer(publish_date - cut_2020)]

if (!is.null(args$sample_n)) {
  set.seed(20260326L)
  sample_n <- min(as.integer(args$sample_n), nrow(dt))
  dt <- dt[sample(.N, sample_n)]
  message("Kept a random sample of ", sample_n, " rows for this run.")
}

output_name <- if (is.null(args$output)) "wechat_posts_clean.rds" else args$output
output_path <- file.path(paths$data, output_name)

message("Writing cleaned dataset to: ", output_path)
saveRDS(dt, output_path)

zero_summary <- data.table::rbindlist(lapply(metric_cols, function(col) {
  data.table::data.table(
    metric = col,
    zero_share = mean(dt[[col]] == 0, na.rm = TRUE),
    mean_value = mean(dt[[col]], na.rm = TRUE),
    median_value = stats::median(dt[[col]], na.rm = TRUE)
  )
}))

coverage_summary <- data.table::data.table(
  n_rows = nrow(dt),
  n_accounts = data.table::uniqueN(dt$public_account_name),
  min_date = min(dt$publish_date, na.rm = TRUE),
  max_date = max(dt$publish_date, na.rm = TRUE)
)

family_counts <- dt[, .(n_posts = .N), by = .(content_family, content_group)][
  order(content_family, -n_posts)
]

data.table::fwrite(
  zero_summary,
  file.path(paths$tables, "metric_zero_summary.csv")
)
data.table::fwrite(
  coverage_summary,
  file.path(paths$tables, "dataset_coverage_summary.csv")
)
data.table::fwrite(
  family_counts,
  file.path(paths$tables, "content_group_counts.csv")
)

message("Finished building the R-ready dataset.")
