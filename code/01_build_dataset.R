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
  "reason",
  "keywords",
  "confidence",
  "read_num",
  "like_num",
  "share_num",
  "look_num",
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
  "collect_num"
)

for (col in numeric_cols) {
  dt[, (col) := as.numeric(get(col))]
}

dt[, publish_date := as.Date(sprintf("%04d-%s", year, date_mmdd))]
dt <- dt[!is.na(publish_date)]
analysis_start <- as.Date("2015-01-01")
analysis_end <- as.Date("2024-12-31")
dt <- dt[publish_date >= analysis_start & publish_date <= analysis_end]
dt[, account_id := .GRP, by = public_account_name]
dt[, category_raw := category]
dt[, category_raw_clean := trimws(as.character(category_raw))]

raw_lookup <- data.table::data.table(
  category_raw_clean = sort(unique(dt$category_raw_clean))
)
raw_lookup[, category_canonical := normalize_category(category = category_raw_clean)]
lookup_vec <- stats::setNames(
  raw_lookup$category_canonical,
  raw_lookup$category_raw_clean
)
dt[, category := unname(lookup_vec[category_raw_clean])]

generic_raw_labels <- c(
  "其他",
  "其它",
  "未知",
  "无",
  "未分类",
  "不适用",
  "无具体分类",
  "无匹配分类",
  "无效内容",
  "暂无分类",
  "暂无合适分类",
  "非明确分类",
  "低",
  "草稿",
  "自创分类",
  "自主创造",
  "（专业分类名称）",
  "内容归纳与分类",
  "综合信息",
  "热点信息",
  "热点新闻",
  "即时新闻",
  "新闻与信息",
  "新闻动态",
  "工作汇报"
)

needs_row_level <- dt$category_raw_clean %in% generic_raw_labels |
  nchar(dt$category_raw_clean) > 40

dt[needs_row_level, category := normalize_category(
  category = category_raw_clean,
  reason = reason,
  keywords = keywords,
  title = title
)]

metric_cols <- c(
  "read_num",
  "like_num",
  "share_num",
  "look_num",
  "collect_num"
)

for (col in metric_cols) {
  dt[, (col) := coalesce_zero(get(col))]
}

dt[, content_group := classify_content_group(category)]
dt[, content_family := classify_content_family(category)]

dt[, public_signal_num := share_num + look_num]
dt[, total_reaction_num := like_num + share_num + look_num + collect_num]
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

dt[, c("category_raw", "category_raw_clean") := NULL]

message("Writing cleaned dataset to: ", output_path)
saveRDS(dt, output_path)

message("Finished building the R-ready dataset.")
