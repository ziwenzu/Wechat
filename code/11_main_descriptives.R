bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

dt[, one_click_rate := like_rate + look_rate]

label_short <- c(
  "\u610f\u8bc6\u5f62\u6001\u4e0e\u5ba3\u4f20\u6559\u80b2" = "Ideological Education",
  "\u65f6\u653f\u4e0e\u9886\u5bfc\u6d3b\u52a8" = "Leadership & Political Events",
  "\u516c\u5171\u670d\u52a1\u4fe1\u606f" = "Public Service Info",
  "\u793e\u4f1a\u4fdd\u969c\u4e0e\u516c\u5171\u798f\u5229" = "Welfare & Social Protection",
  "\u5e94\u6025\u7ba1\u7406\u4e0e\u98ce\u9669\u6c9f\u901a" = "Emergency & Risk",
  "\u653f\u7b56\u4e0e\u653f\u52a1\u516c\u5f00" = "Policy & Disclosure",
  "\u793e\u4f1a\u6cbb\u7406\u4e0e\u6267\u6cd5\u901a\u62a5" = "Governance & Enforcement",
  "\u7fa4\u4f17\u52a8\u5458\u4e0e\u793e\u4f1a\u53c2\u4e0e" = "Mobilization & Participation",
  "\u7ecf\u6d4e\u4e0e\u53d1\u5c55\u5efa\u8bbe" = "Economic Development",
  "\u57ce\u5e02\u5f62\u8c61\u4e0e\u6587\u5316\u6d3b\u52a8" = "City Image & Culture"
)

family_gloss <- c(
  "public_service" = "Public Service",
  "soft_propaganda" = "Soft Propaganda",
  "state_governance" = "State Governance",
  "hard_propaganda" = "Hard Propaganda"
)

family_order <- c("public_service", "soft_propaganda", "state_governance", "hard_propaganda")
category_order <- c(
  "\u516c\u5171\u670d\u52a1\u4fe1\u606f",
  "\u5e94\u6025\u7ba1\u7406\u4e0e\u98ce\u9669\u6c9f\u901a",
  "\u793e\u4f1a\u4fdd\u969c\u4e0e\u516c\u5171\u798f\u5229",
  "\u57ce\u5e02\u5f62\u8c61\u4e0e\u6587\u5316\u6d3b\u52a8",
  "\u7ecf\u6d4e\u4e0e\u53d1\u5c55\u5efa\u8bbe",
  "\u793e\u4f1a\u6cbb\u7406\u4e0e\u6267\u6cd5\u901a\u62a5",
  "\u7fa4\u4f17\u52a8\u5458\u4e0e\u793e\u4f1a\u53c2\u4e0e",
  "\u653f\u7b56\u4e0e\u653f\u52a1\u516c\u5f00",
  "\u65f6\u653f\u4e0e\u9886\u5bfc\u6d3b\u52a8",
  "\u610f\u8bc6\u5f62\u6001\u4e0e\u5ba3\u4f20\u6559\u80b2"
)

fmt_int <- function(x) format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)
fmt_mean_sd <- function(mean_x, sd_x) paste0(fmt_int(mean_x), " (", fmt_int(sd_x), ")")

# ── Overall summary (no Collects) ─────────────────────────────

metric_map <- list(
  "Reads"  = "read_num",
  "Likes"  = "like_num",
  "Shares" = "share_num",
  "Zaikan" = "look_num"
)

overall_rows <- data.table::rbindlist(lapply(names(metric_map), function(m) {
  v <- dt[[metric_map[[m]]]]
  data.table::data.table(
    Variable = m,
    Min = min(v, na.rm = TRUE), Max = max(v, na.rm = TRUE),
    Mean = mean(v, na.rm = TRUE), Median = stats::median(v, na.rm = TRUE),
    SD = stats::sd(v, na.rm = TRUE)
  )
}))

write_tex_table(
  overall_rows,
  file.path(paths$tables, "main_descriptive_overall_stats.tex"),
  caption = "Overall descriptive statistics for article-level engagement variables.",
  label = "tab:main-descriptive-overall-stats",
  digits = c(Min = 0L, Max = 0L, Mean = 0L, Median = 0L, SD = 0L)
)

# ── Family-level (no Collects, includes H1 reads ranking) ─────

family_table <- dt[
  ,
  .(
    `Posts (M)` = .N / 1e6,
    `Mean Reads` = mean(read_num),
    `Median Reads` = as.double(stats::median(read_num)),
    `Likes M (SD)` = fmt_mean_sd(mean(like_num), stats::sd(like_num)),
    `Shares M (SD)` = fmt_mean_sd(mean(share_num), stats::sd(share_num)),
    `Zaikan M (SD)` = fmt_mean_sd(mean(look_num), stats::sd(look_num))
  ),
  by = content_family
]
family_table[, Group := unname(family_gloss[content_family])]
family_table[, Group := factor(Group, levels = unname(family_gloss[family_order]))]
data.table::setorder(family_table, Group)
family_table[, Group := as.character(Group)]
family_table <- family_table[, .(Group, `Posts (M)`, `Mean Reads`, `Median Reads`,
                                 `Likes M (SD)`, `Shares M (SD)`, `Zaikan M (SD)`)]

write_tex_table(
  family_table,
  file.path(paths$tables, "main_descriptive_by_family.tex"),
  caption = "Family-level descriptive statistics. Mean and median reads support H1: public service content attracts more routine consumption than propaganda.",
  label = "tab:main-descriptive-by-family",
  digits = c(`Posts (M)` = 2L, `Mean Reads` = 0L, `Median Reads` = 0L),
  align = "lrrrrrr"
)

# ── Category-level (no Collects) ──────────────────────────────

category_table <- dt[
  ,
  .(
    `Posts (M)` = .N / 1e6,
    `Reads M (SD)` = fmt_mean_sd(mean(read_num), stats::sd(read_num)),
    `Likes M (SD)` = fmt_mean_sd(mean(like_num), stats::sd(like_num)),
    `Shares M (SD)` = fmt_mean_sd(mean(share_num), stats::sd(share_num)),
    `Zaikan M (SD)` = fmt_mean_sd(mean(look_num), stats::sd(look_num))
  ),
  by = .(category, content_family)
]
category_table[, Family := unname(family_gloss[content_family])]
category_table[, Category := unname(label_short[category])]
category_table[, category := factor(category, levels = category_order)]
data.table::setorder(category_table, category)
category_table <- category_table[, .(Category, Family, `Posts (M)`,
                                     `Reads M (SD)`, `Likes M (SD)`,
                                     `Shares M (SD)`, `Zaikan M (SD)`)]

write_tex_table(
  category_table,
  file.path(paths$tables, "main_descriptive_by_category.tex"),
  caption = "Category-level descriptive statistics.",
  label = "tab:main-descriptive-by-category",
  digits = c(`Posts (M)` = 2L),
  align = "llrrrrr"
)

# ── Account-level summary ────────────────────────────────────

account_summary <- dt[, .(
  posts       = .N,
  mean_reads  = mean(read_num, na.rm = TRUE),
  mean_like   = mean(like_rate, na.rm = TRUE),
  mean_look   = mean(look_rate, na.rm = TRUE),
  mean_share  = mean(share_rate, na.rm = TRUE)
), by = account_id]

account_stats <- data.table::data.table(
  Statistic = c(
    "Accounts", "Posts per account: mean", "Posts per account: median",
    "Posts per account: SD", "Posts per account: min", "Posts per account: max",
    "Mean reads per account: mean", "Mean reads per account: SD"
  ),
  Value = c(
    nrow(account_summary),
    mean(account_summary$posts),
    stats::median(account_summary$posts),
    stats::sd(account_summary$posts),
    min(account_summary$posts),
    max(account_summary$posts),
    mean(account_summary$mean_reads),
    stats::sd(account_summary$mean_reads)
  )
)

write_tex_table(
  account_stats,
  file.path(paths$tables, "main_descriptive_account_level.tex"),
  caption = "Account-level summary statistics for the 329 prefecture-level government WeChat accounts.",
  label = "tab:main-descriptive-account",
  digits = c(Value = 0L)
)

message("Descriptive tables saved.")
