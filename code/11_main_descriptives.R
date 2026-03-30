bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
memo_dir <- file.path(paths$root, "memo")
fs::dir_create(memo_dir, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

label_short <- c(
  "意识形态与宣传教育" = "Ideological Education",
  "时政与领导活动" = "Leadership & Political Events",
  "公共服务信息" = "Public Service Info",
  "社会保障与公共福利" = "Welfare & Social Protection",
  "应急管理与风险沟通" = "Emergency & Risk",
  "政策与政务公开" = "Policy & Disclosure",
  "社会治理与执法通报" = "Governance & Enforcement",
  "群众动员与社会参与" = "Mobilization & Participation",
  "经济与发展建设" = "Economic Development",
  "城市形象与文化活动" = "City Image & Culture"
)

family_gloss <- c(
  "public_service" = "Public Service",
  "soft_propaganda" = "Soft Propaganda",
  "state_governance" = "State Governance",
  "hard_propaganda" = "Hard Propaganda"
)

family_order <- c("public_service", "soft_propaganda", "state_governance", "hard_propaganda")
category_order <- c(
  "公共服务信息",
  "应急管理与风险沟通",
  "社会保障与公共福利",
  "城市形象与文化活动",
  "经济与发展建设",
  "社会治理与执法通报",
  "群众动员与社会参与",
  "政策与政务公开",
  "时政与领导活动",
  "意识形态与宣传教育"
)

metric_map <- list(
  "Reads" = "read_num",
  "Likes" = "like_num",
  "Shares" = "share_num",
  "Zaikan" = "look_num",
  "Collects" = "collect_num"
)

fmt_int <- function(x) {
  format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)
}

fmt_mean_sd <- function(mean_x, sd_x) {
  paste0(fmt_int(mean_x), " (", fmt_int(sd_x), ")")
}

overall_rows <- data.table::rbindlist(lapply(names(metric_map), function(metric_name) {
  v <- dt[[metric_map[[metric_name]]]]
  data.table::data.table(
    Variable = metric_name,
    Min = min(v, na.rm = TRUE),
    Max = max(v, na.rm = TRUE),
    Mean = mean(v, na.rm = TRUE),
    Median = stats::median(v, na.rm = TRUE),
    SD = stats::sd(v, na.rm = TRUE)
  )
}))

family_table <- dt[
  ,
  .(
    `Posts (M)` = .N / 1e6,
    `Reads M (SD)` = fmt_mean_sd(mean(read_num), stats::sd(read_num)),
    `Likes M (SD)` = fmt_mean_sd(mean(like_num), stats::sd(like_num)),
    `Shares M (SD)` = fmt_mean_sd(mean(share_num), stats::sd(share_num)),
    `Zaikan M (SD)` = fmt_mean_sd(mean(look_num), stats::sd(look_num)),
    `Collects M (SD)` = fmt_mean_sd(mean(collect_num), stats::sd(collect_num))
  ),
  by = content_family
]
family_table[, Group := unname(family_gloss[content_family])]
family_table[, Group := factor(Group, levels = unname(family_gloss[family_order]))]
data.table::setorder(family_table, Group)
family_table[, Group := as.character(Group)]
family_table <- family_table[, .(
  Group,
  `Posts (M)`,
  `Reads M (SD)`,
  `Likes M (SD)`,
  `Shares M (SD)`,
  `Zaikan M (SD)`,
  `Collects M (SD)`
)]

category_table <- dt[
  ,
  .(
    `Posts (M)` = .N / 1e6,
    `Reads M (SD)` = fmt_mean_sd(mean(read_num), stats::sd(read_num)),
    `Likes M (SD)` = fmt_mean_sd(mean(like_num), stats::sd(like_num)),
    `Shares M (SD)` = fmt_mean_sd(mean(share_num), stats::sd(share_num)),
    `Zaikan M (SD)` = fmt_mean_sd(mean(look_num), stats::sd(look_num)),
    `Collects M (SD)` = fmt_mean_sd(mean(collect_num), stats::sd(collect_num))
  ),
  by = .(category, content_family)
]
category_table[, Family := unname(family_gloss[content_family])]
category_table[, Category := unname(label_short[category])]
category_table[, category := factor(category, levels = category_order)]
data.table::setorder(category_table, category)
category_table <- category_table[, .(
  Category,
  Family,
  `Posts (M)`,
  `Reads M (SD)`,
  `Likes M (SD)`,
  `Shares M (SD)`,
  `Zaikan M (SD)`,
  `Collects M (SD)`
)]

family_mean_reads <- dt[, .(mean_reads = mean(read_num)), by = content_family]
family_mean_reads[, Group := unname(family_gloss[content_family])]
top_family <- family_mean_reads$Group[which.max(family_mean_reads$mean_reads)]

write_tex_table(
  overall_rows,
  file.path(paths$tables, "main_descriptive_overall_stats.tex"),
  caption = "Overall descriptive statistics for the main article-level engagement variables.",
  label = "tab:main-descriptive-overall-stats",
  digits = c(Min = 0L, Max = 0L, Mean = 0L, Median = 0L, SD = 0L)
)

write_tex_table(
  family_table,
  file.path(paths$tables, "main_descriptive_by_family.tex"),
  caption = "Family-level descriptive statistics reported as means with standard deviations in parentheses.",
  label = "tab:main-descriptive-by-family",
  digits = c(`Posts (M)` = 2L),
  align = "lrrrrrr"
)

write_tex_table(
  category_table,
  file.path(paths$tables, "main_descriptive_by_category.tex"),
  caption = "Category-level descriptive statistics reported as means with standard deviations in parentheses.",
  label = "tab:main-descriptive-by-category",
  digits = c(`Posts (M)` = 2L),
  align = "llrrrrrr"
)

memo_lines <- c(
  "# 2026-03-30 Main-Text Descriptives",
  "",
  "Outputs:",
  paste0("- ", file.path(paths$tables, "main_descriptive_overall_stats.tex")),
  paste0("- ", file.path(paths$tables, "main_descriptive_by_family.tex")),
  paste0("- ", file.path(paths$tables, "main_descriptive_by_category.tex")),
  "",
  "High-level takeaways:",
  paste0("- Mean reads: ", fmt_int(mean(dt$read_num)), "; median reads: ", fmt_int(stats::median(dt$read_num)),
         "; sd reads: ", fmt_int(stats::sd(dt$read_num)), "."),
  paste0("- Mean likes: ", fmt_int(mean(dt$like_num)), "; median likes: ", fmt_int(stats::median(dt$like_num)),
         "; sd likes: ", fmt_int(stats::sd(dt$like_num)), "."),
  paste0("- Mean shares: ", fmt_int(mean(dt$share_num)), "; median shares: ", fmt_int(stats::median(dt$share_num)),
         "; sd shares: ", fmt_int(stats::sd(dt$share_num)), "."),
  paste0("- Mean zaikan: ", fmt_int(mean(dt$look_num)), "; median zaikan: ", fmt_int(stats::median(dt$look_num)),
         "; sd zaikan: ", fmt_int(stats::sd(dt$look_num)), "."),
  paste0("- Mean collects: ", fmt_int(mean(dt$collect_num)), "; median collects: ", fmt_int(stats::median(dt$collect_num)),
         "; sd collects: ", fmt_int(stats::sd(dt$collect_num)), "."),
  paste0("- Highest mean reads family: ", top_family, ".")
)

writeLines(
  memo_lines,
  con = file.path(memo_dir, "2026-03-30_main_descriptives.md"),
  useBytes = TRUE
)
