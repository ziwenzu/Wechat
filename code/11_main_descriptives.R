bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)
memo_dir <- file.path(paths$root, "memo")
fs::dir_create(memo_dir, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

label_gloss <- c(
  "意识形态与宣传教育" = "Ideological Education",
  "时政与领导活动" = "Leadership and Political Events",
  "公共服务信息" = "Public Service Information",
  "社会保障与公共福利" = "Social Protection and Welfare",
  "应急管理与风险沟通" = "Emergency and Risk Communication",
  "政策与政务公开" = "Policy and Administrative Disclosure",
  "社会治理与执法通报" = "Social Governance and Enforcement",
  "群众动员与社会参与" = "Mass Mobilization and Participation",
  "经济与发展建设" = "Economic Development and Construction",
  "城市形象与文化活动" = "City Image and Cultural Activity"
)

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
family_colors <- c(
  "Public Service" = "#0f6b6f",
  "Soft Propaganda" = "#b45309",
  "State Governance" = "#4b5563",
  "Hard Propaganda" = "#9f1d20"
)

summarize_block <- function(d, total_n) {
  data.table::data.table(
    posts_m = nrow(d) / 1e6,
    post_share_pct = 100 * nrow(d) / total_n,
    mean_read = mean(d$read_num, na.rm = TRUE),
    median_read = stats::median(d$read_num, na.rm = TRUE),
    topcode_share_pct = 100 * mean(d$read_num >= 100001, na.rm = TRUE),
    like_read_pct = 100 * mean(d$like_num / d$read_num, na.rm = TRUE),
    share_read_pct = 100 * mean(d$share_num / d$read_num, na.rm = TRUE),
    look_read_pct = 100 * mean(d$look_num / d$read_num, na.rm = TRUE),
    collect_read_pct = 100 * mean(d$collect_num / d$read_num, na.rm = TRUE)
  )
}

overall_total <- summarize_block(dt, nrow(dt))
overall_total[, group := "Overall"]

family_summary <- dt[
  ,
  summarize_block(.SD, nrow(dt)),
  by = content_family
]
family_summary[, group := unname(family_gloss[content_family])]
family_summary <- family_summary[, .(
  group,
  posts_m,
  post_share_pct,
  mean_read,
  median_read,
  topcode_share_pct,
  like_read_pct,
  share_read_pct,
  look_read_pct,
  collect_read_pct
)]
family_summary[, group := factor(group, levels = unname(family_gloss[family_order]))]
data.table::setorder(family_summary, group)
family_summary[, group := as.character(group)]

overall_table <- data.table::rbindlist(list(
  overall_total[, .(
    group,
    posts_m,
    post_share_pct,
    mean_read,
    median_read,
    topcode_share_pct,
    like_read_pct,
    share_read_pct,
    look_read_pct,
    collect_read_pct
  )],
  family_summary
), use.names = TRUE)

category_summary <- dt[
  ,
  summarize_block(.SD, nrow(dt)),
  by = .(category, content_family)
]
category_summary[, family := unname(family_gloss[content_family])]
category_summary[, detailed_label := unname(label_gloss[category])]
category_summary[, short_label := unname(label_short[category])]
category_summary[, family := factor(family, levels = unname(family_gloss[family_order]))]
data.table::setorder(category_summary, family, -mean_read)
category_summary[, family := as.character(family)]

category_table <- category_summary[, .(
  detailed_label,
  family,
  post_share_pct,
  mean_read,
  median_read,
  topcode_share_pct,
  like_read_pct,
  share_read_pct,
  look_read_pct,
  collect_read_pct
)]

write_tex_table(
  overall_table,
  file.path(paths$tables, "main_descriptive_overall_family.tex"),
  caption = "Overall and family-level descriptive summary for the instructional WeChat corpus.",
  label = "tab:main-descriptive-overall-family",
  digits = c(
    posts_m = 2L,
    post_share_pct = 1L,
    mean_read = 0L,
    median_read = 0L,
    topcode_share_pct = 2L,
    like_read_pct = 2L,
    share_read_pct = 2L,
    look_read_pct = 2L,
    collect_read_pct = 2L
  )
)

write_tex_table(
  category_table,
  file.path(paths$tables, "main_descriptive_by_category.tex"),
  caption = "Category-level descriptive summary for the instructional WeChat corpus.",
  label = "tab:main-descriptive-by-category",
  digits = c(
    post_share_pct = 1L,
    mean_read = 0L,
    median_read = 0L,
    topcode_share_pct = 2L,
    like_read_pct = 2L,
    share_read_pct = 2L,
    look_read_pct = 2L,
    collect_read_pct = 2L
  ),
  align = "llrrrrrrrr"
)

plot_data <- data.table::rbindlist(list(
  category_summary[, .(
    short_label,
    family,
    metric = "Mean reads",
    value = mean_read
  )],
  category_summary[, .(
    short_label,
    family,
    metric = "Like / read (%)",
    value = like_read_pct
  )],
  category_summary[, .(
    short_label,
    family,
    metric = "Share / read (%)",
    value = share_read_pct
  )],
  category_summary[, .(
    short_label,
    family,
    metric = "Look / read (%)",
    value = look_read_pct
  )],
  category_summary[, .(
    short_label,
    family,
    metric = "Collect / read (%)",
    value = collect_read_pct
  )]
))

category_levels <- category_summary[, short_label]
plot_data[, short_label := factor(short_label, levels = rev(category_levels))]
plot_data[, metric := factor(
  metric,
  levels = c("Mean reads", "Like / read (%)", "Share / read (%)", "Look / read (%)", "Collect / read (%)")
)]

descriptive_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = value, y = short_label, color = family)
) +
  ggplot2::geom_point(size = 2.2, alpha = 0.95) +
  ggplot2::facet_wrap(~metric, scales = "free_x", ncol = 3) +
  ggplot2::scale_color_manual(values = family_colors) +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Category-level descriptive patterns in the instructional WeChat corpus"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.y = ggplot2::element_text(size = 8)
  )

ggplot2::ggsave(
  filename = file.path(paths$figures, "main_descriptive_by_category.pdf"),
  plot = descriptive_plot,
  width = 11,
  height = 7.5,
  units = "in",
  device = "pdf"
)

memo_lines <- c(
  "# 2026-03-30 Main-Text Descriptives",
  "",
  "Outputs:",
  paste0("- ", file.path(paths$tables, "main_descriptive_overall_family.tex")),
  paste0("- ", file.path(paths$tables, "main_descriptive_by_category.tex")),
  paste0("- ", file.path(paths$figures, "main_descriptive_by_category.pdf")),
  "",
  "High-level takeaways:",
  sprintf("- Overall mean reads: %.1f; median reads: %.0f.", overall_total$mean_read, overall_total$median_read),
  sprintf("- Overall like/read ratio: %.2f%%; share/read ratio: %.2f%%; look/read ratio: %.2f%%; collect/read ratio: %.2f%%.",
          overall_total$like_read_pct,
          overall_total$share_read_pct,
          overall_total$look_read_pct,
          overall_total$collect_read_pct),
  sprintf("- Highest-read family: %s (mean reads %.0f).",
          family_summary$group[which.max(family_summary$mean_read)],
          max(family_summary$mean_read)),
  sprintf("- Lowest-read family: %s (mean reads %.0f).",
          family_summary$group[which.min(family_summary$mean_read)],
          min(family_summary$mean_read)),
  sprintf("- Highest like/read category: %s (%.2f%%).",
          category_summary$short_label[which.max(category_summary$like_read_pct)],
          max(category_summary$like_read_pct)),
  sprintf("- Highest share/read category: %s (%.2f%%).",
          category_summary$short_label[which.max(category_summary$share_read_pct)],
          max(category_summary$share_read_pct))
)

writeLines(memo_lines, file.path(memo_dir, "2026-03-30_main_descriptives.md"))
