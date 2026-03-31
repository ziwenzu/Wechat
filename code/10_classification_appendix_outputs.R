bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "ggwordcloud", "showtext", "sysfonts"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

clean <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_posts_clean.rds")))
audit <- data.table::as.data.table(readRDS(file.path(paths$data, "annotation_sample_2000_blind.rds")))

label_order <- content_map$category
family_order <- c("hard_propaganda", "public_service", "state_governance", "soft_propaganda")

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

family_gloss <- c(
  "hard_propaganda" = "Hard Propaganda",
  "public_service" = "Public Service",
  "state_governance" = "State Governance",
  "soft_propaganda" = "Soft Propaganda"
)

family_cloud_label <- c(
  "hard_propaganda" = "Hard Propaganda\nleaders, Party discipline, political study",
  "public_service" = "Public Service\nhealth, weather, daily services",
  "state_governance" = "State Governance\npolicy, disclosure, enforcement",
  "soft_propaganda" = "Soft Propaganda\ndevelopment, tourism, place branding"
)

family_colors <- c(
  "Hard Propaganda\nleaders, Party discipline, political study" = "#9f1d20",
  "Public Service\nhealth, weather, daily services" = "#0f6b6f",
  "State Governance\npolicy, disclosure, enforcement" = "#4b5563",
  "Soft Propaganda\ndevelopment, tourism, place branding" = "#b45309"
)

label_cues <- c(
  "意识形态与宣传教育" = "Political learning, Party education, discipline, and value transmission",
  "时政与领导活动" = "Leaders, meetings, inspections, speeches, appointments, and deployments",
  "公共服务信息" = "Practical guidance, reminders, procedures, notices, and daily-use service information",
  "社会保障与公共福利" = "Education, health, employment, subsidies, pensions, and welfare provision",
  "应急管理与风险沟通" = "Warnings, hazards, disasters, epidemics, safety alerts, and urgent risk communication",
  "政策与政务公开" = "Policy texts, formal notices, administrative disclosure, budgets, and statistics",
  "社会治理与执法通报" = "Enforcement, punishment, inspections, anti-fraud, and order-maintenance action",
  "群众动员与社会参与" = "Calls for participation, volunteering, campaigns, voting, and civic mobilization",
  "经济与发展建设" = "Projects, investment, industry, infrastructure, agriculture, and development performance",
  "城市形象与文化活动" = "Tourism, culture, festivals, exhibitions, place branding, and local storytelling"
)

label_examples <- c(
  "意识形态与宣传教育" = "\"Study and implement the Party Congress spirit\" style political-study or Party-education title",
  "时政与领导活动" = "\"Municipal Party secretary inspects major projects\" style leader-activity or meeting report",
  "公共服务信息" = "\"Housing provident fund application guide\" style practical procedure or reminder title",
  "社会保障与公共福利" = "\"Medical reimbursement policy adjustment\" style welfare, education, health, or subsidy title",
  "应急管理与风险沟通" = "\"Blue rainstorm warning\" style safety warning, hazard, or epidemic-risk title",
  "政策与政务公开" = "\"2024 government work report\" style policy-release or administrative-disclosure title",
  "社会治理与执法通报" = "\"Special rectification campaign bulletin\" style enforcement or rectification title",
  "群众动员与社会参与" = "\"Civilized city creation initiative\" style participation or volunteering title",
  "经济与发展建设" = "\"Major projects break ground\" style development-performance or infrastructure title",
  "城市形象与文化活动" = "\"Cherry blossom festival opens\" style culture, tourism, festival, or branding title"
)

split_keywords <- function(text_vec) {
  text_vec <- as.character(text_vec)
  text_vec[is.na(text_vec)] <- ""
  text_vec <- gsub("[，,;；|/]+", "、", text_vec)
  text_vec <- gsub("\\s+", "", text_vec)
  unlist(strsplit(text_vec, "、", fixed = TRUE), use.names = FALSE)
}

classification_metrics <- function(truth, pred) {
  keep <- !(is.na(truth) | is.na(pred) | !nzchar(trimws(as.character(truth))) | !nzchar(trimws(as.character(pred))))
  truth <- as.character(truth[keep])
  pred <- as.character(pred[keep])

  classes <- sort(unique(c(truth, pred)))
  by_class <- data.table::rbindlist(lapply(classes, function(cls) {
    tp <- sum(truth == cls & pred == cls)
    fp <- sum(truth != cls & pred == cls)
    fn <- sum(truth == cls & pred != cls)
    precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
      NA_real_
    } else {
      2 * precision * recall / (precision + recall)
    }

    data.table::data.table(
      class = cls,
      precision = precision,
      recall = recall,
      f1 = f1,
      support = sum(truth == cls)
    )
  }))

  list(
    summary = data.table::data.table(
      accuracy = mean(truth == pred),
      macro_f1 = mean(by_class$f1, na.rm = TRUE),
      n_eval = length(truth)
    ),
    by_class = by_class
  )
}

cohen_kappa <- function(x, y) {
  keep <- !(is.na(x) | is.na(y) | !nzchar(trimws(as.character(x))) | !nzchar(trimws(as.character(y))))
  x <- as.character(x[keep])
  y <- as.character(y[keep])
  levs <- sort(unique(c(x, y)))
  tab <- table(factor(x, levels = levs), factor(y, levels = levs))
  n <- sum(tab)
  p0 <- sum(diag(tab)) / n
  px <- rowSums(tab) / n
  py <- colSums(tab) / n
  pe <- sum(px * py)
  (p0 - pe) / (1 - pe)
}

taxonomy_summary <- clean[
  ,
  .(n_posts = .N),
  by = .(category, content_family)
][
  ,
  share_posts := .N
]

taxonomy_summary[, share_posts := n_posts / sum(n_posts)]
taxonomy_summary[, category := factor(category, levels = label_order)]
taxonomy_summary[, content_family := factor(content_family, levels = family_order)]
data.table::setorder(taxonomy_summary, content_family, category)

taxonomy_summary[, category := as.character(category)]
taxonomy_summary[, content_family := as.character(content_family)]

taxonomy_summary_en <- data.table::copy(taxonomy_summary)
taxonomy_summary_en[, detailed_label := unname(label_gloss[category])]
taxonomy_summary_en[, family := unname(family_gloss[content_family])]
taxonomy_summary_en <- taxonomy_summary_en[, .(detailed_label, family, n_posts, share_posts)]

discovery_design <- data.table::data.table(
  phase = c(
    "Round 1",
    "Round 2A",
    "Round 2B",
    "Round 2C",
    "Round 2D"
  ),
  target = c(200, 50, 50, 50, 50),
  rule = c(
    "Balanced grid across five time bins and the ten cleaned labels",
    "Short or low-information titles",
    "Low-confidence titles",
    "Mixed-domain titles spanning multiple coding cues",
    "Adjacent-boundary titles from semantically neighboring categories"
  )
)

category_examples <- unique(clean[, .(category, content_family)])
category_examples[, decision_cue := unname(label_cues[category])]
category_examples[, example_title_pattern := unname(label_examples[category])]
category_examples <- category_examples[, .(category, content_family, decision_cue, example_title_pattern)]
category_examples[, category := factor(category, levels = label_order)]
data.table::setorder(category_examples, category)
category_examples[, category := as.character(category)]

category_examples_en <- data.table::copy(category_examples)
category_examples_en[, detailed_label := unname(label_gloss[category])]
category_examples_en[, family := unname(family_gloss[content_family])]
category_examples_en <- category_examples_en[, .(detailed_label, family, decision_cue, example_title_pattern)]

audit_city <- audit[, .N, by = city]
annotation_summary <- data.table::data.table(
  metric = c(
    "Annotated titles",
    "Cities represented",
    "Provincial-level units represented",
    "Minimum titles per city",
    "Median titles per city",
    "Maximum titles per city"
  ),
  value = c(
    format(nrow(audit), big.mark = ","),
    format(data.table::uniqueN(audit$city), big.mark = ","),
    format(data.table::uniqueN(audit$province), big.mark = ","),
    format(min(audit_city$N), big.mark = ","),
    format(stats::median(audit_city$N), big.mark = ","),
    format(max(audit_city$N), big.mark = ",")
  )
)

reliability_pretty <- data.table::data.table(
  statistic = c(
    "Primary-label exact agreement",
    "Family-label exact agreement",
    "Primary-label Cohen's kappa",
    "Family-label Cohen's kappa",
    "Adjudication rate"
  ),
  value = c(
    mean(audit$primary_label_ra1 == audit$primary_label_ra2, na.rm = TRUE),
    mean(audit$family_label_ra1 == audit$family_label_ra2, na.rm = TRUE),
    cohen_kappa(audit$primary_label_ra1, audit$primary_label_ra2),
    cohen_kappa(audit$family_label_ra1, audit$family_label_ra2),
    mean(
      audit$adjudicated_primary_label != audit$primary_label_ra1 |
        audit$adjudicated_primary_label != audit$primary_label_ra2,
      na.rm = TRUE
    )
  )
)

detailed_metrics <- classification_metrics(audit$adjudicated_primary_label, audit$llm_primary_label)
family_metrics <- classification_metrics(audit$adjudicated_family_label, audit$llm_family_label)

validation_overall_pretty <- data.table::data.table(
  task = c("Detailed labels", "Higher-order families"),
  accuracy = c(detailed_metrics$summary$accuracy, family_metrics$summary$accuracy),
  macro_f1 = c(detailed_metrics$summary$macro_f1, family_metrics$summary$macro_f1),
  evaluation_titles = c(detailed_metrics$summary$n_eval, family_metrics$summary$n_eval)
)

detailed_by_class_en <- data.table::copy(detailed_metrics$by_class)
detailed_by_class_en[, detailed_label := unname(label_gloss[class])]
detailed_by_class_en[, family := unname(family_gloss[unname(content_map$content_family[match(class, content_map$category)])])]
detailed_by_class_en <- detailed_by_class_en[, .(detailed_label, family, precision, recall, f1, support)]

family_by_class_pretty <- data.table::copy(family_metrics$by_class)
family_by_class_pretty[, family := unname(family_gloss[class])]
family_by_class_pretty <- family_by_class_pretty[, .(family, precision, recall, f1, support)]

family_confusion <- audit[
  !is.na(adjudicated_family_label) &
    nzchar(trimws(adjudicated_family_label)) &
    !is.na(llm_family_label) &
    nzchar(trimws(llm_family_label)),
  .N,
  by = .(truth = adjudicated_family_label, pred = llm_family_label)
]

family_levels <- family_order
family_confusion[, truth := factor(truth, levels = family_levels)]
family_confusion[, pred := factor(pred, levels = family_levels)]

full_grid <- data.table::CJ(truth = factor(family_levels, levels = family_levels),
                            pred = factor(family_levels, levels = family_levels),
                            unique = TRUE)
family_confusion <- family_confusion[full_grid, on = .(truth, pred)]
family_confusion[is.na(N), N := 0L]
family_confusion[, row_total := sum(N), by = truth]
family_confusion[, row_pct := ifelse(row_total > 0, N / row_total, 0)]
family_confusion[, label_text := sprintf("%d\n%.0f%%", N, 100 * row_pct)]

keyword_counts <- data.table::rbindlist(
  lapply(family_order, function(family_name) {
    family_keywords <- clean[content_family == family_name & !is.na(keywords) & nzchar(keywords), keywords]
    tokens <- split_keywords(family_keywords)
    tokens <- trimws(tokens)
    tokens <- tokens[
      nzchar(tokens) &
        nchar(tokens) >= 2 &
        !grepl("^[0-9]+$", tokens)
    ]

    token_counts <- sort(table(tokens), decreasing = TRUE)

    data.table::data.table(
      content_family = family_name,
      keyword = names(token_counts),
      n = as.integer(token_counts)
    )
  }),
  use.names = TRUE
)

keyword_docfreq <- keyword_counts[, .(df = .N), by = keyword]
keyword_counts <- keyword_counts[keyword_docfreq, on = "keyword"]
keyword_counts[, family_total := sum(n), by = content_family]
keyword_counts[, tf := n / family_total]
keyword_counts[, idf := log(length(family_order) / df)]
keyword_counts[, tf_idf := tf * idf]
keyword_counts[, plot_score := log1p(n) * idf]
keyword_counts <- keyword_counts[n >= 200 & plot_score > 0]
keyword_counts[, family := unname(family_gloss[content_family])]

keyword_cloud_data <- keyword_counts[
  order(content_family, -plot_score, -n),
  head(.SD, 85),
  by = content_family
]
keyword_cloud_data[, family := factor(
  unname(family_cloud_label[content_family]),
  levels = unname(family_cloud_label[family_order])
)]
keyword_cloud_data[, plot_size := {
  z <- plot_score
  if (length(unique(z)) <= 1L) {
    rep(8, .N)
  } else {
    4.8 + 6.2 * (z - min(z)) / (max(z) - min(z))
  }
}, by = content_family]

if (!"cn_sans" %in% sysfonts::font_families()) {
  sysfonts::font_add("cn_sans", regular = "/System/Library/Fonts/STHeiti Medium.ttc")
}

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

keyword_cloud_plot <- ggplot2::ggplot(
  keyword_cloud_data,
  ggplot2::aes(label = keyword, size = plot_size, color = family)
) +
  ggwordcloud::geom_text_wordcloud_area(
    area_corr = TRUE,
    shape = "square",
    eccentricity = 0.55,
    rstep = 0.006,
    tstep = 0.012,
    perc_step = 0.005,
    grid_size = 1,
    max_grid_size = 160,
    grid_margin = 0,
    max_steps = 30,
    rm_outside = TRUE,
    seed = 1234,
    family = "cn_sans"
  ) +
  ggplot2::facet_wrap(~family, ncol = 2) +
  ggplot2::scale_size_identity() +
  ggplot2::scale_color_manual(values = family_colors, guide = "none") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_family = "cn_sans", base_size = 11) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 11.5, face = "bold", lineheight = 1.05),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.spacing = grid::unit(0.9, "lines")
  )

confusion_plot <- ggplot2::ggplot(
  family_confusion,
  ggplot2::aes(x = pred, y = truth, fill = row_pct)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.3) +
  ggplot2::geom_text(ggplot2::aes(label = label_text), size = 3.2, lineheight = 0.9) +
  ggplot2::scale_fill_gradient(
    low = "#f1f5f9",
    high = "#0f4c81",
    limits = c(0, 1),
    name = "Row share"
  ) +
  ggplot2::labs(
    x = "LLM family label",
    y = "Adjudicated family label"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
    legend.position = "right"
  )

confusion_path <- file.path(paths$figures, "wechat_validation_confusion.pdf")
open_pdf_figure(confusion_path, width = 7.4, height = 5.6, useDingbats = FALSE)
print(confusion_plot)
grDevices::dev.off()

keyword_cloud_path <- file.path(paths$figures, "wechat_keyword_family_clouds.pdf")
open_pdf_figure(keyword_cloud_path, width = 8.8, height = 6.9, useDingbats = FALSE)
print(keyword_cloud_plot)
grDevices::dev.off()

write_tex_table(
  taxonomy_summary_en,
  file.path(paths$tables, "wechat_taxonomy_summary_en.tex"),
  caption = "Final ten-label taxonomy, higher-order families, and corpus shares.",
  label = "tab:wechat-taxonomy-summary-en",
  digits = c(n_posts = 0, share_posts = 3),
  align = "p{0.48\\textwidth}p{0.22\\textwidth}rr"
)

write_tex_table(
  discovery_design,
  file.path(paths$tables, "wechat_discovery_design.tex"),
  caption = "Discovery-sample design used to refine category boundaries and codebook rules.",
  label = "tab:wechat-discovery-design",
  digits = c(target = 0)
)

write_tex_table(
  category_examples_en,
  file.path(paths$tables, "wechat_category_examples_en.tex"),
  caption = "Illustrative coding cues and title patterns for the ten detailed labels.",
  label = "tab:wechat-category-examples-en",
  align = ">{\\raggedright\\arraybackslash}p{0.24\\textwidth}>{\\raggedright\\arraybackslash}p{0.16\\textwidth}>{\\raggedright\\arraybackslash}p{0.24\\textwidth}>{\\raggedright\\arraybackslash}p{0.30\\textwidth}"
)

write_tex_table(
  annotation_summary,
  file.path(paths$tables, "wechat_annotation_sample_summary.tex"),
  caption = "Coverage summary for the 2,000-title validation sample.",
  label = "tab:wechat-annotation-sample-summary"
)

write_tex_table(
  reliability_pretty,
  file.path(paths$tables, "wechat_annotation_reliability_pretty.tex"),
  caption = "Inter-coder reliability for the dual-coded 2,000-title validation sample.",
  label = "tab:wechat-annotation-reliability-pretty",
  digits = c(value = 4)
)

write_tex_table(
  validation_overall_pretty,
  file.path(paths$tables, "wechat_validation_overall_summary_pretty.tex"),
  caption = "Overall validation metrics for detailed-label and family-level LLM coding.",
  label = "tab:wechat-validation-overall-summary-pretty",
  digits = c(accuracy = 4, macro_f1 = 4, evaluation_titles = 0)
)

write_tex_table(
  detailed_by_class_en,
  file.path(paths$tables, "wechat_validation_detailed_by_class_en.tex"),
  caption = "Class-wise validation metrics for the ten detailed labels.",
  label = "tab:wechat-validation-detailed-by-class-en",
  digits = c(precision = 4, recall = 4, f1 = 4, support = 0),
  align = "p{0.38\\textwidth}p{0.22\\textwidth}rrrr"
)

write_tex_table(
  family_by_class_pretty,
  file.path(paths$tables, "wechat_validation_family_by_class_pretty.tex"),
  caption = "Class-wise validation metrics for the four higher-order families.",
  label = "tab:wechat-validation-family-by-class-pretty",
  digits = c(precision = 4, recall = 4, f1 = 4, support = 0)
)

message("Wrote appendix classification tables and figure.")
