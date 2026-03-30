bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

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
  "confidence"
)

message("Reading source data from: ", paths$articles_csv)
dt <- data.table::fread(
  input = paths$articles_csv,
  select = selected_cols,
  encoding = "UTF-8",
  showProgress = TRUE
)

data.table::setnames(dt, old = c("YEAR", "DATE"), new = c("year", "date_mmdd"))
dt[, publish_date := as.Date(sprintf("%04d-%s", as.integer(year), date_mmdd))]
dt[, raw_category := category]

canonical_categories <- content_map$category
canonical_lookup <- data.table::as.data.table(content_map)[
  ,
  .(canonical_category = category, content_family)
]

label_gloss_map <- c(
  "意识形态与宣传教育" = "Ideology and propaganda education",
  "时政与领导活动" = "Current affairs and leadership activity",
  "公共服务信息" = "Public service information",
  "社会保障与公共福利" = "Social security and public welfare",
  "应急管理与风险沟通" = "Emergency management and risk communication",
  "政策与政务公开" = "Policy and government disclosure",
  "社会治理与执法通报" = "Social governance and law-enforcement bulletins",
  "群众动员与社会参与" = "Mass mobilization and social participation",
  "经济与发展建设" = "Economic and development construction",
  "城市形象与文化活动" = "City image and cultural activities",
  "纪检监察与党风廉政" = "Party discipline and clean government",
  "公众服务信息" = "Public service information (variant)",
  "教育医疗养老" = "Education, health, and elder care",
  "文化活动与城市形象" = "Cultural activities and city image",
  "政治与政务公开" = "Politics and government disclosure",
  "宣传教育与意识形态" = "Propaganda education and ideology",
  "社会动员与社会参与" = "Social mobilization and social participation",
  "纪委监察与党风廉政" = "Discipline inspection and clean government",
  "教育与发展建设" = "Education and development construction",
  "环境保护与生态治理" = "Environmental protection and ecological governance",
  "环境保护与治理" = "Environmental protection and governance",
  "人事任免与干部公示" = "Personnel appointments and cadre disclosure",
  "文化形象与城市活动" = "Cultural image and city activities",
  "文化活动" = "Cultural activities",
  "环境保护与生态建设" = "Environmental protection and ecological construction",
  "人事任免" = "Personnel appointments",
  "未分类" = "Unclassified",
  "residual_uncertain" = "Residual or uncertain",
  "其他残差标签" = "Other residual labels"
)

label_gloss <- function(x) {
  out <- unname(label_gloss_map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

family_gloss_map <- c(
  "hard_propaganda" = "Hard propaganda",
  "public_service" = "Public service",
  "state_governance" = "State governance",
  "soft_propaganda" = "Soft propaganda"
)

family_gloss <- function(x) {
  out <- unname(family_gloss_map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

dt[, raw_category := as.character(raw_category)]
dt[, is_exact_canonical := raw_category %in% canonical_categories]

total_posts <- nrow(dt)
raw_label_counts <- dt[
  ,
  .(n_posts = .N),
  by = raw_category
][order(-n_posts, raw_category)]

raw_label_counts[, share_total := n_posts / total_posts]

canonical_counts <- merge(
  data.table::data.table(canonical_category = canonical_categories),
  raw_label_counts[raw_category %in% canonical_categories, .(
    canonical_category = raw_category,
    n_posts,
    share_total
  )],
  by = "canonical_category",
  all.x = TRUE,
  sort = FALSE
)
canonical_counts[is.na(n_posts), `:=`(n_posts = 0L, share_total = 0)]
canonical_counts <- merge(
  canonical_counts,
  canonical_lookup,
  by = "canonical_category",
  all.x = TRUE,
  sort = FALSE
)
canonical_counts[, canonical_label := label_gloss(canonical_category)]
canonical_counts[, content_family := family_gloss(content_family)]
data.table::setcolorder(
  canonical_counts,
  c("canonical_label", "canonical_category", "content_family", "n_posts", "share_total")
)
canonical_counts <- canonical_counts[
  ,
  .(
    `Canonical label` = canonical_label,
    `Analytic family` = content_family,
    `Posts` = n_posts,
    `Share of corpus` = share_total
  )
]

residual_counts <- raw_label_counts[!raw_category %in% canonical_categories]
residual_total <- residual_counts[, sum(n_posts)]
residual_counts[, share_residual := if (residual_total > 0) n_posts / residual_total else 0]

overview_table <- data.table::data.table(
  Metric = c(
    "Total posts",
    "Distinct raw label strings",
    "Posts with exact canonical labels",
    "Share with exact canonical labels",
    "Posts in residual raw-label pool",
    "Share in residual raw-label pool"
  ),
  Value = c(
    format(total_posts, big.mark = ",", scientific = FALSE, trim = TRUE),
    format(data.table::uniqueN(dt$raw_category), big.mark = ",", scientific = FALSE, trim = TRUE),
    format(sum(canonical_counts$Posts), big.mark = ",", scientific = FALSE, trim = TRUE),
    sprintf("%.4f", sum(canonical_counts$`Share of corpus`)),
    format(residual_total, big.mark = ",", scientific = FALSE, trim = TRUE),
    sprintf("%.4f", residual_total / total_posts)
  )
)

top_residual <- residual_counts[
  1:min(.N, 12),
  .(
    `Raw label` = label_gloss(raw_category),
    `Posts` = n_posts,
    `Share of corpus` = share_total,
    `Share of residual pool` = share_residual
  )
]

lookup_count <- function(label) {
  match_idx <- raw_label_counts$raw_category == label
  if (!any(match_idx)) {
    return(0L)
  }
  raw_label_counts[match_idx, n_posts][1]
}

normalization_examples <- data.table::data.table(
  raw_label = c(
    "纪检监察与党风廉政",
    "公众服务信息",
    "文化活动与城市形象",
    "文化形象与城市活动",
    "政治与政务公开",
    "宣传教育与意识形态",
    "社会动员与社会参与",
    "纪委监察与党风廉政",
    "环境保护与生态治理",
    "未分类"
  ),
  normalized_label = c(
    "意识形态与宣传教育",
    "公共服务信息",
    "城市形象与文化活动",
    "城市形象与文化活动",
    "政策与政务公开",
    "意识形态与宣传教育",
    "群众动员与社会参与",
    "意识形态与宣传教育",
    "residual_uncertain",
    "residual_uncertain"
  ),
  rationale = c(
    "Political-discipline and party-cleanliness content is treated as explicit political education.",
    "Orthographic variant of the canonical public-service label.",
    "Word-order variant of the canonical city-image label.",
    "Close lexical variant of the canonical city-image label.",
    "Near-synonym of policy and government disclosure.",
    "Word-order variant of the canonical ideology label.",
    "Close lexical variant of collective mobilization and participation.",
    "Close lexical variant of party-discipline content.",
    "Environmental labels remain residual when they cannot be assigned cleanly to one canonical class.",
    "Uninformative labels remain residual rather than being forced into a substantive class."
  )
)
normalization_examples[, n_posts := vapply(raw_label, lookup_count, integer(1))]
normalization_examples[, raw_label_gloss := label_gloss(raw_label)]
normalization_examples[, normalized_label_gloss := label_gloss(normalized_label)]
normalization_examples <- normalization_examples[
  ,
  .(
    `Raw label` = raw_label_gloss,
    `Posts` = n_posts,
    `Normalized label` = normalized_label_gloss,
    `Normalization rationale` = rationale
  )
]

discovery_plan <- data.table::data.table(
  discovery_stratum = c(
    canonical_categories,
    "纪检监察与党风廉政",
    "公众服务信息",
    "其他残差标签"
  ),
  target_n = c(rep(25L, length(canonical_categories)), 75L, 25L, 50L)
)

sample_stratum <- function(data, n, stratum_label) {
  if (nrow(data) == 0L || n <= 0L) {
    return(data.table::data.table())
  }

  take_n <- min(nrow(data), as.integer(n))
  sampled <- data[sample(.N, take_n)]
  sampled[, discovery_stratum := stratum_label]
  sampled
}

set.seed(20260329L)

discovery_columns <- c(
  "id",
  "province",
  "city",
  "public_account_name",
  "title",
  "publish_date",
  "raw_category",
  "confidence",
  "discovery_stratum"
)

discovery_parts <- lapply(canonical_categories, function(label) {
  sample_stratum(
    data = dt[raw_category == label],
    n = discovery_plan[discovery_stratum == label, target_n],
    stratum_label = label
  )
})

discovery_parts[[length(discovery_parts) + 1L]] <- sample_stratum(
  data = dt[raw_category == "纪检监察与党风廉政"],
  n = discovery_plan[discovery_stratum == "纪检监察与党风廉政", target_n],
  stratum_label = "纪检监察与党风廉政"
)

discovery_parts[[length(discovery_parts) + 1L]] <- sample_stratum(
  data = dt[raw_category == "公众服务信息"],
  n = discovery_plan[discovery_stratum == "公众服务信息", target_n],
  stratum_label = "公众服务信息"
)

discovery_parts[[length(discovery_parts) + 1L]] <- sample_stratum(
  data = dt[
    !raw_category %in% c(canonical_categories, "纪检监察与党风廉政", "公众服务信息")
  ],
  n = discovery_plan[discovery_stratum == "其他残差标签", target_n],
  stratum_label = "其他残差标签"
)

discovery_sample <- data.table::rbindlist(discovery_parts, use.names = TRUE, fill = TRUE)
discovery_sample <- discovery_sample[, ..discovery_columns]
data.table::setorder(discovery_sample, discovery_stratum, publish_date, id)

discovery_sample_path <- file.path(paths$data, "discovery_sample_instructional.rds")
saveRDS(discovery_sample, discovery_sample_path)

discovery_summary <- merge(
  discovery_plan,
  discovery_sample[
    ,
    .(
      realized_n = .N,
      unique_raw_labels = data.table::uniqueN(raw_category)
    ),
    by = discovery_stratum
  ],
  by = "discovery_stratum",
  all.x = TRUE,
  sort = FALSE
)
discovery_summary[is.na(realized_n), `:=`(realized_n = 0L, unique_raw_labels = 0L)]
discovery_summary[, discovery_stratum_label := label_gloss(discovery_stratum)]
discovery_summary <- discovery_summary[
  ,
  .(
    `Discovery stratum` = discovery_stratum_label,
    `Target posts` = target_n,
    `Realized posts` = realized_n,
    `Unique raw labels` = unique_raw_labels
  )
]

canonical_lookup[, canonical_label := label_gloss(canonical_category)]
canonical_lookup[, content_family := family_gloss(content_family)]
canonical_lookup <- canonical_lookup[
  ,
  .(
    `Canonical label` = canonical_label,
    `Analytic family` = content_family
  )
]

write_tex_table(
  overview_table,
  file.path(paths$tables, "taxonomy_summary_overview.tex"),
  caption = "Overview of raw and canonical category coverage in the instructional corpus.",
  label = "tab:taxonomy-overview",
  align = "ll"
)

write_tex_table(
  canonical_counts,
  file.path(paths$tables, "taxonomy_canonical_distribution.tex"),
  caption = "Exact-match counts for the ten canonical taxonomy labels used in the instructional dataset.",
  label = "tab:taxonomy-canonical-distribution",
  digits = c(`Posts` = 0, `Share of corpus` = 4),
  align = "p{2.35in}p{1.35in}rr"
)

write_tex_table(
  discovery_summary,
  file.path(paths$tables, "taxonomy_discovery_sample_design.tex"),
  caption = "Instructor-prepared discovery sample design and realized coverage.",
  label = "tab:taxonomy-discovery-sample",
  digits = c(`Target posts` = 0, `Realized posts` = 0, `Unique raw labels` = 0),
  align = "p{2.6in}rrr"
)

write_tex_table(
  top_residual,
  file.path(paths$tables, "taxonomy_top_residual_labels.tex"),
  caption = "Most common residual raw labels outside the ten canonical taxonomy labels.",
  label = "tab:taxonomy-top-residual-labels",
  digits = c(`Posts` = 0, `Share of corpus` = 4, `Share of residual pool` = 4),
  align = "p{2.35in}rrr"
)

write_tex_table(
  normalization_examples,
  file.path(paths$tables, "taxonomy_normalization_examples.tex"),
  caption = "Illustrative raw-label normalization rules used in the instructor-prepared taxonomy reconstruction.",
  label = "tab:taxonomy-normalization-examples",
  digits = c(`Posts` = 0),
  align = "p{1.7in}rp{1.7in}p{2.3in}"
)

write_tex_table(
  canonical_lookup,
  file.path(paths$tables, "taxonomy_family_mapping.tex"),
  caption = "Mapping from canonical category labels to theory-facing analytic families.",
  label = "tab:taxonomy-family-mapping",
  align = "p{2.35in}p{1.45in}"
)

message("Wrote discovery sample to: ", discovery_sample_path)
message("Finished taxonomy appendix outputs.")
