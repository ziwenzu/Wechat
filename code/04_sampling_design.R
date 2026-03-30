bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

args <- parse_args()
paths <- project_paths()
fs::dir_create(paths$data, recurse = TRUE)

set.seed(20260329L)

d <- data.table::as.data.table(
  readRDS(file.path(paths$data, "wechat_posts_clean.rds"))
)

message("Loaded cleaned dataset with ", nrow(d), " rows.")

if (!all(c("category_raw", "category", "content_family") %in% names(d))) {
  stop("wechat_posts_clean.rds is missing cleaned taxonomy columns.", call. = FALSE)
}

d[, category_raw_clean := trimws(as.character(category_raw))]
d[, year := as.integer(year)]

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

east_provinces <- c("北京", "天津", "河北", "上海", "江苏", "浙江", "福建", "山东", "广东", "海南")
central_provinces <- c("山西", "安徽", "江西", "河南", "湖北", "湖南")
west_provinces <- c("内蒙古", "广西", "重庆", "四川", "贵州", "云南", "西藏", "陕西", "甘肃", "青海", "宁夏", "新疆")
northeast_provinces <- c("辽宁", "吉林", "黑龙江")

derive_macro_region <- function(province) {
  out <- rep("other", length(province))
  out[province %in% east_provinces] <- "east"
  out[province %in% central_provinces] <- "central"
  out[province %in% west_provinces] <- "west"
  out[province %in% northeast_provinces] <- "northeast"
  out
}

derive_account_type <- function(account_name) {
  account_name <- as.character(account_name)
  account_name[is.na(account_name)] <- ""
  out <- rep("general_government", length(account_name))

  sectoral_pattern <- paste(
    c(
      "教育", "人社", "医保", "卫健", "健康", "文旅", "住建",
      "生态环境", "农业", "民政", "交通", "统计", "税务", "市场监管"
    ),
    collapse = "|"
  )
  legal_pattern <- paste(
    c(
      "公安", "交警", "法院", "检察", "司法", "法治", "应急", "城管"
    ),
    collapse = "|"
  )
  media_pattern <- paste(
    c(
      "发布", "政务", "政府", "宣传", "融媒", "新闻"
    ),
    collapse = "|"
  )

  out[grepl(sectoral_pattern, account_name, perl = TRUE)] <- "sectoral_service"
  out[grepl(legal_pattern, account_name, perl = TRUE)] <- "legal_enforcement"
  out[grepl(media_pattern, account_name, perl = TRUE)] <- "general_government"
  out
}

sample_diverse <- function(data, n) {
  if (nrow(data) == 0L || n <= 0L) {
    return(data[0])
  }

  take_n <- min(nrow(data), as.integer(n))
  shuffled <- data[sample(.N)]
  shuffled[, diversify_key := paste(macro_region, account_type_proxy, province, sep = " | ")]
  first_pass <- shuffled[!duplicated(diversify_key)]

  if (nrow(first_pass) >= take_n) {
    return(first_pass[1:take_n][, diversify_key := NULL])
  }

  remaining <- shuffled[!id %in% first_pass$id]
  supplement_n <- min(take_n - nrow(first_pass), nrow(remaining))

  data.table::rbindlist(
    list(
      first_pass,
      remaining[1:supplement_n]
    ),
    use.names = TRUE,
    fill = TRUE
  )[, diversify_key := NULL]
}

count_pattern_hits <- function(text, pattern_list) {
  vapply(
    text,
    function(single_text) {
      sum(vapply(pattern_list, function(pattern) grepl(pattern, single_text, perl = TRUE), logical(1)))
    },
    numeric(1)
  )
}

d[, macro_region := derive_macro_region(province)]
d[, account_type_proxy := derive_account_type(public_account_name)]
d[, time_bin := data.table::fcase(
  year %in% 2015:2016, "2015-2016",
  year %in% 2017:2018, "2017-2018",
  year %in% 2019:2020, "2019-2020",
  year %in% 2021:2022, "2021-2022",
  year %in% 2023:2024, "2023-2024",
  default = "other"
)]

message("Prepared macro-region, account-type, and time-bin fields.")

round1 <- d[
  ,
  sample_diverse(.SD, n = 4),
  by = .(time_bin, category)
]
message("Finished Round 1 discovery sampling.")
round1[, discovery_round := "round1"]
round1[, discovery_stratum := paste(time_bin, category, sep = " | ")]

raw_counts <- d[, .N, by = category_raw_clean]
d <- merge(
  d,
  raw_counts,
  by = "category_raw_clean",
  all.x = TRUE,
  sort = FALSE
)
data.table::setnames(d, "N", "raw_label_count")
message("Built raw-label frequency lookup.")

domain_patterns <- c(
  leadership = "时政|领导|干部公示|人事任免",
  ideology = "意识形态|宣传教育|纪检|党风廉政|红色|党务|党的建设",
  service = "公共服务|服务信息|公告与通知|生活小贴士|办事|指南",
  welfare = "社会保障|公共福利|教育|医疗|卫生|健康|招聘|人才|养老",
  emergency = "应急|风险沟通|疫情|防控|预警|安全生产|气象|天气",
  policy = "政策|政务公开|公示|预算|统计数据|工作汇报",
  governance = "社会治理|执法|司法|法律|公安|市场监管|整治|处罚",
  mobilization = "社会参与|群众动员|志愿|投票|征集|倡议|文明实践",
  development = "经济|发展建设|项目|招商|产业|乡村振兴|农业|基础设施",
  culture = "城市形象|文化活动|历史文化|文旅|旅游|节庆|赛事|地方故事"
)

raw_domain_lookup <- unique(d[, .(category_raw_clean)])
raw_domain_lookup[, mixed_domain_score := count_pattern_hits(category_raw_clean, domain_patterns)]
d <- merge(
  d,
  raw_domain_lookup,
  by = "category_raw_clean",
  all.x = TRUE,
  sort = FALSE
)
message("Built mixed-domain lookup.")

round2_generic <- sample_diverse(
  d[category_raw_clean %in% generic_raw_labels & !id %in% round1$id],
  n = 50
)
round2_generic[, boundary_stratum := "generic_or_low_information_raw_label"]

round2_lowconf <- sample_diverse(
  d[confidence <= 0.6 & !id %in% c(round1$id, round2_generic$id)],
  n = 50
)
round2_lowconf[, boundary_stratum := "low_confidence_existing_label"]

round2_longtail <- sample_diverse(
  d[raw_label_count <= 5 & !id %in% c(round1$id, round2_generic$id, round2_lowconf$id)],
  n = 50
)
round2_longtail[, boundary_stratum := "long_tail_raw_label"]

round2_mixed <- sample_diverse(
  d[mixed_domain_score >= 2 & !id %in% c(round1$id, round2_generic$id, round2_lowconf$id, round2_longtail$id)],
  n = 50
)
round2_mixed[, boundary_stratum := "mixed_domain_raw_label"]

round2 <- data.table::rbindlist(
  list(round2_generic, round2_lowconf, round2_longtail, round2_mixed),
  use.names = TRUE,
  fill = TRUE
)
message("Finished Round 2 discovery sampling.")
round2[, discovery_round := "round2"]
round2[, discovery_stratum := boundary_stratum]

discovery_sample <- data.table::rbindlist(
  list(round1, round2),
  use.names = TRUE,
  fill = TRUE
)
data.table::setorder(discovery_sample, discovery_round, discovery_stratum, publish_date, id)

discovery_keep <- c(
  "id",
  "province",
  "city",
  "macro_region",
  "public_account_name",
  "account_type_proxy",
  "title",
  "publish_date",
  "year",
  "category_raw",
  "reason",
  "keywords",
  "confidence",
  "category",
  "content_family",
  "discovery_round",
  "discovery_stratum"
)
discovery_sample <- discovery_sample[, ..discovery_keep]
message("Assembled discovery sample.")

city_counts <- d[, .N, by = city][order(city)]
city_counts[, quota_raw := 2000 * N / sum(N)]
city_counts[, target_n := pmax(1L, floor(quota_raw))]
city_counts[, remainder := quota_raw - floor(quota_raw)]

current_total <- city_counts[, sum(target_n)]
if (current_total < 2000) {
  add_n <- 2000 - current_total
  add_idx <- city_counts[order(-remainder, city), head(.I, add_n)]
  city_counts[add_idx, target_n := target_n + 1L]
} else if (current_total > 2000) {
  drop_n <- current_total - 2000
  drop_idx <- city_counts[target_n > 1L][order(remainder, city), head(.I, drop_n)]
  city_counts[drop_idx, target_n := pmax(1L, target_n - 1L)]
}

annotation_internal <- merge(
  d,
  city_counts[, .(city, city_posts = N, city_target_n = target_n)],
  by = "city",
  all.x = TRUE,
  sort = FALSE
)[
  ,
  .SD[sample(.N, min(.N, unique(city_target_n)))],
  by = city
]
message("Finished city-weighted annotation sampling.")

annotation_internal <- annotation_internal[sample(.N)]
annotation_internal[, annotation_id := sprintf("ann_%04d", seq_len(.N))]

annotation_internal_keep <- c(
  "annotation_id",
  "id",
  "province",
  "city",
  "macro_region",
  "public_account_name",
  "account_type_proxy",
  "title",
  "publish_date",
  "year",
  "category_raw",
  "reason",
  "keywords",
  "confidence",
  "category",
  "content_family",
  "city_posts",
  "city_target_n"
)
annotation_internal <- annotation_internal[, ..annotation_internal_keep]

annotation_blind <- data.table::copy(annotation_internal)[
  ,
  .(
    annotation_id,
    id,
    province,
    city,
    macro_region,
    public_account_name,
    account_type_proxy,
    title,
    publish_date,
    year
  )
]
annotation_blind[, `:=`(
  primary_label_ra1 = NA_character_,
  family_label_ra1 = NA_character_,
  primary_label_ra2 = NA_character_,
  family_label_ra2 = NA_character_,
  adjudicated_primary_label = NA_character_,
  adjudicated_family_label = NA_character_,
  annotation_notes = NA_character_
)]

saveRDS(discovery_sample, file.path(paths$data, "discovery_sample_400.rds"))
saveRDS(annotation_internal, file.path(paths$data, "annotation_sample_2000_internal.rds"))
saveRDS(annotation_blind, file.path(paths$data, "annotation_sample_2000_blind.rds"))

message("Saved discovery sample to: ", file.path(paths$data, "discovery_sample_400.rds"))
message("Saved internal annotation sample to: ", file.path(paths$data, "annotation_sample_2000_internal.rds"))
message("Saved blind annotation template to: ", file.path(paths$data, "annotation_sample_2000_blind.rds"))
