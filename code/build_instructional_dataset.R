bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

paths <- project_paths()
source_path <- file.path(paths$data, "wechat_posts_clean.rds")
output_path <- file.path(paths$data, "wechat_instructional_dataset.rds")

cut_like_to_look <- as.Date("2018-12-21")
cut_like_return <- as.Date("2020-07-01")

message("Reading source analysis dataset from: ", source_path)
dt <- data.table::as.data.table(readRDS(source_path))

if (!all(c("like_num", "look_num", "share_num", "collect_num", "read_num", "publish_date") %in% names(dt))) {
  stop("Source dataset is missing one or more required columns.", call. = FALSE)
}

message("Applying instructional-use measurement adjustments.")

# Remove sparse pre-reform traces of the post-2018 public endorsement channel.
dt[publish_date < cut_like_to_look, look_num := 0]

# Hold the legacy like channel at zero until the clean July 2020 reintroduction.
dt[publish_date >= cut_like_to_look & publish_date < cut_like_return, like_num := 0]

message("Rebuilding read counts in the copied dataset.")

set.seed(20260330L)

positive_reads <- dt[read_num > 0]
if (nrow(positive_reads) == 0L) {
  stop("Source dataset does not contain positive read counts for calibration.", call. = FALSE)
}

match_any <- function(pattern) {
  grepl(pattern, dt$title, perl = TRUE) | grepl(pattern, dt$keywords, perl = TRUE)
}

overall_read_median <- positive_reads[, median(read_num)]
overall_sdlog <- positive_reads[, sd(log(pmax(read_num, 1L)))]
if (!is.finite(overall_sdlog) || is.na(overall_sdlog)) {
  overall_sdlog <- 1
}

category_stats <- positive_reads[
  ,
  .(
    cat_n = .N,
    cat_read_median = median(read_num),
    cat_sdlog = sd(log(pmax(read_num, 1L)))
  ),
  by = category
]
category_stats[!is.finite(cat_sdlog) | is.na(cat_sdlog), cat_sdlog := overall_sdlog]
category_stats[, cat_sdlog := pmin(pmax(cat_sdlog, 0.75), 1.20)]

year_stats <- positive_reads[
  ,
  .(year_read_median = median(read_num)),
  by = year
]
year_stats[, year_read_multiplier := pmin(pmax(year_read_median / overall_read_median, 0.75), 1.50)]

category_year_stats <- positive_reads[
  ,
  .(
    cy_n = .N,
    cy_read_median = median(read_num)
  ),
  by = .(category, year)
]
category_year_stats[category_stats, on = "category", cat_read_median := i.cat_read_median]
category_year_stats[year_stats, on = "year", year_read_multiplier := i.year_read_multiplier]
category_year_stats[, smooth_weight := pmin(cy_n / (cy_n + 200), 0.85)]
category_year_stats[
  ,
  base_read_median := exp(
    smooth_weight * log(pmax(cy_read_median, 1)) +
      (1 - smooth_weight) * log(pmax(cat_read_median * year_read_multiplier, 1))
  )
]
category_year_stats <- category_year_stats[, .(category, year, base_read_median)]

account_stats <- positive_reads[
  ,
  .(
    account_positive_n = .N,
    account_read_median = median(read_num)
  ),
  by = public_account_name
]
account_stats[, account_weight := account_positive_n / (account_positive_n + 40)]
account_stats[
  ,
  account_read_multiplier := exp(account_weight * log(pmax(account_read_median, 1) / overall_read_median))
]
account_stats[, account_read_multiplier := pmin(pmax(account_read_multiplier, 0.40), 2.80)]
account_stats <- account_stats[, .(public_account_name, account_read_multiplier)]

dt[category_stats, on = "category", `:=`(
  cat_read_median = i.cat_read_median,
  cat_sdlog = i.cat_sdlog
)]
dt[category_year_stats, on = .(category, year), base_read_median := i.base_read_median]
dt[is.na(base_read_median), base_read_median := cat_read_median]
dt[account_stats, on = "public_account_name", account_read_multiplier := i.account_read_multiplier]
dt[is.na(account_read_multiplier), account_read_multiplier := 1]

dt[, read_generated_flag := as.integer(is.na(read_num) | read_num <= 0)]

big_city_names <- c(
  "北京市", "上海市", "广州市", "深圳市", "成都市", "杭州市", "重庆市", "武汉市",
  "南京市", "西安市", "天津市", "苏州市", "长沙市", "青岛市", "郑州市", "宁波市",
  "厦门市", "合肥市", "东莞市", "佛山市"
)

dt[, is_big_city := city %in% big_city_names]
dt[, is_service_helpful := category %in% c("公共服务信息", "社会保障与公共福利", "应急管理与风险沟通")]
dt[, is_collectible := match_any("攻略|指南|收藏|日历|时间表|课表|考试|报名|成绩|查询|清单|流程|步骤|一图读懂|怎么办|放假安排|求职|招聘|简历|面试|国考|省考|考研|医保|社保|公积金|办事|教程|提醒")]
dt[, is_major_event := match_any("最新|重磅|刚刚|突发|回应|通报|被查|双开|落马|开除党籍|任命|当选|选举|世界|国际|美国|俄罗斯|乌克兰|以色列|巴勒斯坦|联合国|地震|台风|暴雨|洪水|寒潮|高温|红色预警|疫情|油价")]
dt[, is_leader_news := match_any("书记|市长|主席|常委|大会|两会|代表|开幕|闭幕|任命|当选|选举")]
dt[, is_learning_material := match_any("学习|考试|报名|课程|课表|题库|培训|讲座|党课|笔记|安排|日程")]
dt[
  ,
  hot_score := pmin(
    3L,
    as.integer(is_major_event) + as.integer(is_leader_news) + as.integer(is_big_city)
  )
]

max_publish_date <- dt[, max(publish_date, na.rm = TRUE)]
dt[, days_to_sample_end := as.integer(max_publish_date - publish_date)]
dt[
  ,
  recency_read_multiplier := ifelse(
    days_to_sample_end <= 180L,
    0.70 + 0.30 * (days_to_sample_end / 180),
    1.00
  )
]

dt[, total_reaction_num := like_num + share_num + look_num + collect_num]

dt[
  ,
  baseline_read_floor := as.integer(round(
    ifelse(
      is_service_helpful,
      stats::runif(.N, min = 18, max = 55),
      stats::runif(.N, min = 12, max = 38)
    )
  ))
]

dt[
  ,
  read_generation_floor := pmax(
    baseline_read_floor,
    as.integer(ceiling(like_num / 0.08)),
    as.integer(ceiling(look_num / 0.05)),
    as.integer(ceiling(share_num / 0.10)),
    as.integer(ceiling(collect_num / 0.02)),
    as.integer(ceiling(total_reaction_num / 0.15))
  )
]

zero_read_idx <- which(is.na(dt$read_num) | dt$read_num <= 0)
if (length(zero_read_idx) > 0L) {
  target_read_median <- pmax(
    30,
    dt$base_read_median[zero_read_idx] *
      dt$account_read_multiplier[zero_read_idx] *
      dt$recency_read_multiplier[zero_read_idx] *
      0.70
  )
  simulated_reads <- as.integer(round(rlnorm(
    n = length(zero_read_idx),
    meanlog = log(target_read_median),
    sdlog = dt$cat_sdlog[zero_read_idx]
  )))
  simulated_reads <- pmax(simulated_reads, dt$read_generation_floor[zero_read_idx])
  simulated_reads <- as.integer(round(
    simulated_reads *
      (1 + 0.12 * dt$hot_score[zero_read_idx] +
         0.08 * as.integer(dt$is_service_helpful[zero_read_idx]) +
         0.10 * as.integer(dt$is_big_city[zero_read_idx]))
  ))
  read_cap <- ifelse(
    dt$hot_score[zero_read_idx] >= 3L & (dt$is_big_city[zero_read_idx] | dt$account_read_multiplier[zero_read_idx] >= 1.6),
    as.integer(round(stats::runif(length(zero_read_idx), min = 92000, max = 128000))),
    ifelse(
      dt$hot_score[zero_read_idx] >= 2L,
      as.integer(round(stats::runif(length(zero_read_idx), min = 42000, max = 76000))),
      as.integer(round(stats::runif(length(zero_read_idx), min = 26000, max = 46000)))
    )
  )
  over_cap <- simulated_reads > read_cap
  if (any(over_cap)) {
    simulated_reads[over_cap] <- as.integer(round(
      stats::runif(sum(over_cap), min = 0.82, max = 1.00) * read_cap[over_cap]
    ))
  }
  dt[zero_read_idx, read_num := simulated_reads]
}

low_generated_idx <- which(dt$read_generated_flag == 1L & dt$read_num <= 150L)
if (length(low_generated_idx) > 0L) {
  low_base <- dt$read_num[low_generated_idx]
  low_smoothed <- stats::runif(
    length(low_generated_idx),
    min = pmax(1, low_base * 0.72 - 3),
    max = low_base * 1.32 + 7
  )
  dt[low_generated_idx, read_num := as.integer(round(low_smoothed))]
}

message("Rebuilding interaction counts in the copied dataset.")

year_engagement_lookup <- c(
  "2015" = 0.86, "2016" = 0.90, "2017" = 0.95, "2018" = 1.00, "2019" = 1.04,
  "2020" = 1.08, "2021" = 1.11, "2022" = 1.15, "2023" = 1.18, "2024" = 1.21
)
dt[, year_engagement_multiplier := unname(year_engagement_lookup[as.character(year)])]
dt[is.na(year_engagement_multiplier), year_engagement_multiplier := 1]
dt[, account_engagement_multiplier := pmin(pmax(sqrt(account_read_multiplier), 0.80), 1.75)]
dt[
  ,
  recency_engagement_multiplier := ifelse(
    days_to_sample_end <= 180L,
    0.55 + 0.45 * (days_to_sample_end / 180),
    1.00
  )
]

family_share_base <- c(
  public_service = 0.00155,
  soft_propaganda = 0.00105,
  state_governance = 0.00078,
  hard_propaganda = 0.00062
)
family_collect_base <- c(
  public_service = 0.00034,
  soft_propaganda = 0.00022,
  state_governance = 0.00015,
  hard_propaganda = 0.00011
)
family_like_pre_base <- c(
  public_service = 0.040,
  soft_propaganda = 0.035,
  state_governance = 0.027,
  hard_propaganda = 0.022
)
family_like_post_base <- c(
  public_service = 0.036,
  soft_propaganda = 0.031,
  state_governance = 0.024,
  hard_propaganda = 0.019
)
family_look_mid_base <- c(
  public_service = 0.0020,
  soft_propaganda = 0.0015,
  state_governance = 0.0011,
  hard_propaganda = 0.0008
)
family_look_post_base <- c(
  public_service = 0.0016,
  soft_propaganda = 0.0012,
  state_governance = 0.0009,
  hard_propaganda = 0.0007
)

dt[, common_shock := exp(stats::rnorm(.N, mean = 0, sd = 0.22))]
dt[, approval_shock := exp(stats::rnorm(.N, mean = 0, sd = 0.18))]
dt[, circulation_shock := exp(stats::rnorm(.N, mean = 0, sd = 0.18))]
dt[, utility_shock := exp(stats::rnorm(.N, mean = 0, sd = 0.16))]
dt[
  ,
  silent_interaction := stats::rbinom(
    .N,
    size = 1,
    prob = pmin(
      0.004 +
        0.012 * as.integer(hot_score >= 2 & !is_collectible) +
        0.008 * as.integer(is_major_event & !is_service_helpful),
      0.035
    )
  )
]

dt[
  ,
  share_prob := pmin(
    unname(family_share_base[content_family]) *
      year_engagement_multiplier *
      account_engagement_multiplier *
      recency_engagement_multiplier *
      common_shock * circulation_shock *
      (1 + 0.28 * hot_score +
         0.30 * as.integer(is_service_helpful) +
         0.18 * as.integer(is_big_city) +
         0.22 * as.integer(is_major_event) +
         0.35 * as.integer(is_collectible)),
    0.035
  )
]
dt[silent_interaction == 1, share_prob := share_prob * 0.35]
dt[
  ,
  collect_prob := pmin(
    unname(family_collect_base[content_family]) *
      year_engagement_multiplier *
      account_engagement_multiplier *
      recency_engagement_multiplier *
      common_shock * utility_shock *
      (1 + 0.12 * hot_score +
         0.38 * as.integer(is_service_helpful) +
         0.95 * as.integer(is_collectible) +
         0.45 * as.integer(is_learning_material)),
    0.018
  )
]
dt[is_major_event & !is_collectible, collect_prob := collect_prob * 0.35]
dt[silent_interaction == 1, collect_prob := collect_prob * 0.20]

dt[, like_prob := 0]
dt[publish_date < cut_like_to_look, like_prob :=
  pmin(
    unname(family_like_pre_base[content_family]) *
      year_engagement_multiplier *
      account_engagement_multiplier *
      recency_engagement_multiplier *
      common_shock * approval_shock *
      (1 + 0.24 * hot_score +
         0.10 * as.integer(is_service_helpful) +
         0.10 * as.integer(is_big_city) +
         0.08 * as.integer(is_collectible)),
    0.25
  )
]
dt[publish_date >= cut_like_return, like_prob :=
  pmin(
    unname(family_like_post_base[content_family]) *
      year_engagement_multiplier *
      account_engagement_multiplier *
      recency_engagement_multiplier *
      common_shock * approval_shock *
      (1 + 0.22 * hot_score +
         0.10 * as.integer(is_service_helpful) +
         0.08 * as.integer(is_big_city) +
         0.08 * as.integer(is_collectible)),
    0.22
  )
]
dt[silent_interaction == 1, like_prob := like_prob * 0.35]

dt[, look_prob := 0]
dt[publish_date >= cut_like_to_look & publish_date < cut_like_return, look_prob :=
  pmin(
    unname(family_look_mid_base[content_family]) *
      year_engagement_multiplier *
      account_engagement_multiplier *
      recency_engagement_multiplier *
      common_shock * approval_shock *
      (1 + 0.14 * hot_score +
         0.06 * as.integer(is_service_helpful) +
         0.05 * as.integer(is_big_city) +
         0.05 * as.integer(is_collectible)),
    0.030
  )
]
dt[publish_date >= cut_like_return, look_prob :=
  pmin(
    unname(family_look_post_base[content_family]) *
      year_engagement_multiplier *
      account_engagement_multiplier *
      recency_engagement_multiplier *
      common_shock * approval_shock *
      (1 + 0.12 * hot_score +
         0.06 * as.integer(is_service_helpful) +
         0.05 * as.integer(is_big_city) +
         0.05 * as.integer(is_collectible)),
    0.024
  )
]
dt[silent_interaction == 1, look_prob := look_prob * 0.35]

dt[, share_num := stats::rbinom(.N, size = read_num, prob = share_prob)]
dt[, collect_num := stats::rbinom(.N, size = read_num, prob = collect_prob)]
dt[, like_num := stats::rbinom(.N, size = read_num, prob = like_prob)]
dt[, look_num := stats::rbinom(.N, size = read_num, prob = look_prob)]

dt[, public_signal_num := share_num + look_num]
dt[, total_reaction_num := like_num + share_num + look_num + collect_num]
dt[, any_engagement := total_reaction_num > 0 | read_num > 0]
dt[, all_metrics_zero := rowSums(.SD) == 0, .SDcols = c("read_num", "like_num", "share_num", "look_num", "collect_num")]

dt[, like_rate := safe_rate(like_num, read_num)]
dt[, share_rate := safe_rate(share_num, read_num)]
dt[, look_rate := safe_rate(look_num, read_num)]
dt[, collect_rate := safe_rate(collect_num, read_num)]
dt[, public_signal_rate := safe_rate(public_signal_num, read_num)]
dt[, total_reaction_rate := safe_rate(total_reaction_num, read_num)]

dt[, `:=`(
  cat_read_median = NULL,
  cat_sdlog = NULL,
  base_read_median = NULL,
  account_read_multiplier = NULL,
  days_to_sample_end = NULL,
  recency_read_multiplier = NULL,
  read_generation_floor = NULL,
  read_generated_flag = NULL,
  is_big_city = NULL,
  is_service_helpful = NULL,
  is_collectible = NULL,
  is_major_event = NULL,
  is_leader_news = NULL,
  is_learning_material = NULL,
  hot_score = NULL,
  year_engagement_multiplier = NULL,
  account_engagement_multiplier = NULL,
  recency_engagement_multiplier = NULL,
  common_shock = NULL,
  approval_shock = NULL,
  circulation_shock = NULL,
  utility_shock = NULL,
  baseline_read_floor = NULL,
  silent_interaction = NULL,
  share_prob = NULL,
  collect_prob = NULL,
  like_prob = NULL,
  look_prob = NULL
)]

message("Writing instructional-use dataset to: ", output_path)
saveRDS(dt, output_path)

message("Finished building the instructional-use dataset.")
