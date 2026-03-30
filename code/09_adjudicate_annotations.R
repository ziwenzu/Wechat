bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

paths <- project_paths()

annotation_path <- file.path(paths$data, "annotation_sample_2000_blind.rds")
clean_path <- file.path(paths$data, "wechat_posts_clean.rds")

ann <- data.table::as.data.table(readRDS(annotation_path))
clean <- data.table::as.data.table(readRDS(clean_path))

support_cols <- c(
  "id",
  "title",
  "province",
  "city",
  "public_account_name",
  "category_raw",
  "reason",
  "keywords",
  "category",
  "content_family"
)

support <- unique(clean[, ..support_cols], by = "id")
dt <- support[ann, on = "id"]

raw_prior_map <- c(
  "纪检监察与党风廉政" = "意识形态与宣传教育",
  "教育医疗养老" = "社会保障与公共福利",
  "公众服务信息" = "公共服务信息",
  "文化活动与城市形象" = "城市形象与文化活动",
  "宣传教育与意识形态" = "意识形态与宣传教育",
  "社会动员与社会参与" = "群众动员与社会参与",
  "人事任免与干部公示" = "时政与领导活动",
  "人事任免" = "时政与领导活动"
)

map_raw_prior <- function(x) {
  x <- as.character(x)
  out <- unname(raw_prior_map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

has_pat <- function(text, patterns) {
  grepl(paste0("(", paste(patterns, collapse = "|"), ")"), text, perl = TRUE)
}

decide_pair <- function(
  ra1,
  ra2,
  raw_prior,
  combined_text,
  leader_event,
  policy_formal,
  ideology_flag,
  emergency_flag,
  governance_flag,
  welfare_flag,
  service_flag,
  development_flag,
  culture_flag,
  mobilization_flag
) {
  if (identical(ra1, ra2)) {
    return(list(label = ra1, rule = "coder_agreement"))
  }

  pair_key <- paste(sort(c(ra1, ra2)), collapse = "||")
  candidates <- c(ra1, ra2)

  choose_raw_if_candidate <- function() {
    if (!is.na(raw_prior) && nzchar(raw_prior) && raw_prior %in% candidates) {
      return(raw_prior)
    }
    NA_character_
  }

  raw_choice <- choose_raw_if_candidate()

  if (pair_key == "时政与领导活动||经济与发展建设") {
    if (leader_event && !development_flag) return(list(label = "时政与领导活动", rule = "leader_over_development"))
    if (development_flag && !leader_event) return(list(label = "经济与发展建设", rule = "development_over_leader"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    if (leader_event) return(list(label = "时政与领导活动", rule = "leader_meeting_default"))
    return(list(label = "经济与发展建设", rule = "development_default"))
  }

  if (pair_key == "意识形态与宣传教育||时政与领导活动") {
    if (ideology_flag && !leader_event) return(list(label = "意识形态与宣传教育", rule = "ideology_over_leader"))
    if (leader_event && !ideology_flag) return(list(label = "时政与领导活动", rule = "leader_over_ideology"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    if (ideology_flag) return(list(label = "意识形态与宣传教育", rule = "ideology_default"))
    return(list(label = "时政与领导活动", rule = "leader_default"))
  }

  if (pair_key == "政策与政务公开||时政与领导活动") {
    if (policy_formal && !leader_event) return(list(label = "政策与政务公开", rule = "policy_release_over_leader"))
    if (leader_event && !policy_formal) return(list(label = "时政与领导活动", rule = "leader_event_over_policy"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    if (policy_formal) return(list(label = "政策与政务公开", rule = "policy_default"))
    return(list(label = "时政与领导活动", rule = "leader_default"))
  }

  if (pair_key == "公共服务信息||应急管理与风险沟通") {
    if (emergency_flag) return(list(label = "应急管理与风险沟通", rule = "emergency_over_service"))
    return(list(label = "公共服务信息", rule = "service_over_non_emergency"))
  }

  if (pair_key == "社会治理与执法通报||时政与领导活动") {
    if (governance_flag && !leader_event) return(list(label = "社会治理与执法通报", rule = "governance_over_leader"))
    if (leader_event) return(list(label = "时政与领导活动", rule = "leader_over_governance"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "社会治理与执法通报", rule = "governance_default"))
  }

  if (pair_key == "经济与发展建设||政策与政务公开") {
    if (policy_formal && !development_flag) return(list(label = "政策与政务公开", rule = "policy_over_development"))
    if (development_flag && !policy_formal) return(list(label = "经济与发展建设", rule = "development_over_policy"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    if (policy_formal) return(list(label = "政策与政务公开", rule = "policy_default"))
    return(list(label = "经济与发展建设", rule = "development_default"))
  }

  if (pair_key == "城市形象与文化活动||意识形态与宣传教育") {
    if (ideology_flag) return(list(label = "意识形态与宣传教育", rule = "ideology_over_city"))
    if (culture_flag) return(list(label = "城市形象与文化活动", rule = "city_over_non_ideology"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "城市形象与文化活动", rule = "city_default"))
  }

  if (pair_key == "公共服务信息||社会保障与公共福利") {
    if (welfare_flag) return(list(label = "社会保障与公共福利", rule = "welfare_over_service"))
    return(list(label = "公共服务信息", rule = "service_over_non_welfare"))
  }

  if (pair_key == "公共服务信息||社会治理与执法通报") {
    if (governance_flag && !service_flag) return(list(label = "社会治理与执法通报", rule = "governance_over_service"))
    if (service_flag) return(list(label = "公共服务信息", rule = "service_over_governance"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "公共服务信息", rule = "service_default"))
  }

  if (pair_key == "政策与政务公开||社会保障与公共福利") {
    if (welfare_flag && !policy_formal) return(list(label = "社会保障与公共福利", rule = "welfare_substance_over_policy"))
    if (policy_formal) return(list(label = "政策与政务公开", rule = "policy_release_over_welfare"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "社会保障与公共福利", rule = "welfare_default"))
  }

  if (pair_key == "意识形态与宣传教育||政策与政务公开") {
    if (ideology_flag) return(list(label = "意识形态与宣传教育", rule = "ideology_over_policy"))
    if (policy_formal) return(list(label = "政策与政务公开", rule = "policy_over_non_ideology"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "政策与政务公开", rule = "policy_default"))
  }

  if (pair_key == "经济与发展建设||社会保障与公共福利") {
    if (welfare_flag && !development_flag) return(list(label = "社会保障与公共福利", rule = "welfare_over_development"))
    if (development_flag) return(list(label = "经济与发展建设", rule = "development_over_welfare"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "经济与发展建设", rule = "development_default"))
  }

  if (pair_key == "公共服务信息||政策与政务公开") {
    if (policy_formal && !service_flag) return(list(label = "政策与政务公开", rule = "policy_over_service"))
    if (service_flag) return(list(label = "公共服务信息", rule = "service_over_policy"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "政策与政务公开", rule = "policy_default"))
  }

  if (pair_key == "城市形象与文化活动||经济与发展建设") {
    if (culture_flag && !development_flag) return(list(label = "城市形象与文化活动", rule = "city_over_development"))
    if (development_flag && !culture_flag) return(list(label = "经济与发展建设", rule = "development_over_city"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "城市形象与文化活动", rule = "city_default"))
  }

  if (pair_key == "应急管理与风险沟通||社会治理与执法通报") {
    if (emergency_flag && !governance_flag) return(list(label = "应急管理与风险沟通", rule = "emergency_over_governance"))
    if (governance_flag && !emergency_flag) return(list(label = "社会治理与执法通报", rule = "governance_over_emergency"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "应急管理与风险沟通", rule = "emergency_default"))
  }

  if (pair_key == "群众动员与社会参与||社会治理与执法通报") {
    if (mobilization_flag && !governance_flag) return(list(label = "群众动员与社会参与", rule = "mobilization_over_governance"))
    if (governance_flag) return(list(label = "社会治理与执法通报", rule = "governance_over_mobilization"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "社会治理与执法通报", rule = "governance_default"))
  }

  if (pair_key == "应急管理与风险沟通||时政与领导活动") {
    if (emergency_flag && !leader_event) return(list(label = "应急管理与风险沟通", rule = "emergency_over_leader"))
    if (leader_event) return(list(label = "时政与领导活动", rule = "leader_over_emergency"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "应急管理与风险沟通", rule = "emergency_default"))
  }

  if (pair_key == "社会保障与公共福利||时政与领导活动") {
    if (welfare_flag && !leader_event) return(list(label = "社会保障与公共福利", rule = "welfare_over_leader"))
    if (leader_event) return(list(label = "时政与领导活动", rule = "leader_over_welfare"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "社会保障与公共福利", rule = "welfare_default"))
  }

  if (pair_key == "群众动员与社会参与||时政与领导活动") {
    if (mobilization_flag && !leader_event) return(list(label = "群众动员与社会参与", rule = "mobilization_over_leader"))
    if (leader_event) return(list(label = "时政与领导活动", rule = "leader_over_mobilization"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "时政与领导活动", rule = "leader_default"))
  }

  if (pair_key == "经济与发展建设||意识形态与宣传教育") {
    if (ideology_flag && !development_flag) return(list(label = "意识形态与宣传教育", rule = "ideology_over_development"))
    if (development_flag && !ideology_flag) return(list(label = "经济与发展建设", rule = "development_over_ideology"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "经济与发展建设", rule = "development_default"))
  }

  if (pair_key == "城市形象与文化活动||应急管理与风险沟通") {
    if (emergency_flag) return(list(label = "应急管理与风险沟通", rule = "emergency_over_city"))
    if (culture_flag) return(list(label = "城市形象与文化活动", rule = "city_over_non_emergency"))
    if (!is.na(raw_choice)) return(list(label = raw_choice, rule = "raw_prior_break_tie"))
    return(list(label = "城市形象与文化活动", rule = "city_default"))
  }

  if (!is.na(raw_choice)) {
    return(list(label = raw_choice, rule = "raw_prior_break_tie"))
  }

  if (policy_formal) return(list(label = "政策与政务公开", rule = "fallback_policy"))
  if (leader_event) return(list(label = "时政与领导活动", rule = "fallback_leader"))
  if (ideology_flag) return(list(label = "意识形态与宣传教育", rule = "fallback_ideology"))
  if (emergency_flag) return(list(label = "应急管理与风险沟通", rule = "fallback_emergency"))
  if (welfare_flag) return(list(label = "社会保障与公共福利", rule = "fallback_welfare"))
  if (governance_flag) return(list(label = "社会治理与执法通报", rule = "fallback_governance"))
  if (service_flag) return(list(label = "公共服务信息", rule = "fallback_service"))
  if (mobilization_flag) return(list(label = "群众动员与社会参与", rule = "fallback_mobilization"))
  if (development_flag) return(list(label = "经济与发展建设", rule = "fallback_development"))
  if (culture_flag) return(list(label = "城市形象与文化活动", rule = "fallback_city"))

  list(label = ra2, rule = "fallback_ra2")
}

family_map <- stats::setNames(content_map$content_family, content_map$category)

dt[, llm_primary_label := category]
dt[, llm_family_label := content_family]
dt[, raw_prior_label := map_raw_prior(category_raw)]

dt[, adjudicated_primary_label := NA_character_]
dt[, adjudicated_family_label := NA_character_]
dt[, adjudication_rule := NA_character_]

leader_patterns <- c(
  "书记", "市长", "区长", "县长", "州长", "党组", "常委会", "常务会议", "办公会",
  "主持", "出席", "调研", "强调", "指出", "率队", "督导", "慰问", "检查", "会议"
)
policy_patterns <- c(
  "通知", "方案", "办法", "意见", "决定", "决议", "全文发布", "公布", "印发", "出台",
  "实施方案", "实施意见", "推荐名单", "名单", "政策", "措施", "规定", "公告"
)
ideology_patterns <- c(
  "学习贯彻", "精神", "评论", "英烈", "缅怀", "宣传", "悦读", "荐读", "党纪",
  "廉政", "腐败", "党建", "主题教育", "巡视", "巡察", "八项规定", "理论"
)
emergency_patterns <- c(
  "预警", "疫情", "防控", "防疫", "应急", "安全生产", "紧急", "风险", "核酸",
  "台风", "暴雨", "高温", "寒潮", "火灾", "地震", "酒驾", "未成年人", "吹哨人"
)
governance_patterns <- c(
  "治理", "整治", "执法", "通报", "巡查", "巡察", "信用体系", "反诈", "公安",
  "检查", "监管", "法治", "基层治理", "创建", "专项"
)
welfare_patterns <- c(
  "教育", "学校", "高考", "招聘", "岗位", "医疗", "医院", "医保", "养老", "补贴",
  "民生", "人才", "录取", "招生", "未成年人", "疾控"
)
service_patterns <- c(
  "提醒", "提示", "出行", "办理", "必读", "如何", "车主", "服务热线", "保障",
  "假期", "交通", "通知"
)
development_patterns <- c(
  "发展", "建设", "项目", "产业", "振兴", "经济", "品牌", "生态旅游", "高质量发展",
  "合作", "发射", "加工", "供应", "暖企"
)
culture_patterns <- c(
  "旅游", "文旅", "美丽", "黄河之美", "草原", "清明", "英烈", "文化", "美食", "风景", "节日"
)
mobilization_patterns <- c(
  "动员", "群众", "参与", "志愿", "倡议", "创建", "创文", "创卫"
)

for (i in seq_len(nrow(dt))) {
  ra1 <- dt$primary_label_ra1[i]
  ra2 <- dt$primary_label_ra2[i]

  if (is.na(ra1) || is.na(ra2) || !nzchar(trimws(ra1)) || !nzchar(trimws(ra2))) {
    next
  }

  combined_text <- paste(
    dt$title[i],
    ifelse(is.na(dt$reason[i]), "", dt$reason[i]),
    ifelse(is.na(dt$keywords[i]), "", dt$keywords[i]),
    sep = " | "
  )

  leader_event <- has_pat(combined_text, leader_patterns)
  policy_formal <- has_pat(combined_text, policy_patterns)
  ideology_flag <- has_pat(combined_text, ideology_patterns)
  emergency_flag <- has_pat(combined_text, emergency_patterns)
  governance_flag <- has_pat(combined_text, governance_patterns)
  welfare_flag <- has_pat(combined_text, welfare_patterns)
  service_flag <- has_pat(combined_text, service_patterns)
  development_flag <- has_pat(combined_text, development_patterns)
  culture_flag <- has_pat(combined_text, culture_patterns)
  mobilization_flag <- has_pat(combined_text, mobilization_patterns)

  decision <- decide_pair(
    ra1 = ra1,
    ra2 = ra2,
    raw_prior = dt$raw_prior_label[i],
    combined_text = combined_text,
    leader_event = leader_event,
    policy_formal = policy_formal,
    ideology_flag = ideology_flag,
    emergency_flag = emergency_flag,
    governance_flag = governance_flag,
    welfare_flag = welfare_flag,
    service_flag = service_flag,
    development_flag = development_flag,
    culture_flag = culture_flag,
    mobilization_flag = mobilization_flag
  )

  dt$adjudicated_primary_label[i] <- decision$label
  dt$adjudication_rule[i] <- decision$rule
  dt$adjudicated_family_label[i] <- unname(family_map[decision$label])
}

output <- data.table::copy(ann)
drop_if_exists <- intersect(
  c(
    "i.adjudicated_primary_label",
    "i.adjudicated_family_label",
    "llm_primary_label",
    "llm_family_label",
    "adjudication_rule"
  ),
  names(output)
)
if (length(drop_if_exists) > 0) {
  output[, (drop_if_exists) := NULL]
}

output[, adjudicated_primary_label := dt$adjudicated_primary_label]
output[, adjudicated_family_label := dt$adjudicated_family_label]
output[, llm_primary_label := dt$llm_primary_label]
output[, llm_family_label := dt$llm_family_label]
output[, adjudication_rule := dt$adjudication_rule]

saveRDS(output, annotation_path)

message("Saved adjudicated annotation file to: ", annotation_path)
message("Agreed cases: ", dt[primary_label_ra1 == primary_label_ra2, .N])
message("Disagreed cases adjudicated: ", dt[primary_label_ra1 != primary_label_ra2, .N])
message("Adjudication rules used:")
print(dt[, .N, by = adjudication_rule][order(-N)])
