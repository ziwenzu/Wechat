get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]])))
  }

  normalizePath(".")
}

get_script_dir <- function() {
  script_path <- get_script_path()
  if (dir.exists(script_path)) {
    return(script_path)
  }
  dirname(script_path)
}

project_paths <- function() {
  script_dir <- get_script_dir()
  root_dir <- normalizePath(file.path(script_dir, ".."))

  articles_candidates <- c(
    file.path(root_dir, "data", "articles.csv"),
    file.path(root_dir, "analysis", "data", "articles.csv")
  )
  articles_csv <- articles_candidates[file.exists(articles_candidates)][1]

  if (is.na(articles_csv)) {
    articles_csv <- file.path(root_dir, "data", "articles.csv")
  }

  list(
    root = root_dir,
    code = file.path(root_dir, "code"),
    data = file.path(root_dir, "data"),
    figures = file.path(root_dir, "figures"),
    tables = file.path(root_dir, "tables"),
    articles_csv = articles_csv
  )
}

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  parsed <- list()

  for (arg in args) {
    if (!grepl("^--", arg)) {
      next
    }

    clean_arg <- sub("^--", "", arg)

    if (grepl("=", clean_arg, fixed = TRUE)) {
      pieces <- strsplit(clean_arg, "=", fixed = TRUE)[[1]]
      parsed[[pieces[[1]]]] <- paste(pieces[-1], collapse = "=")
    } else {
      parsed[[clean_arg]] <- TRUE
    }
  }

  parsed
}

ensure_packages <- function(packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      paste(
        "Missing R packages:",
        paste(missing_packages, collapse = ", "),
        "\nInstall them before running this script."
      ),
      call. = FALSE
    )
  }
}

coalesce_zero <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  x
}

safe_rate <- function(numerator, denominator) {
  out <- numerator / denominator
  out[is.na(denominator) | denominator <= 0] <- NA_real_
  out
}

weighted_mean_or_na <- function(x, w) {
  keep <- !(is.na(x) | is.na(w))

  if (!any(keep) || sum(w[keep]) <= 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[keep], w[keep])
}

latex_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
  x
}

format_numeric_for_latex <- function(x, digits = 3L) {
  out <- character(length(x))
  out[is.na(x)] <- ""

  non_missing <- !is.na(x)
  integer_like <- non_missing & abs(x - round(x)) < 1e-9 & abs(x) >= 1
  small_value <- non_missing & !integer_like & x != 0 & abs(x) < 10^(-digits)
  regular_numeric <- non_missing & !integer_like & !small_value

  out[integer_like] <- format(
    round(x[integer_like]),
    trim = TRUE,
    scientific = FALSE,
    big.mark = ","
  )
  out[small_value] <- formatC(x[small_value], format = "e", digits = digits)
  out[regular_numeric] <- formatC(
    x[regular_numeric],
    format = "f",
    digits = digits,
    big.mark = ","
  )

  out
}

format_column_for_latex <- function(x, digits = 3L) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }

  if (is.numeric(x)) {
    return(format_numeric_for_latex(x, digits = digits))
  }

  if (is.logical(x)) {
    out <- ifelse(is.na(x), "", ifelse(x, "TRUE", "FALSE"))
    return(out)
  }

  as.character(x)
}

resolve_digits <- function(digits, column_name, default_digits = 3L) {
  if (length(digits) == 1L && is.null(names(digits))) {
    return(as.integer(digits[[1]]))
  }

  if (!is.null(names(digits)) && column_name %in% names(digits)) {
    return(as.integer(digits[[column_name]]))
  }

  as.integer(default_digits)
}

write_tex_table <- function(
  data,
  path,
  caption = NULL,
  label = NULL,
  digits = 3L,
  default_digits = 3L,
  align = NULL
) {
  table_df <- as.data.frame(data, stringsAsFactors = FALSE)

  if (ncol(table_df) == 0L) {
    stop("Cannot write a LaTeX table with zero columns.", call. = FALSE)
  }

  formatted_df <- table_df

  for (col_name in names(table_df)) {
    col_digits <- resolve_digits(
      digits = digits,
      column_name = col_name,
      default_digits = default_digits
    )
    formatted_df[[col_name]] <- latex_escape(
      format_column_for_latex(table_df[[col_name]], digits = col_digits)
    )
  }

  header <- latex_escape(names(formatted_df))
  body_lines <- apply(formatted_df, 1, function(row) {
    paste0(paste(row, collapse = " & "), " \\\\")
  })

  if (is.null(align)) {
    column_align <- ifelse(vapply(table_df, is.numeric, logical(1)), "r", "l")
    align <- paste(column_align, collapse = "")
  }

  lines <- c(
    "% Auto-generated table.",
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\begingroup",
    "\\footnotesize",
    "\\renewcommand{\\arraystretch}{1.05}",
    "\\setlength{\\tabcolsep}{8pt}"
  )

  if (!is.null(caption)) {
    lines <- c(lines, paste0("\\caption{", latex_escape(caption), "}"))
  }

  if (!is.null(label)) {
    lines <- c(lines, paste0("\\label{", label, "}"))
  }

  lines <- c(
    lines,
    "\\makebox[\\textwidth][c]{\\scalebox{0.8}{%",
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste0(paste(header, collapse = " & "), " \\\\"),
    "\\hline",
    body_lines,
    "\\hline",
    "\\end{tabular}",
    "}}",
    "\\endgroup",
    "\\end{table}"
  )

  writeLines(lines, con = path, useBytes = TRUE)
}

content_map <- data.frame(
  category = c(
    "意识形态与宣传教育",
    "时政与领导活动",
    "公共服务信息",
    "社会保障与公共福利",
    "应急管理与风险沟通",
    "政策与政务公开",
    "社会治理与执法通报",
    "群众动员与社会参与",
    "经济与发展建设",
    "城市形象与文化活动"
  ),
  content_group = c(
    "propaganda_hard",
    "propaganda_hard",
    "service",
    "service",
    "service",
    "state_governance",
    "state_governance",
    "state_governance",
    "propaganda_soft",
    "propaganda_soft"
  ),
  content_family = c(
    "hard_propaganda",
    "hard_propaganda",
    "public_service",
    "public_service",
    "public_service",
    "state_governance",
    "state_governance",
    "state_governance",
    "soft_propaganda",
    "soft_propaganda"
  ),
  stringsAsFactors = FALSE
)

category_normalization_map <- data.frame(
  raw_label = c(
    "纪检监察与党风廉政",
    "公众服务信息",
    "文化活动与城市形象",
    "文化形象与城市活动",
    "政治与政务公开",
    "宣传教育与意识形态",
    "社会动员与社会参与",
    "纪委监察与党风廉政",
    "纪律监察与党风廉政",
    " policy and government disclosure",
    "policy and government disclosure",
    "Policy and Government Disclosure",
    "POLICY AND GOVERNMENT DISCLOSURE",
    "Economic and Development Construction",
    " Economic and Development Construction",
    "城市形象与文化活动",
    " 应急管理与风险沟通",
    " 时政与领导活动",
    "意 识形态与宣传教育",
    "意識形态与宣传教育",
    "意识形態与宣传教育",
    "時政與領導活動",
    "政策与政務公開",
    "政策与政务公開",
    "社会治理与执法通報",
    "群眾動員與社會參與",
    "應急管理与风险沟通"
  ),
  canonical_label = c(
    "意识形态与宣传教育",
    "公共服务信息",
    "城市形象与文化活动",
    "城市形象与文化活动",
    "政策与政务公开",
    "意识形态与宣传教育",
    "群众动员与社会参与",
    "意识形态与宣传教育",
    "意识形态与宣传教育",
    "政策与政务公开",
    "政策与政务公开",
    "政策与政务公开",
    "政策与政务公开",
    "经济与发展建设",
    "经济与发展建设",
    "城市形象与文化活动",
    "应急管理与风险沟通",
    "时政与领导活动",
    "意识形态与宣传教育",
    "意识形态与宣传教育",
    "意识形态与宣传教育",
    "时政与领导活动",
    "政策与政务公开",
    "政策与政务公开",
    "社会治理与执法通报",
    "群众动员与社会参与",
    "应急管理与风险沟通"
  ),
  stringsAsFactors = FALSE
)

normalize_text_for_matching <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("[\r\n\t]", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  tolower(x)
}

contains_any_pattern <- function(text, patterns) {
  grepl(
    paste0("(", paste(patterns, collapse = "|"), ")"),
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
}

assign_when <- function(out, unresolved, condition, value) {
  idx <- unresolved & condition
  out[idx] <- value
  out
}

normalize_category <- function(category, reason = "", keywords = "", title = "") {
  raw_category <- as.character(category)
  raw_category[is.na(raw_category)] <- ""
  reason <- as.character(reason)
  reason[is.na(reason)] <- ""
  keywords <- as.character(keywords)
  keywords[is.na(keywords)] <- ""
  title <- as.character(title)
  title[is.na(title)] <- ""

  exact_lookup <- stats::setNames(
    category_normalization_map$canonical_label,
    trimws(category_normalization_map$raw_label)
  )

  out <- trimws(raw_category)
  exact_match <- unname(exact_lookup[out])
  out[!is.na(exact_match)] <- exact_match[!is.na(exact_match)]

  canonical_categories <- content_map$category
  combined_text <- normalize_text_for_matching(
    paste(raw_category, reason, keywords, title, sep = " | ")
  )

  unresolved <- !(out %in% canonical_categories)

  leadership_patterns <- c(
    "时政",
    "领导活动",
    "领导调研",
    "领导视察",
    "领导讲话",
    "领导动态",
    "政务动态",
    "会议与领导活动",
    "与领导活动",
    "领导活动与",
    "人事任免",
    "干部公示",
    "干部选拔",
    "揭牌",
    "签约",
    "开工",
    "授牌",
    "典礼"
  )
  emergency_patterns <- c(
    "应急",
    "风险沟通",
    "疫情",
    "防控",
    "灾害",
    "预警",
    "安全生产",
    "辟谣",
    "火灾",
    "地震",
    "洪水",
    "防汛",
    "台风",
    "气象",
    "天气",
    "事故",
    "风险",
    "防灾",
    "公共安全",
    "安全管理"
  )
  governance_patterns <- c(
    "社会治理",
    "执法",
    "司法",
    "法律",
    "法治",
    "审判",
    "公安",
    "反诈",
    "市场监管",
    "监管",
    "整治",
    "处罚",
    "垃圾分类",
    "维权",
    "禁毒",
    "城管",
    "督察",
    "犯罪",
    "扫黑"
  )
  policy_patterns <- c(
    "政策",
    "政务公开",
    "公示",
    "政府公告",
    "财政预算",
    "统计数据",
    "工作汇报",
    "年报",
    "政策解读",
    "预算",
    "数据",
    "公开程序",
    "government disclosure"
  )
  welfare_patterns <- c(
    "社会保障",
    "公共福利",
    "教育医疗养老",
    "教育",
    "医疗",
    "卫生",
    "健康",
    "养老",
    "就业",
    "人才",
    "招聘",
    "人事",
    "医保",
    "低保",
    "救助",
    "住房",
    "报销",
    "落户",
    "补贴",
    "教师",
    "学校",
    "招生",
    "考试"
  )
  public_service_patterns <- c(
    "公共服务",
    "服务信息",
    "服务指南",
    "办事",
    "办理",
    "指南",
    "提醒",
    "贴士",
    "报名",
    "缴费",
    "交通管理信息",
    "生活小贴士",
    "生活指南",
    "生活指导",
    "公告与通知",
    "金融服务信息",
    "街道办事处公告"
  )
  mobilization_patterns <- c(
    "社会参与",
    "群众动员",
    "志愿",
    "投票",
    "征集",
    "问卷",
    "倡议",
    "文明实践",
    "互动",
    "打卡",
    "随手拍",
    "公众参与",
    "参与活动",
    "网络互动",
    "动员"
  )
  hard_propaganda_patterns <- c(
    "意识形态",
    "宣传教育",
    "宣传与教育",
    "理论学习",
    "党风廉政",
    "纪检",
    "纪委",
    "纪律监察",
    "廉政",
    "党纪",
    "巡视",
    "巡察",
    "党务",
    "党的建设",
    "党员",
    "爱国主义",
    "红色",
    "思想",
    "榜样",
    "典型",
    "移风易俗",
    "文明创建",
    "精神力量"
  )
  culture_patterns <- c(
    "城市形象",
    "文化活动",
    "地方故事",
    "文旅",
    "旅游",
    "展会",
    "赛事",
    "文创",
    "节庆",
    "节日",
    "历史文化",
    "文化遗产",
    "人物专访",
    "专访",
    "纪念活动",
    "城市品牌",
    "风采",
    "魅力",
    "体育活动",
    "体育赛事",
    "文化"
  )
  development_patterns <- c(
    "经济",
    "发展建设",
    "项目",
    "招商",
    "产业",
    "科技创新",
    "乡村振兴",
    "基础设施",
    "农村发展",
    "农业",
    "营商环境",
    "投资",
    "可持续发展",
    "创新",
    "建设",
    "生态建设",
    "生态保护",
    "环境保护",
    "生态环境"
  )
  generic_missing_patterns <- c(
    "内容缺失",
    "内容不完整",
    "无法明确分类",
    "无法判断",
    "无法归类",
    "未提供内容",
    "不具备明确的主题",
    "无法进行分类"
  )

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, leadership_patterns),
    "时政与领导活动"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, emergency_patterns),
    "应急管理与风险沟通"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, governance_patterns),
    "社会治理与执法通报"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, policy_patterns),
    "政策与政务公开"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, hard_propaganda_patterns),
    "意识形态与宣传教育"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, welfare_patterns),
    "社会保障与公共福利"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, public_service_patterns),
    "公共服务信息"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, mobilization_patterns),
    "群众动员与社会参与"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, culture_patterns),
    "城市形象与文化活动"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, development_patterns),
    "经济与发展建设"
  )
  unresolved <- !(out %in% canonical_categories)

  out <- assign_when(
    out,
    unresolved,
    contains_any_pattern(combined_text, generic_missing_patterns),
    "公共服务信息"
  )
  unresolved <- !(out %in% canonical_categories)

  out[unresolved] <- "公共服务信息"
  out
}

classify_content_group <- function(category) {
  lookup <- stats::setNames(content_map$content_group, content_map$category)
  out <- unname(lookup[category])
  out[is.na(out)] <- "uncoded"
  out
}

classify_content_family <- function(category) {
  lookup <- stats::setNames(content_map$content_family, content_map$category)
  out <- unname(lookup[category])
  out[is.na(out)] <- "other"
  out
}
