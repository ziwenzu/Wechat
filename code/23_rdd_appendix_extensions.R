bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]
dt[, days_from_2017 := as.integer(publish_date - as.Date("2017-05-18"))]
dt[, one_click_num := like_num + look_num]
dt[, any_one_click := as.numeric(one_click_num > 0)]
dt[, any_like := as.numeric(like_num > 0)]
dt[, any_look := as.numeric(look_num > 0)]
dt[, any_share := as.numeric(share_num > 0)]

max_window <- 30L
covid_pat <- "疫情|新冠|肺炎|防控|核酸|隔离|口罩|疫苗|确诊|无症状|健康码|行程码"
dt[, covid_keyword_flag := grepl(covid_pat, paste(title, keywords), perl = TRUE)]

# ── 2017 discovery reform as contrast case ───────────────────

art_2017 <- dt[abs(days_from_2017) <= max_window]

contrast_2017 <- data.table::rbindlist(list(
  safe_rdd(art_2017$read_num, art_2017$days_from_2017, art_2017$account_id,
           label = "Reads"),
  safe_rdd(art_2017$one_click_rate, art_2017$days_from_2017, art_2017$account_id,
           label = "One-click rate"),
  safe_rdd(art_2017$share_rate, art_2017$days_from_2017, art_2017$account_id,
           label = "Share rate")
))

fmt_rdd_tex(
  contrast_2017,
  caption = "Contrast test at the May 2017 discovery reform. This change broadened content discovery without directly rebundling article-footer approval buttons.",
  label = "tab:rdd-2017-contrast",
  filepath = file.path(paths$tables, "appendix_rdd_2017_contrast.tex")
)

# ── Account-day aggregation robustness ───────────────────────

aggregate_account_day <- function(sub, day_var) {
  sub[, .(
    one_click_rate = mean(one_click_rate, na.rm = TRUE),
    like_rate = mean(like_rate, na.rm = TRUE),
    look_rate = mean(look_rate, na.rm = TRUE),
    share_rate = mean(share_rate, na.rm = TRUE),
    n_posts = .N
  ), by = .(account_id, x = get(day_var))]
}

ad_2018 <- aggregate_account_day(dt[abs(days_from_2018) <= max_window], "days_from_2018")
ad_2020 <- aggregate_account_day(dt[abs(days_from_2020) <= max_window], "days_from_2020")

account_day_rows <- data.table::rbindlist(list(
  safe_rdd(ad_2018$one_click_rate, ad_2018$x, ad_2018$account_id,
           label = "Account-day (2018): One-click"),
  safe_rdd(ad_2020$one_click_rate, ad_2020$x, ad_2020$account_id,
           label = "Account-day (2020): One-click"),
  safe_rdd(ad_2020$like_rate, ad_2020$x, ad_2020$account_id,
           label = "Account-day (2020): Like"),
  safe_rdd(ad_2020$look_rate, ad_2020$x, ad_2020$account_id,
           label = "Account-day (2020): Zaikan"),
  safe_rdd(ad_2020$share_rate, ad_2020$x, ad_2020$account_id,
           label = "Account-day (2020): Share")
))

fmt_rdd_tex(
  account_day_rows,
  caption = "Account-day aggregated RDD estimates. Each account-day receives equal weight, reducing the influence of high-volume posting accounts.",
  label = "tab:rdd-account-day",
  filepath = file.path(paths$tables, "appendix_rdd_account_day.tex")
)

# ── Alternative samples for 2020 (COVID / geography) ───────

art_2020 <- dt[abs(days_from_2020) <= max_window]
art_2020_no_covid <- art_2020[
  category != "应急管理与风险沟通" & !covid_keyword_flag
]
art_2020_no_hubei <- art_2020[
  province != "湖北省" & city != "武汉市"
]

alt_2020_rows <- data.table::rbindlist(list(
  safe_rdd(art_2020_no_covid$one_click_rate, art_2020_no_covid$days_from_2020,
           art_2020_no_covid$account_id,
           label = "No COVID/emergency: One-click"),
  safe_rdd(art_2020_no_covid$like_rate, art_2020_no_covid$days_from_2020,
           art_2020_no_covid$account_id,
           label = "No COVID/emergency: Like"),
  safe_rdd(art_2020_no_covid$look_rate, art_2020_no_covid$days_from_2020,
           art_2020_no_covid$account_id,
           label = "No COVID/emergency: Zaikan"),
  safe_rdd(art_2020_no_hubei$one_click_rate, art_2020_no_hubei$days_from_2020,
           art_2020_no_hubei$account_id,
           label = "Exclude Hubei/Wuhan: One-click"),
  safe_rdd(art_2020_no_hubei$like_rate, art_2020_no_hubei$days_from_2020,
           art_2020_no_hubei$account_id,
           label = "Exclude Hubei/Wuhan: Like"),
  safe_rdd(art_2020_no_hubei$look_rate, art_2020_no_hubei$days_from_2020,
           art_2020_no_hubei$account_id,
           label = "Exclude Hubei/Wuhan: Zaikan")
))

fmt_rdd_tex(
  alt_2020_rows,
  caption = "Alternative-sample checks for the July 2020 cutoff. The first panel excludes emergency-category and COVID-linked posts; the second excludes Hubei and Wuhan accounts.",
  label = "tab:rdd-2020-alt-samples",
  filepath = file.path(paths$tables, "appendix_rdd_2020_alt_samples.tex")
)

# ── Alternative outcomes: extensive margins ──────────────────

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

extensive_rows <- data.table::rbindlist(list(
  safe_rdd(art_2018$any_one_click, art_2018$days_from_2018, art_2018$account_id,
           label = "2018: Any one-click"),
  safe_rdd(art_2018$any_share, art_2018$days_from_2018, art_2018$account_id,
           label = "2018: Any share"),
  safe_rdd(art_2020$any_one_click, art_2020$days_from_2020, art_2020$account_id,
           label = "2020: Any one-click"),
  safe_rdd(art_2020$any_like, art_2020$days_from_2020, art_2020$account_id,
           label = "2020: Any like"),
  safe_rdd(art_2020$any_look, art_2020$days_from_2020, art_2020$account_id,
           label = "2020: Any zaikan"),
  safe_rdd(art_2020$any_share, art_2020$days_from_2020, art_2020$account_id,
           label = "2020: Any share")
))

fmt_rdd_tex(
  extensive_rows,
  caption = "RDD estimates for extensive-margin engagement outcomes. Each outcome equals one when the post receives any positive count on that engagement channel.",
  label = "tab:rdd-extensive-margin",
  filepath = file.path(paths$tables, "appendix_rdd_extensive_margin.tex")
)

message("Saved appendix extension tables to ", paths$tables)
