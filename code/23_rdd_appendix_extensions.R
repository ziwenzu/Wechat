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
dt[, article_rank := seq_len(.N), by = .(account_id, publish_date)]
dt[, is_head := article_rank == 1L]
dt[, title_length := nchar(title)]

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

# ── Leave-one-province-out jackknife ─────────────────────────

province_jackknife <- function(sub, x_var, outcome_var, display_label) {
  full_fit <- safe_rdd(sub[[outcome_var]], sub[[x_var]], sub$account_id, label = "full")
  provinces <- sort(unique(sub$province))

  loo_fits <- data.table::rbindlist(lapply(provinces, function(prov) {
    keep <- sub$province != prov
    fit <- safe_rdd(sub[[outcome_var]][keep], sub[[x_var]][keep], sub$account_id[keep],
                    label = prov)
    if (!is.null(fit)) fit[, omitted_province := prov]
    fit
  }), fill = TRUE)

  loo_fits <- loo_fits[is.finite(tau_rb)]
  full_sign <- sign(full_fit$tau_rb)

  data.table::data.table(
    Specification = display_label,
    `Full-sample estimate` = full_fit$tau_rb,
    `Min LOO estimate` = min(loo_fits$tau_rb, na.rm = TRUE),
    `Max LOO estimate` = max(loo_fits$tau_rb, na.rm = TRUE),
    `Same-sign omissions` = sprintf("%d/%d",
                                    sum(sign(loo_fits$tau_rb) == full_sign, na.rm = TRUE),
                                    nrow(loo_fits)),
    `Significant omissions` = sprintf("%d/%d",
                                      sum(loo_fits$pvalue < 0.05, na.rm = TRUE),
                                      nrow(loo_fits))
  )
}

province_jackknife_rows <- data.table::rbindlist(list(
  province_jackknife(dt[abs(days_from_2018) <= max_window],
                     "days_from_2018", "one_click_rate", "2018: One-click"),
  province_jackknife(dt[abs(days_from_2020) <= max_window],
                     "days_from_2020", "one_click_rate", "2020: One-click"),
  province_jackknife(dt[abs(days_from_2020) <= max_window],
                     "days_from_2020", "like_rate", "2020: Like"),
  province_jackknife(dt[abs(days_from_2020) <= max_window],
                     "days_from_2020", "look_rate", "2020: Zaikan")
))

write_tex_table(
  province_jackknife_rows,
  path = file.path(paths$tables, "appendix_rdd_province_jackknife.tex"),
  caption = "Leave-one-province-out jackknife RDDs. Each row summarizes how the estimate changes when one provincial unit is omitted at a time.",
  label = "tab:rdd-province-jackknife",
  digits = c(
    `Full-sample estimate` = 4L,
    `Min LOO estimate` = 4L,
    `Max LOO estimate` = 4L
  ),
  align = "lrrrll"
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

# ── Discrete-cutoff placebo-cutoff inference ────────────────

cutoff_randomization <- function(sub, x_var, outcome_var, cutoff_label, display_label,
                                 placebo_grid = setdiff(-14:14, -2:2)) {
  y <- sub[[outcome_var]]
  x <- sub[[x_var]]
  cl <- sub$account_id

  actual <- safe_rdd(y, x, cl, label = "actual")
  if (is.null(actual)) return(NULL)

  placebo_vals <- vapply(placebo_grid, function(c0) {
    fit <- safe_rdd(y, x - c0, cl, label = paste0("placebo_", c0))
    if (is.null(fit)) return(NA_real_)
    fit$tau_rb
  }, numeric(1))
  placebo_vals <- placebo_vals[is.finite(placebo_vals)]

  data.table::data.table(
    Specification = paste0(cutoff_label, ": ", display_label),
    Estimate = actual$tau_rb,
    `Conventional p-value` = actual$pvalue,
    `Placebo-cutoff p-value` = mean(abs(placebo_vals) >= abs(actual$tau_rb)),
    `Placebo cutoffs` = length(placebo_vals)
  )
}

randomization_res <- data.table::rbindlist(list(
  cutoff_randomization(dt[abs(days_from_2018) <= max_window], "days_from_2018",
                       "one_click_rate", "2018", "One-click"),
  cutoff_randomization(dt[abs(days_from_2020) <= max_window], "days_from_2020",
                       "one_click_rate", "2020", "One-click"),
  cutoff_randomization(dt[abs(days_from_2020) <= max_window], "days_from_2020",
                       "like_rate", "2020", "Like"),
  cutoff_randomization(dt[abs(days_from_2020) <= max_window], "days_from_2020",
                       "look_rate", "2020", "Zaikan")
), fill = TRUE)

write_tex_table(
  randomization_res,
  path = file.path(paths$tables, "appendix_rdd_cutoff_randomization.tex"),
  caption = "Discrete-cutoff placebo-cutoff inference within a local neighborhood. The placebo-cutoff p-value reports the share of alternative cutoff days in a plus-or-minus 14-day neighborhood whose absolute RD estimate is at least as large as the observed estimate.",
  label = "tab:rdd-cutoff-randomization",
  digits = c(
    Estimate = 4L,
    `Conventional p-value` = 3L,
    `Placebo-cutoff p-value` = 3L,
    `Placebo cutoffs` = 0L
  ),
  align = "lrrrr"
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

message("Saved appendix extension tables to ", paths$tables)
