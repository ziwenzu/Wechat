bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "fixest", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]

cutoff_2018 <- as.Date("2018-12-21")
cutoff_2020 <- as.Date("2020-07-01")

# ==============================================================================
# Panel construction: account × month
# ==============================================================================

message("=== Building account-month panel ===")

dt[, year_month := as.Date(format(publish_date, "%Y-%m-01"))]

is_treated_family <- function(fam) fam %in% c("hard_propaganda", "soft_propaganda")

panel <- dt[, .(
  one_click_rate = mean(one_click_rate, na.rm = TRUE),
  like_rate      = mean(like_rate, na.rm = TRUE),
  look_rate      = mean(look_rate, na.rm = TRUE),
  share_rate     = mean(share_rate, na.rm = TRUE),
  read_num       = mean(read_num, na.rm = TRUE),
  n_articles     = .N
), by = .(account_id, year_month, content_family)]

panel[, treated := as.integer(is_treated_family(content_family))]

panel[, post_2018 := as.integer(year_month >= cutoff_2018)]
panel[, post_2020 := as.integer(year_month >= cutoff_2020)]

panel[, regime := "Pre (before Dec 2018)"]
panel[year_month >= cutoff_2018 & year_month < cutoff_2020,
      regime := "Middle (Dec 2018 -- Jun 2020)"]
panel[year_month >= cutoff_2020, regime := "Post (Jul 2020 --)"]

panel[, ym_num := as.integer(year_month)]
panel[, family_label := factor(content_family,
  levels = c("public_service", "soft_propaganda",
             "state_governance", "hard_propaganda"),
  labels = c("Public Service", "Soft Propaganda",
             "State Governance", "Hard Propaganda")
)]

# ==============================================================================
# DiD: Propaganda (treated) vs Service (control) around each cutoff
# ==============================================================================

message("=== DiD: propaganda vs service ===")

# --- Short window DiD around 2018 cutoff (±6 months) ---
panel_2018 <- panel[
  year_month >= (cutoff_2018 - 180) & year_month <= (cutoff_2018 + 180)
]

did_2018 <- fixest::feols(
  one_click_rate ~ treated:post_2018 | account_id + year_month,
  data = panel_2018[content_family %in% c("hard_propaganda", "soft_propaganda",
                                           "public_service")],
  weights = ~n_articles,
  cluster = ~account_id
)

# --- Short window DiD around 2020 cutoff (±6 months) ---
panel_2020 <- panel[
  year_month >= (cutoff_2020 - 180) & year_month <= (cutoff_2020 + 180)
]

did_2020_oneclk <- fixest::feols(
  one_click_rate ~ treated:post_2020 | account_id + year_month,
  data = panel_2020[content_family %in% c("hard_propaganda", "soft_propaganda",
                                           "public_service")],
  weights = ~n_articles,
  cluster = ~account_id
)

did_2020_like <- fixest::feols(
  like_rate ~ treated:post_2020 | account_id + year_month,
  data = panel_2020[content_family %in% c("hard_propaganda", "soft_propaganda",
                                           "public_service")],
  weights = ~n_articles,
  cluster = ~account_id
)

did_2020_zaikan <- fixest::feols(
  look_rate ~ treated:post_2020 | account_id + year_month,
  data = panel_2020[content_family %in% c("hard_propaganda", "soft_propaganda",
                                           "public_service")],
  weights = ~n_articles,
  cluster = ~account_id
)

message("  2018 DiD (one-click): ", sprintf("%.5f (%.5f)",
  stats::coef(did_2018)[[1]], sqrt(diag(stats::vcov(did_2018)))[[1]]))
message("  2020 DiD (one-click): ", sprintf("%.5f (%.5f)",
  stats::coef(did_2020_oneclk)[[1]], sqrt(diag(stats::vcov(did_2020_oneclk)))[[1]]))

# ==============================================================================
# Long-panel DiD: full timeline with both treatments
# ==============================================================================

message("=== Long-panel DiD (full timeline) ===")

panel_full <- panel[content_family %in% c("hard_propaganda", "soft_propaganda",
                                           "public_service")]

did_long <- fixest::feols(
  one_click_rate ~ treated:post_2018 + treated:post_2020 |
    account_id + year_month,
  data = panel_full,
  weights = ~n_articles,
  cluster = ~account_id
)

did_long_like <- fixest::feols(
  like_rate ~ treated:post_2018 + treated:post_2020 |
    account_id + year_month,
  data = panel_full,
  weights = ~n_articles,
  cluster = ~account_id
)

did_long_zaikan <- fixest::feols(
  look_rate ~ treated:post_2018 + treated:post_2020 |
    account_id + year_month,
  data = panel_full,
  weights = ~n_articles,
  cluster = ~account_id
)

# --- Format DiD results ---
extract_did <- function(fit, label) {
  ct <- fixest::coeftable(fit)
  data.table::data.table(
    Specification = label,
    Coefficient = rownames(ct),
    Estimate = ct[, "Estimate"],
    SE = ct[, "Std. Error"],
    `p-value` = ct[, "Pr(>|t|)"],
    N = stats::nobs(fit)
  )
}

did_table <- data.table::rbindlist(list(
  extract_did(did_2018, "Short-window 2018: One-click"),
  extract_did(did_2020_oneclk, "Short-window 2020: One-click"),
  extract_did(did_2020_like, "Short-window 2020: Like"),
  extract_did(did_2020_zaikan, "Short-window 2020: Zaikan"),
  extract_did(did_long, "Long panel: One-click"),
  extract_did(did_long_like, "Long panel: Like"),
  extract_did(did_long_zaikan, "Long panel: Zaikan")
))

write_tex_table(
  did_table,
  path = file.path(paths$tables, "appendix_did_propaganda_vs_service.tex"),
  caption = paste(
    "Difference-in-differences: propaganda (treated) vs. public-service (control)",
    "content families. Short-window specifications use \\pm 6 months around each",
    "cutoff. Long-panel specifications span the full data range with both",
    "post-2018 and post-2020 treatment indicators. All models include account",
    "and year-month fixed effects, article-count weights, and account-clustered SEs."
  ),
  label = "tab:did-propaganda-service",
  digits = c(Estimate = 5L, SE = 5L, `p-value` = 3L, N = 0L),
  align = "llrrrr"
)

# ==============================================================================
# Event study: leads and lags around each cutoff
# ==============================================================================

message("=== Event study: leads and lags ===")

make_event_time <- function(sub, cutoff_date, n_periods = 6L) {
  sub <- data.table::copy(sub)
  sub[, rel_month := as.integer(
    12 * (as.integer(format(year_month, "%Y")) - as.integer(format(cutoff_date, "%Y"))) +
    as.integer(format(year_month, "%m")) - as.integer(format(cutoff_date, "%m"))
  )]
  sub[rel_month < -n_periods, rel_month := -n_periods]
  sub[rel_month > n_periods, rel_month := n_periods]
  sub[, rel_month_f := stats::relevel(factor(rel_month), ref = as.character(-1L))]
  sub
}

# 2020 event study
es_2020 <- make_event_time(
  panel_full[year_month >= (cutoff_2020 - 210) & year_month <= (cutoff_2020 + 210)],
  cutoff_2020
)

es_fit_2020 <- fixest::feols(
  one_click_rate ~ i(rel_month, treated, ref = -1) | account_id + year_month,
  data = es_2020,
  weights = ~n_articles,
  cluster = ~account_id
)

es_ct <- fixest::coeftable(es_fit_2020)
es_coefs <- data.table::data.table(
  term = rownames(es_ct),
  estimate = es_ct[, "Estimate"],
  std.error = es_ct[, "Std. Error"]
)
es_coefs[, rel_month := as.integer(sub(".*::", "", term))]
es_coefs <- es_coefs[!is.na(rel_month)]
es_coefs[, `:=`(conf.low = estimate - 1.96 * std.error,
                conf.high = estimate + 1.96 * std.error,
                ref_row = FALSE)]
ref_row <- data.table::data.table(
  rel_month = -1L, estimate = 0, conf.low = 0, conf.high = 0, ref_row = TRUE
)
es_coefs <- data.table::rbindlist(list(es_coefs, ref_row), fill = TRUE)
data.table::setorder(es_coefs, rel_month)

p_es <- ggplot2::ggplot(es_coefs, ggplot2::aes(x = rel_month, y = estimate)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = conf.low, ymax = conf.high),
    fill = "grey75", alpha = 0.5
  ) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(
    ggplot2::aes(shape = ref_row),
    size = 2.5
  ) +
  ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", linewidth = 0.5) +
  ggplot2::scale_x_continuous(breaks = seq(-6, 6, by = 1)) +
  ggplot2::labs(
    x = "Months relative to July 2020 reform",
    y = "DiD coefficient (propaganda vs. service)"
  ) +
  rd_theme

save_figure(
  path = file.path(paths$figures, "appendix_event_study_did_2020.pdf"),
  plot = p_es,
  width = 8,
  height = 5,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)

# 2018 event study
es_2018 <- make_event_time(
  panel_full[year_month >= (cutoff_2018 - 210) & year_month <= (cutoff_2018 + 210)],
  cutoff_2018
)

es_fit_2018 <- fixest::feols(
  one_click_rate ~ i(rel_month, treated, ref = -1) | account_id + year_month,
  data = es_2018,
  weights = ~n_articles,
  cluster = ~account_id
)

es_ct_18 <- fixest::coeftable(es_fit_2018)
es_coefs_18 <- data.table::data.table(
  term = rownames(es_ct_18),
  estimate = es_ct_18[, "Estimate"],
  std.error = es_ct_18[, "Std. Error"]
)
es_coefs_18[, rel_month := as.integer(sub(".*::", "", term))]
es_coefs_18 <- es_coefs_18[!is.na(rel_month)]
es_coefs_18[, `:=`(conf.low = estimate - 1.96 * std.error,
                    conf.high = estimate + 1.96 * std.error,
                    ref_row = FALSE)]
ref_row_18 <- data.table::data.table(
  rel_month = -1L, estimate = 0, conf.low = 0, conf.high = 0, ref_row = TRUE
)
es_coefs_18 <- data.table::rbindlist(list(es_coefs_18, ref_row_18), fill = TRUE)
data.table::setorder(es_coefs_18, rel_month)

p_es_18 <- ggplot2::ggplot(es_coefs_18, ggplot2::aes(x = rel_month, y = estimate)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = conf.low, ymax = conf.high),
    fill = "grey75", alpha = 0.5
  ) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(
    ggplot2::aes(shape = ref_row),
    size = 2.5
  ) +
  ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", linewidth = 0.5) +
  ggplot2::scale_x_continuous(breaks = seq(-6, 6, by = 1)) +
  ggplot2::labs(
    x = "Months relative to December 2018 reform",
    y = "DiD coefficient (propaganda vs. service)"
  ) +
  rd_theme

save_figure(
  path = file.path(paths$figures, "appendix_event_study_did_2018.pdf"),
  plot = p_es_18,
  width = 8,
  height = 5,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)

# ==============================================================================
# Long-run trend plot: propaganda vs service means over time
# ==============================================================================

message("=== Long-run trend plot ===")

monthly_means <- dt[
  content_family %in% c("hard_propaganda", "soft_propaganda", "public_service"),
  .(one_click_rate = mean(one_click_rate, na.rm = TRUE),
    like_rate      = mean(like_rate, na.rm = TRUE),
    look_rate      = mean(look_rate, na.rm = TRUE)),
  by = .(year_month, group = ifelse(
    content_family %in% c("hard_propaganda", "soft_propaganda"),
    "Propaganda", "Public Service"
  ))
]

p_trend <- ggplot2::ggplot(monthly_means,
  ggplot2::aes(x = year_month, y = one_click_rate, color = group, linetype = group)) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_vline(xintercept = as.numeric(cutoff_2018),
    linetype = "dashed", color = "grey40", linewidth = 0.5) +
  ggplot2::geom_vline(xintercept = as.numeric(cutoff_2020),
    linetype = "dashed", color = "grey40", linewidth = 0.5) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::scale_color_manual(values = c("Propaganda" = "black", "Public Service" = "grey50")) +
  ggplot2::scale_linetype_manual(values = c("Propaganda" = "solid", "Public Service" = "dashed")) +
  ggplot2::labs(
    x = NULL,
    y = "Mean one-click engagement rate",
    color = NULL, linetype = NULL
  ) +
  ggplot2::annotate("text", x = cutoff_2018, y = Inf, label = "Dec 2018",
    vjust = 2, hjust = 1.1, size = 3, color = "grey30") +
  ggplot2::annotate("text", x = cutoff_2020, y = Inf, label = "Jul 2020",
    vjust = 2, hjust = -0.1, size = 3, color = "grey30") +
  rd_theme +
  ggplot2::theme(legend.position = "bottom")

save_figure(
  path = file.path(paths$figures, "appendix_did_trend_propaganda_vs_service.pdf"),
  plot = p_trend,
  width = 9,
  height = 5,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)

message("Saved DiD tables and event study figures.")
