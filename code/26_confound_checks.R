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
max_window <- 30L

party_pat <- "建党|党的生日|七一|光辉历程|党史|党旗|入党|党员|红船|初心使命"
dt[, title_clean := data.table::fifelse(is.na(title), "", title)]
dt[, keywords_clean := data.table::fifelse(is.na(keywords), "", keywords)]
dt[, party_anniversary_flag := grepl(
  party_pat,
  paste(title_clean, keywords_clean),
  perl = TRUE
)]
dt[, dow := as.integer(format(publish_date, "%u"))]
dt[, is_weekend := dow >= 6L]

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

safe_if_observed <- function(y, x, cl, label) {
  keep <- is.finite(y) & is.finite(x) & !is.na(cl)
  if (sum(keep) == 0L) return(NULL)
  if (sum(x[keep] < 0) < 20L || sum(x[keep] >= 0) < 20L) return(NULL)
  safe_rdd(y[keep], x[keep], cl[keep], label = label)
}

# ==============================================================================
# 2019-07-01 placebo: Party anniversary without interface reform
# ==============================================================================

message("=== 2019-07-01 Party anniversary placebo ===")

dt[, days_from_2019jul := as.integer(publish_date - as.Date("2019-07-01"))]
art_2019jul <- dt[abs(days_from_2019jul) <= max_window]

placebo_party <- data.table::rbindlist(list(
  safe_rdd(art_2019jul$one_click_rate, art_2019jul$days_from_2019jul,
           art_2019jul$account_id, label = "One-click rate (2019-07-01)"),
  safe_if_observed(art_2019jul$read_num, art_2019jul$days_from_2019jul,
                   art_2019jul$account_id, label = "Reads (2019-07-01)"),
  safe_rdd(art_2019jul$share_rate, art_2019jul$days_from_2019jul,
           art_2019jul$account_id, label = "Share rate (2019-07-01)")
))

fmt_rdd_tex(
  placebo_party,
  caption = paste(
    "Placebo RDD at July 1, 2019 (CPC 98th anniversary, no interface reform).",
    "This date matches the calendar position of the July 2020 cutoff but",
    "without the footer redesign. A non-zero one-click estimate therefore",
    "indicates that some July 1 engagement movement may reflect calendar-specific",
    "dynamics rather than the redesign alone."
  ),
  label = "tab:placebo-party-2019",
  filepath = file.path(paths$tables, "appendix_placebo_party_2019.tex")
)

# ==============================================================================
# Exclude party-themed articles from 2020 cutoff
# ==============================================================================

message("=== Exclude party-themed articles ===")

art_2020_no_party <- art_2020[party_anniversary_flag == FALSE]

party_excl <- data.table::rbindlist(list(
  safe_rdd(art_2020$one_click_rate, art_2020$days_from_2020,
           art_2020$account_id, label = "Full sample: One-click"),
  safe_rdd(art_2020_no_party$one_click_rate, art_2020_no_party$days_from_2020,
           art_2020_no_party$account_id, label = "Excl. party-themed: One-click"),
  safe_rdd(art_2020$like_rate, art_2020$days_from_2020,
           art_2020$account_id, label = "Full sample: Like"),
  safe_rdd(art_2020_no_party$like_rate, art_2020_no_party$days_from_2020,
           art_2020_no_party$account_id, label = "Excl. party-themed: Like"),
  safe_rdd(art_2020$look_rate, art_2020$days_from_2020,
           art_2020$account_id, label = "Full sample: Zaikan"),
  safe_rdd(art_2020_no_party$look_rate, art_2020_no_party$days_from_2020,
           art_2020_no_party$account_id, label = "Excl. party-themed: Zaikan")
))

party_share <- art_2020[, .(
  n_party = sum(party_anniversary_flag),
  n_total = .N,
  share = mean(party_anniversary_flag)
), by = .(side = ifelse(days_from_2020 < 0, "Pre", "Post"))]

message("Party-themed article shares around 2020 cutoff:")
print(party_share)

fmt_rdd_tex(
  party_excl,
  caption = paste(
    "RDD estimates at July 2020 cutoff excluding party-anniversary articles.",
    "Articles whose title or keywords contain CPC-anniversary terms are dropped.",
    sprintf("Removed %.1f%% of articles pre-cutoff and %.1f%% post-cutoff.",
            100 * party_share[side == "Pre", share],
            100 * party_share[side == "Post", share])
  ),
  label = "tab:rdd-2020-no-party",
  filepath = file.path(paths$tables, "appendix_rdd_2020_excl_party.tex")
)

# ==============================================================================
# Omnibus balance test: predicted one-click rate as single index
# ==============================================================================

message("=== Omnibus balance test ===")

run_omnibus <- function(sub, xvar, cutoff_tag) {
  sub <- data.table::copy(sub)
  sub[, article_rank := seq_len(.N), by = .(account_id, publish_date)]
  sub[, is_head := article_rank == 1L]
  sub[, title_length := nchar(title)]
  sub[, dow := as.integer(format(publish_date, "%u"))]

  covs_mat <- model.matrix(
    ~ title_length + is_head + dow +
      I(content_family == "hard_propaganda") +
      I(content_family == "soft_propaganda") +
      I(content_family == "public_service"),
    data = sub
  )[, -1, drop = FALSE]

  left <- sub[[xvar]] < 0
  fit_left <- stats::lm(sub$one_click_rate[left] ~ covs_mat[left, ])
  sub[, predicted_y := as.numeric(covs_mat %*% stats::coef(fit_left)[-1])]

  safe_rdd(sub$predicted_y, sub[[xvar]], sub$account_id,
           label = paste0("Predicted one-click (", cutoff_tag, ")"))
}

art_2018 <- dt[abs(days_from_2018) <= max_window]

omnibus_res <- data.table::rbindlist(list(
  run_omnibus(art_2018, "days_from_2018", "2018"),
  run_omnibus(art_2020, "days_from_2020", "2020")
))

fmt_rdd_tex(
  omnibus_res,
  caption = paste(
    "Omnibus article-composition balance test. The outcome is a predicted",
    "one-click rate constructed from pre-publication article attributes",
    "(title length, head-article indicator, day-of-week, and content-family",
    "indicators) using coefficients estimated on the left side of the cutoff",
    "only. A null result indicates no systematic composition discontinuity."
  ),
  label = "tab:omnibus-balance",
  filepath = file.path(paths$tables, "appendix_omnibus_balance.tex")
)

# ==============================================================================
# Content composition jump at 2020 cutoff (party anniversary)
# ==============================================================================

message("=== Content composition jump ===")

comp_res <- data.table::rbindlist(list(
  safe_rdd(as.numeric(art_2020$party_anniversary_flag), art_2020$days_from_2020,
           art_2020$account_id, label = "Party-themed share (2020)"),
  safe_rdd(as.numeric(art_2020$content_family == "hard_propaganda"),
           art_2020$days_from_2020, art_2020$account_id,
           label = "Hard propaganda share (2020)"),
  safe_rdd(art_2020$read_num, art_2020$days_from_2020,
           art_2020$account_id, label = "Reads (2020)"),
  safe_rdd(as.numeric(art_2019jul$party_anniversary_flag),
           art_2019jul$days_from_2019jul, art_2019jul$account_id,
           label = "Party-themed share (2019 placebo)")
), fill = TRUE)

fmt_rdd_tex(
  comp_res,
  caption = paste(
    "Content composition checks at the July 2020 cutoff.",
    "Tests whether party-anniversary articles or hard-propaganda content",
    "jump discontinuously, which would indicate confounding from the",
    "CPC anniversary rather than the interface reform."
  ),
  label = "tab:content-composition-jump",
  filepath = file.path(paths$tables, "appendix_content_composition_jump.tex")
)

message("Saved confound check tables.")
