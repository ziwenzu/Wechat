bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

# ---------------------------------------------------------------------------
# 33_rdd_policy_placebo_2018.R
#
# Reviewer-facing check for the December 2018 design. On December 7, 2018 the
# State Council General Office issued the Opinions on Promoting the Healthy and
# Orderly Development of Government New Media (国办发〔2018〕123号), which bans
# forcing citizens to like or forward posts and bans buying followers. The
# WeChat footer redesign followed two weeks later (December 21). This script:
#
#   (1) estimates placebo RDDs at the December 7 policy date;
#   (2) tests whether one-click engagement was already trending down between
#       the policy date and the redesign (left-side local slope, Dec 7-20);
#   (3) re-estimates the December 21 RDD with the left window truncated at
#       December 7, so that no pre-policy observations enter the fit.
#
# DATA STATUS: uses data/wechat_instructional_dataset.rds (SIMULATED
# engagement counts, see DATA_STATUS.md). Outputs are teaching material.
# ---------------------------------------------------------------------------

ensure_packages(c("data.table", "fs", "rdrobust"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

sim_note <- "Simulated teaching data (see DATA_STATUS.md)."

dt <- load_rdd_data(paths)
keep_cols <- c("account_id", "publish_date", "content_family", "read_num",
               "like_rate", "look_rate", "share_rate", "collect_rate", "one_click_rate")
dt <- dt[, ..keep_cols]
dt[, log_reads := log1p(read_num)]

policy_date <- as.Date("2018-12-07")
redesign_date <- as.Date("2018-12-21")
dt[, days_from_policy := as.integer(publish_date - policy_date)]
dt[, days_from_2018 := as.integer(publish_date - redesign_date)]

# (1) Placebo RDDs at the policy date, standard 30-day window.
art_policy <- dt[abs(days_from_policy) <= 30 & read_num > 0]

placebo_rows <- data.table::rbindlist(list(
  safe_rdd(art_policy$one_click_rate, art_policy$days_from_policy, cl = art_policy$account_id,
           label = "Dec 7 policy placebo: one-click rate"),
  safe_rdd(art_policy$like_rate, art_policy$days_from_policy, cl = art_policy$account_id,
           label = "Dec 7 policy placebo: like rate"),
  safe_rdd(art_policy$share_rate, art_policy$days_from_policy, cl = art_policy$account_id,
           label = "Dec 7 policy placebo: share rate"),
  safe_rdd(art_policy$collect_rate, art_policy$days_from_policy, cl = art_policy$account_id,
           label = "Dec 7 policy placebo: collect rate"),
  safe_rdd(art_policy$log_reads, art_policy$days_from_policy, cl = art_policy$account_id,
           label = "Dec 7 policy placebo: log(1 + reads)")
))

# (3) December 21 RDD with the left window truncated at the policy date
#     (no observations before December 7 enter the fit).
art_trunc <- dt[days_from_2018 >= -14 & days_from_2018 <= 30 & read_num > 0]
trunc_rows <- data.table::rbindlist(list(
  safe_rdd(art_trunc$one_click_rate, art_trunc$days_from_2018, cl = art_trunc$account_id,
           label = "Dec 21 redesign, left window starts Dec 7: one-click rate"),
  safe_rdd(art_trunc$share_rate, art_trunc$days_from_2018, cl = art_trunc$account_id,
           label = "Dec 21 redesign, left window starts Dec 7: share rate")
))

fmt_rdd_tex(
  data.table::rbindlist(list(placebo_rows, trunc_rows)),
  caption = paste0(
    "The December 7, 2018 State Council opinion on government new media as a placebo cutoff, ",
    "and the December 21 redesign estimated without pre-policy observations. Local linear RD, ",
    "triangular kernel, MSE-optimal bandwidth unless the window is truncated, account-clustered SEs. ",
    sim_note
  ),
  label = "tab:rdd-policy-placebo-2018",
  filepath = file.path(paths$tables, "appendix_rdd_policy_placebo_2018.tex")
)

# (2) Left-side local slope between the policy date and the redesign.
#     A negative, significant slope would indicate that engagement was already
#     falling after the policy announcement.
between <- dt[publish_date >= policy_date & publish_date < redesign_date & read_num > 0]
slope_fit <- function(d, outcome, label) {
  loc <- d[is.finite(get(outcome)), .(y = get(outcome), x = days_from_policy, account_id)]
  X <- cbind(1, loc$x)
  XtX_inv <- solve(crossprod(X))
  beta <- drop(XtX_inv %*% crossprod(X, loc$y))
  e <- drop(loc$y - X %*% beta)
  score_g <- rowsum(X * e, loc$account_id)
  G <- nrow(score_g); n <- nrow(loc); k <- 2L
  V <- XtX_inv %*% crossprod(score_g) %*% XtX_inv * (G / (G - 1)) * ((n - 1) / (n - k))
  se <- sqrt(diag(V))[2]
  data.table::data.table(
    Outcome = label,
    `Daily slope, Dec 7-20` = beta[2],
    SE = se,
    `p-value` = 2 * stats::pnorm(-abs(beta[2] / se)),
    Posts = n
  )
}
slope_rows <- data.table::rbindlist(list(
  slope_fit(between, "one_click_rate", "One-click rate"),
  slope_fit(between, "like_rate", "Like rate"),
  slope_fit(between, "share_rate", "Share rate"),
  slope_fit(between, "log_reads", "log(1 + reads)")
))

write_tex_table(
  slope_rows,
  path = file.path(paths$tables, "appendix_policy_to_redesign_slope.tex"),
  caption = paste0(
    "Linear time trend in engagement between the December 7, 2018 policy and the December 21 redesign ",
    "(posts published December 7-20 with at least one read). Account-clustered SEs. ",
    sim_note
  ),
  label = "tab:policy-to-redesign-slope",
  digits = c(`Daily slope, Dec 7-20` = 5L, SE = 5L, `p-value` = 3L, Posts = 0L),
  align = "lrrrr"
)

message("Saved policy placebo and policy-to-redesign slope tables.")
