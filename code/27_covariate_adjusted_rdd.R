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
dt[, article_rank := seq_len(.N), by = .(account_id, publish_date)]
dt[, is_head := article_rank == 1L]
dt[, title_length := nchar(title)]
dt[, dow := as.integer(format(publish_date, "%u"))]
dt[, is_hard_prop := as.numeric(content_family == "hard_propaganda")]
dt[, is_soft_prop := as.numeric(content_family == "soft_propaganda")]
dt[, is_pub_svc := as.numeric(content_family == "public_service")]

max_window <- 30L
art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

# ==============================================================================
# Covariate-adjusted RDD: main outcomes with covariates via rdrobust
# ==============================================================================

message("=== Covariate-adjusted RDD ===")

build_covs <- function(sub) {
  as.matrix(sub[, .(read_num, title_length, is_head = as.numeric(is_head),
                     dow, is_hard_prop, is_soft_prop, is_pub_svc)])
}

run_cov_comparison <- function(sub, xvar, yvar, y_label, cutoff_tag) {
  y  <- sub[[yvar]]
  x  <- sub[[xvar]]
  cl <- sub$account_id
  covs <- build_covs(sub)

  data.table::rbindlist(list(
    safe_rdd(y, x, cl, label = paste0(y_label, ", no covariates (", cutoff_tag, ")")),
    safe_rdd_covs(y, x, cl, covs = covs,
                  label = paste0(y_label, ", with covariates (", cutoff_tag, ")"))
  ))
}

cov_adj_res <- data.table::rbindlist(list(
  run_cov_comparison(art_2018, "days_from_2018", "one_click_rate", "One-click", "2018"),
  run_cov_comparison(art_2020, "days_from_2020", "one_click_rate", "One-click", "2020"),
  run_cov_comparison(art_2020, "days_from_2020", "like_rate", "Like", "2020"),
  run_cov_comparison(art_2020, "days_from_2020", "look_rate", "Zaikan", "2020"),
  run_cov_comparison(art_2020, "days_from_2020", "share_rate", "Share", "2020")
))

fmt_rdd_tex(
  cov_adj_res,
  caption = paste(
    "Covariate-adjusted RDD estimates.",
    "Covariates: reads, title length, head-article indicator, day-of-week,",
    "and content-family indicators. Following Calonico, Cattaneo, and Titiunik",
    "(2019), covariates are included in the local polynomial regression to",
    "improve efficiency without changing the point estimate under correct",
    "specification."
  ),
  label = "tab:rdd-cov-adjusted",
  filepath = file.path(paths$tables, "appendix_rdd_covariate_adjusted.tex")
)

message("Saved covariate-adjusted RDD table.")
