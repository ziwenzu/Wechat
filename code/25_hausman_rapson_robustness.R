bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "sandwich", "lmtest"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]
max_window <- 30L

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

# ==============================================================================
# Hausman-Rapson (2018) diagnostic: daily N and cross-sectional variation
# ==============================================================================

message("=== Hausman-Rapson diagnostics ===")

daily_n_2018 <- art_2018[, .(n_articles = .N, n_accounts = uniqueN(account_id)), by = days_from_2018]
daily_n_2020 <- art_2020[, .(n_articles = .N, n_accounts = uniqueN(account_id)), by = days_from_2020]

hr_summary <- data.table::data.table(
  Statistic = c(
    "Articles per day (mean, 2018 window)",
    "Articles per day (median, 2018 window)",
    "Accounts per day (mean, 2018 window)",
    "Articles per day (mean, 2020 window)",
    "Articles per day (median, 2020 window)",
    "Accounts per day (mean, 2020 window)",
    "Total articles in 2018 window",
    "Total articles in 2020 window",
    "Unique accounts in 2018 window",
    "Unique accounts in 2020 window"
  ),
  Value = c(
    mean(daily_n_2018$n_articles),
    median(daily_n_2018$n_articles),
    mean(daily_n_2018$n_accounts),
    mean(daily_n_2020$n_articles),
    median(daily_n_2020$n_articles),
    mean(daily_n_2020$n_accounts),
    nrow(art_2018),
    nrow(art_2020),
    uniqueN(art_2018$account_id),
    uniqueN(art_2020$account_id)
  )
)

write_tex_table(
  hr_summary,
  path = file.path(paths$tables, "appendix_hausman_rapson_diagnostics.tex"),
  caption = paste(
    "Hausman-Rapson (2018) diagnostics. Each observation is an article, not a",
    "time point. The large number of articles per day provides extensive",
    "cross-sectional variation, addressing the concern that temporal RDD",
    "may lack independent observations."
  ),
  label = "tab:hr-diagnostics",
  digits = c(Value = 0L),
  align = "lr"
)

# ==============================================================================
# Alternative clustering levels
# ==============================================================================

message("=== Alternative clustering ===")

run_alt_cluster <- function(sub, xvar, yvar, cutoff_tag) {
  y <- sub[[yvar]]
  x <- sub[[xvar]]

  results <- list(
    safe_rdd(y, x, sub$account_id, label = paste0("Account cluster (", cutoff_tag, ")")),
    safe_rdd(y, x, as.integer(sub[[xvar]]),
             label = paste0("Day cluster (", cutoff_tag, ")")),
    safe_rdd(y, x, NULL,
             label = paste0("HC robust, no cluster (", cutoff_tag, ")"))
  )

  if ("province" %in% names(sub)) {
    prov_id <- as.integer(as.factor(sub$province))
    results <- c(results, list(
      safe_rdd(y, x, prov_id,
               label = paste0("Province cluster (", cutoff_tag, ")"))
    ))
  }

  data.table::rbindlist(results)
}

cluster_res <- data.table::rbindlist(list(
  run_alt_cluster(art_2018, "days_from_2018", "one_click_rate", "2018"),
  run_alt_cluster(art_2020, "days_from_2020", "one_click_rate", "2020")
))

fmt_rdd_tex(
  cluster_res,
  caption = paste(
    "Alternative clustering for RDD standard errors.",
    "Baseline clusters at the account level.",
    "Day-level clustering addresses Hausman-Rapson (2018) concerns about",
    "within-day correlation in temporal RDD designs."
  ),
  label = "tab:rdd-alt-cluster",
  filepath = file.path(paths$tables, "appendix_rdd_alt_clustering.tex")
)

# ==============================================================================
# Two-way clustered SE via manual local-linear regression
# ==============================================================================

message("=== Two-way clustered SE ===")

twoway_cluster_rdd <- function(sub, xvar, yvar, cutoff_tag) {
  base <- safe_rdd(sub[[yvar]], sub[[xvar]], sub$account_id)
  if (is.null(base)) return(NULL)
  h <- (base$bw_l + base$bw_r) / 2

  loc <- data.table::copy(sub[abs(get(xvar)) <= h])
  loc[, `:=`(x = get(xvar), y = get(yvar),
             post = as.integer(get(xvar) >= 0),
             wt = pmax(0, 1 - abs(get(xvar)) / h))]

  fit <- stats::lm(y ~ post + x + post:x, data = loc, weights = wt)

  vc_account <- sandwich::vcovCL(fit, cluster = loc$account_id, type = "HC1")
  vc_day     <- sandwich::vcovCL(fit, cluster = loc[[xvar]], type = "HC1")
  vc_hc      <- sandwich::vcovHC(fit, type = "HC1")
  vc_twoway  <- vc_account + vc_day - vc_hc

  se_account <- sqrt(vc_account["post", "post"])
  se_day     <- sqrt(vc_day["post", "post"])
  se_twoway  <- sqrt(max(0, vc_twoway["post", "post"]))

  tau <- unname(stats::coef(fit)["post"])

  data.table::data.table(
    label = c(
      paste0("Account cluster (", cutoff_tag, ")"),
      paste0("Day cluster (", cutoff_tag, ")"),
      paste0("Two-way: account + day (", cutoff_tag, ")")
    ),
    tau_rb = tau, se_rb = c(se_account, se_day, se_twoway),
    ci_lo = tau - 1.96 * c(se_account, se_day, se_twoway),
    ci_hi = tau + 1.96 * c(se_account, se_day, se_twoway),
    pvalue = 2 * stats::pnorm(-abs(tau / c(se_account, se_day, se_twoway))),
    bw_l = h, bw_r = h, n_l = sum(loc$x < 0), n_r = sum(loc$x >= 0), p = 1L
  )
}

twoway_res <- data.table::rbindlist(list(
  twoway_cluster_rdd(art_2018, "days_from_2018", "one_click_rate", "2018"),
  twoway_cluster_rdd(art_2020, "days_from_2020", "one_click_rate", "2020")
))

fmt_rdd_tex(
  twoway_res,
  caption = paste(
    "Two-way clustered standard errors (account and day).",
    "Following Cameron, Gelbach, and Miller (2011), two-way clustering",
    "addresses both within-account serial correlation and within-day",
    "cross-sectional dependence."
  ),
  label = "tab:rdd-twoway-cluster",
  filepath = file.path(paths$tables, "appendix_rdd_twoway_cluster.tex")
)

message("Saved Hausman-Rapson diagnostics and clustering robustness tables.")
