bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "fixest", "rdrobust"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]

max_window <- 30L

family_pairs <- c("soft_propaganda", "hard_propaganda")
family_labels <- c(
  soft_propaganda = "Soft Propaganda vs Public Service",
  hard_propaganda = "Hard Propaganda vs Public Service"
)

run_local_diff <- function(cutoff_year, family, outcome, outcome_label) {
  xvar <- if (cutoff_year == 2018L) "days_from_2018" else "days_from_2020"

  sub <- data.table::copy(
    dt[
      content_family %in% c("public_service", family) &
        abs(get(xvar)) <= max_window
    ]
  )

  sub[, x := get(xvar)]
  sub[, treated := as.integer(content_family == family)]

  base <- tryCatch(
    rdrobust::rdrobust(
      y = sub[[outcome]],
      x = sub$x,
      cluster = sub$account_id,
      p = 1,
      kernel = "triangular",
      bwselect = "mserd",
      all = TRUE
    ),
    error = function(e) {
      message("  SKIP: ", cutoff_year, " / ", family, " / ", outcome, " - ", e$message)
      NULL
    }
  )

  if (is.null(base)) {
    return(NULL)
  }

  h <- mean(base$bws[1, ])
  loc <- sub[abs(x) <= h]
  loc[, post := as.integer(x >= 0)]
  loc[, wt := pmax(0, 1 - abs(x) / h)]

  fit <- tryCatch(
    fixest::feols(
      stats::as.formula(paste0(outcome, " ~ treated:post | account_id + publish_date")),
      data = loc,
      weights = ~wt,
      cluster = ~account_id
    ),
    error = function(e) {
      message("  SKIP FE: ", cutoff_year, " / ", family, " / ", outcome, " - ", e$message)
      NULL
    }
  )

  if (is.null(fit)) {
    return(NULL)
  }

  ct <- fixest::coeftable(fit)
  term <- rownames(ct)[1]

  data.table::data.table(
    label = paste0(
      if (cutoff_year == 2018L) "2018: " else "2020: ",
      family_labels[[family]],
      " | ",
      outcome_label
    ),
    tau_rb = ct[term, "Estimate"],
    se_rb = ct[term, "Std. Error"],
    ci_lo = ct[term, "Estimate"] - 1.96 * ct[term, "Std. Error"],
    ci_hi = ct[term, "Estimate"] + 1.96 * ct[term, "Std. Error"],
    pvalue = ct[term, "Pr(>|t|)"],
    bw_l = h,
    bw_r = h,
    n_l = sum(loc$x < 0),
    n_r = sum(loc$x >= 0),
    p = 1L
  )
}

rows <- data.table::rbindlist(list(
  run_local_diff(2018L, "soft_propaganda", "one_click_rate", "One-click"),
  run_local_diff(2018L, "hard_propaganda", "one_click_rate", "One-click"),
  run_local_diff(2020L, "soft_propaganda", "one_click_rate", "One-click"),
  run_local_diff(2020L, "hard_propaganda", "one_click_rate", "One-click"),
  run_local_diff(2020L, "soft_propaganda", "like_rate", "Like"),
  run_local_diff(2020L, "hard_propaganda", "like_rate", "Like"),
  run_local_diff(2020L, "soft_propaganda", "look_rate", "Zaikan"),
  run_local_diff(2020L, "hard_propaganda", "look_rate", "Zaikan")
), fill = TRUE)

fmt_rdd_tex(
  rows,
  caption = paste(
    "Family-differential local designs around each cutoff.",
    "Each row compares one propaganda family to public service within a",
    "pair-specific local window selected by \\texttt{rdrobust}.",
    "Observations are weighted by the corresponding triangular kernel and",
    "the regression absorbs account and calendar-date fixed effects, so the",
    "estimate captures whether the propaganda-service gap shifts discretely",
    "at the reform date."
  ),
  label = "tab:family-diff-local-rd",
  filepath = file.path(paths$tables, "appendix_family_differential_local_rd.tex")
)

message("Saved family-differential local RD table.")
