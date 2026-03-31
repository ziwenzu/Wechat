bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

dt <- load_rdd_data(paths)
max_window <- 30L
dt[, account_id := .GRP, by = public_account_name]

# ---- Placebo cutoff dates ----------------------------------------------------

placebo_dates <- as.Date(c("2018-06-21", "2018-09-21",
                           "2019-03-21", "2019-06-21"))

placebo_res <- data.table::rbindlist(lapply(placebo_dates, function(d) {
  days_var <- as.integer(dt$publish_date - d)
  keep <- abs(days_var) <= max_window
  safe_rdd(dt$one_click_rate[keep], days_var[keep], dt$account_id[keep],
           label = format(d, "%Y-%m-%d"))
}))

fmt_rdd_tex(placebo_res,
            caption = "Placebo cutoff tests for one-click rate",
            label = "tab:rdd-placebo-cutoffs",
            filepath = file.path(paths$tables, "rdd_placebo_cutoffs.tex"))

# ---- 2019-03-15 Haokan-to-Zaikan rename placebo -----------------------------

rename_date <- as.Date("2019-03-15")
days_rename <- as.integer(dt$publish_date - rename_date)
keep <- abs(days_rename) <= max_window

rename_res <- data.table::rbindlist(list(
  safe_rdd(dt$one_click_rate[keep], days_rename[keep], dt$account_id[keep],
           label = "One-click rate"),
  safe_rdd(dt$share_rate[keep], days_rename[keep], dt$account_id[keep],
           label = "Share rate"),
  safe_rdd(dt$read_num[keep], days_rename[keep], dt$account_id[keep],
           label = "Reads")
))

fmt_rdd_tex(rename_res,
            caption = "Placebo RDD at 2019-03-15 Haokan-to-Zaikan rename",
            label = "tab:placebo-2019-rename",
            filepath = file.path(paths$tables, "appendix_placebo_2019.tex"))

message("Saved placebo cutoff and rename tables.")
