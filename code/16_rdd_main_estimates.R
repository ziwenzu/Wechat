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
art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

# --- H2: 2018 cutoff (Like → Haokan rename) ----------------------------------

h2_rows <- data.table::rbindlist(list(
  safe_rdd(art_2018$one_click_rate, art_2018$days_from_2018,
           cl = art_2018$account_id, p = 1,
           label = "One-click (linear, clustered)"),
  safe_rdd(art_2018$one_click_rate, art_2018$days_from_2018,
           cl = art_2018$account_id, p = 2,
           label = "One-click (quadratic, clustered)"),
  tryCatch({
    fit <- rdrobust::rdrobust(
      y = art_2018$one_click_rate, x = art_2018$days_from_2018,
      p = 1, kernel = "triangular", all = TRUE,
      cluster = art_2018$account_id, vce = "hc1", bwselect = "mserd")
    data.table::data.table(
      label = "One-click (robust HC, clustered)",
      tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
      bw_l = fit$bws[1, 1], bw_r = fit$bws[1, 2],
      n_l = fit$N_h[1], n_r = fit$N_h[2], p = 1L)
  }, error = function(e) { message("  SKIP: HC1 - ", e$message); NULL }),
  safe_rdd(art_2018$share_rate, art_2018$days_from_2018,
           cl = art_2018$account_id, p = 1,
           label = "Share rate (placebo, clustered)")
))

fmt_rdd_tex(
  h2_rows,
  caption  = "RD estimates at the December 2018 interface reform.",
  label    = "tab:rdd-2018-main",
  filepath = file.path(paths$tables, "rdd_2018_main.tex")
)

# --- H3: 2020 cutoff (Like + Zaikan + Share) ----------------------------------

h3_rows <- data.table::rbindlist(list(
  safe_rdd(art_2020$one_click_rate, art_2020$days_from_2020,
           cl = art_2020$account_id, p = 1,
           label = "One-click rate (linear, clustered)"),
  safe_rdd(art_2020$like_rate, art_2020$days_from_2020,
           cl = art_2020$account_id, p = 1,
           label = "Like rate (linear, clustered)"),
  safe_rdd(art_2020$look_rate, art_2020$days_from_2020,
           cl = art_2020$account_id, p = 1,
           label = "Zaikan rate (linear, clustered)"),
  safe_rdd(art_2020$share_rate, art_2020$days_from_2020,
           cl = art_2020$account_id, p = 1,
           label = "Share rate (linear, clustered)")
))

fmt_rdd_tex(
  h3_rows,
  caption  = "RD estimates at the July 2020 interface reform.",
  label    = "tab:rdd-2020-main",
  filepath = file.path(paths$tables, "rdd_2020_main.tex")
)

# --- H4: Heterogeneity by content family (2020 cutoff) ------------------------

families     <- c("public_service", "soft_propaganda",
                   "state_governance", "hard_propaganda")
family_labels <- c("Public Service", "Soft Propaganda",
                    "State Governance", "Hard Propaganda")

h4_outcomes <- c("one_click_rate", "like_rate", "look_rate")
h4_olabels  <- c("One-click", "Like", "Zaikan")

h4_rows <- data.table::rbindlist(lapply(seq_along(families), function(fi) {
  sub <- art_2020[content_family == families[fi]]
  data.table::rbindlist(lapply(seq_along(h4_outcomes), function(oi) {
    safe_rdd(sub[[h4_outcomes[oi]]], sub$days_from_2020,
             cl = sub$account_id, p = 1,
             label = paste0(family_labels[fi], ": ", h4_olabels[oi]))
  }))
}))

fmt_rdd_tex(
  h4_rows,
  caption  = "Heterogeneous RD estimates by content family at the July 2020 cutoff.",
  label    = "tab:rdd-2020-heterogeneity",
  filepath = file.path(paths$tables, "rdd_2020_heterogeneity.tex")
)

message("Done: RDD estimation tables saved to ", paths$tables)
