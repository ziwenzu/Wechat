bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "rdpower", "rdlocrand"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]
max_window <- 30L

art_2018 <- dt[abs(days_from_2018) <= max_window]
art_2020 <- dt[abs(days_from_2020) <= max_window]

# ==============================================================================
# Power analysis: rdpower (Cattaneo, Titiunik, Vazquez-Bare 2019)
# ==============================================================================

message("=== Power analysis ===")

run_power <- function(y, x, cutoff_tag) {
  base <- rdrobust::rdrobust(y = y, x = x, p = 1, kernel = "triangular",
                              bwselect = "mserd", all = TRUE)
  tau_hat <- base$coef[3]
  h_opt <- mean(base$bws[1, ])

  taus_to_test <- c(0.25, 0.50, 0.75, 1.00, 1.25, 1.50) * abs(tau_hat)

  power_rows <- lapply(taus_to_test, function(tau_test) {
    tryCatch({
      pw <- rdpower::rdpower(
        y = y, x = x, tau = tau_test,
        p = 1, kernel = "triangular",
        cluster = NULL
      )
      data.table::data.table(
        cutoff = cutoff_tag,
        tau_tested = tau_test,
        tau_ratio = tau_test / abs(tau_hat),
        power = pw$power,
        alpha = pw$alpha,
        N_h_l = pw$N_h_l,
        N_h_r = pw$N_h_r,
        sampsi_h_l = pw$sampsi_h_l,
        sampsi_h_r = pw$sampsi_h_r
      )
    }, error = function(e) {
      message("  Power calc skipped for tau=", tau_test, ": ", e$message)
      NULL
    })
  })
  data.table::rbindlist(power_rows)
}

power_res <- data.table::rbindlist(list(
  tryCatch(
    run_power(art_2018$one_click_rate, art_2018$days_from_2018, "2018"),
    error = function(e) { message("  Power 2018 failed: ", e$message); NULL }
  ),
  tryCatch(
    run_power(art_2020$one_click_rate, art_2020$days_from_2020, "2020"),
    error = function(e) { message("  Power 2020 failed: ", e$message); NULL }
  )
))

if (nrow(power_res) > 0) {
  write_tex_table(
    power_res[, .(Cutoff = cutoff,
                  `Effect tested` = tau_tested,
                  `Ratio to estimate` = tau_ratio,
                  Power = power,
                  `N left` = N_h_l,
                  `N right` = N_h_r)],
    path = file.path(paths$tables, "appendix_power_analysis.tex"),
    caption = paste(
      "Power analysis following Cattaneo, Titiunik, and Vazquez-Bare (2019).",
      "For each cutoff, we test power at various multiples of the estimated",
      "treatment effect. Alpha = 0.05 throughout."
    ),
    label = "tab:power-analysis",
    digits = c(`Effect tested` = 5L, `Ratio to estimate` = 2L, Power = 3L,
               `N left` = 0L, `N right` = 0L),
    align = "lrrrrr"
  )
}

# ==============================================================================
# Local randomization inference (rdlocrand)
# ==============================================================================

message("=== Local randomization inference ===")

run_locrand <- function(y, x, cl, wlist, cutoff_tag, outcome_label) {
  results <- lapply(wlist, function(w) {
    tryCatch({
      keep <- abs(x) <= w
      if (sum(keep & x < 0) < 10 || sum(keep & x >= 0) < 10) return(NULL)

      lr <- rdlocrand::rdrandinf(
        Y = y[keep], R = x[keep],
        wl = -w, wr = w,
        reps = 5000,
        quietly = TRUE
      )

      data.table::data.table(
        label = paste0(outcome_label, ", w=", w, " (", cutoff_tag, ")"),
        window = w,
        obs_stat = lr$obs.stat,
        p_value = lr$p.value,
        ci_lo = lr$ci[1],
        ci_hi = lr$ci[2],
        n_left = sum(keep & x < 0),
        n_right = sum(keep & x >= 0)
      )
    }, error = function(e) {
      message("  rdrandinf w=", w, " failed: ", e$message)
      NULL
    })
  })
  data.table::rbindlist(results)
}

windows <- c(2L, 3L, 5L, 7L)

locrand_res <- data.table::rbindlist(list(
  run_locrand(art_2018$one_click_rate, art_2018$days_from_2018,
              art_2018$account_id, windows, "2018", "One-click"),
  run_locrand(art_2020$one_click_rate, art_2020$days_from_2020,
              art_2020$account_id, windows, "2020", "One-click"),
  run_locrand(art_2020$like_rate, art_2020$days_from_2020,
              art_2020$account_id, windows, "2020", "Like"),
  run_locrand(art_2020$look_rate, art_2020$days_from_2020,
              art_2020$account_id, windows, "2020", "Zaikan")
))

if (nrow(locrand_res) > 0) {
  write_tex_table(
    locrand_res[, .(Specification = label,
                    Window = window,
                    `Test stat` = obs_stat,
                    `p-value` = p_value,
                    `CI low` = ci_lo,
                    `CI high` = ci_hi,
                    `N (L)` = n_left,
                    `N (R)` = n_right)],
    path = file.path(paths$tables, "appendix_local_randomization.tex"),
    caption = paste(
      "Local randomization inference (Cattaneo, Titiunik, and Vazquez-Bare 2015).",
      "Within narrow symmetric windows around each cutoff, treatment assignment",
      "is modeled as if randomized. P-values from 5,000 permutations of the",
      "Fisherian null of no effect."
    ),
    label = "tab:local-randomization",
    digits = c(`Test stat` = 4L, `p-value` = 3L, `CI low` = 4L,
               `CI high` = 4L, Window = 0L, `N (L)` = 0L, `N (R)` = 0L),
    align = "lrrrrrrr"
  )
}

message("Saved power analysis and local randomization tables.")
