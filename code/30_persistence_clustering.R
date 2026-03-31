bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]

max_window <- 90L

# ==============================================================================
# Expanding donut: persistence analysis (Hilbig 2024 JOP style)
# ==============================================================================

message("=== Expanding donut persistence ===")

run_expanding_donut <- function(sub, xvar, yvar, cutoff_tag,
                                 donut_sizes = 0:21) {
  y <- sub[[yvar]]
  x <- sub[[xvar]]
  cl <- sub$account_id

  rows <- lapply(donut_sizes, function(d) {
    if (d == 0L) {
      keep <- rep(TRUE, length(x))
    } else {
      keep <- abs(x) > d
    }
    if (sum(keep & x < 0) < 20 || sum(keep & x >= 0) < 20) return(NULL)

    r <- safe_rdd(y[keep], x[keep], cl[keep],
                  label = sprintf("Donut %d days (%s)", d, cutoff_tag))
    if (!is.null(r)) r[, donut := d]
    r
  })
  data.table::rbindlist(rows)
}

donut_res <- data.table::rbindlist(list(
  run_expanding_donut(
    dt[abs(days_from_2018) <= max_window],
    "days_from_2018", "one_click_rate", "2018"
  ),
  run_expanding_donut(
    dt[abs(days_from_2020) <= max_window],
    "days_from_2020", "one_click_rate", "2020"
  )
))

if (nrow(donut_res) > 0) {
  donut_res[, cutoff := ifelse(grepl("2018", label), "Dec 2018", "Jul 2020")]

  p_donut <- ggplot2::ggplot(donut_res, ggplot2::aes(x = donut, y = tau_rb)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
      fill = "grey75", alpha = 0.5
    ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::facet_wrap(~cutoff, scales = "free_y") +
    ggplot2::labs(
      x = "Donut hole size (days excluded from each side)",
      y = "RDD estimate (bias-corrected)"
    ) +
    rd_theme

  save_figure(
    path = file.path(paths$figures, "appendix_expanding_donut.pdf"),
    plot = p_donut,
    width = 9,
    height = 4.5,
    units = "in",
    bg = "white",
    useDingbats = FALSE
  )

  fmt_rdd_tex(
    donut_res[donut %in% c(0, 1, 2, 3, 5, 7, 10, 14, 21)],
    caption = paste(
      "Expanding donut-hole RDD estimates for one-click rate.",
      "Progressively excluding observations within d days of the cutoff.",
      "Persistent estimates as d grows indicate the effect is not driven by",
      "immediate post-reform confusion or novelty."
    ),
    label = "tab:expanding-donut",
    filepath = file.path(paths$tables, "appendix_expanding_donut.tex")
  )
}

# ==============================================================================
# Additional placebo cutoff: all July 1 dates without reform
# ==============================================================================

message("=== Additional July 1 placebos ===")

july1_dates <- as.Date(c("2017-07-01", "2019-07-01", "2021-07-01"))

july1_placebos <- data.table::rbindlist(lapply(july1_dates, function(d) {
  days_var <- as.integer(dt$publish_date - d)
  keep <- abs(days_var) <= 30L
  if (sum(keep) < 100) return(NULL)

  data.table::rbindlist(list(
    safe_rdd(dt$one_click_rate[keep], days_var[keep], dt$account_id[keep],
             label = paste0("One-click (", format(d, "%Y-%m-%d"), ")")),
    safe_rdd(dt$like_rate[keep], days_var[keep], dt$account_id[keep],
             label = paste0("Like (", format(d, "%Y-%m-%d"), ")")),
    safe_rdd(dt$look_rate[keep], days_var[keep], dt$account_id[keep],
             label = paste0("Zaikan (", format(d, "%Y-%m-%d"), ")"))
  ))
}))

if (nrow(july1_placebos) > 0) {
  fmt_rdd_tex(
    july1_placebos,
    caption = paste(
      "Placebo RDD at every July 1 without an interface reform.",
      "If engagement rates routinely shift around July 1 (CPC anniversary)",
      "even without a reform, the 2020 estimate may be confounded."
    ),
    label = "tab:july1-placebos",
    filepath = file.path(paths$tables, "appendix_july1_placebos.tex")
  )
}

message("Saved expanding donut and additional placebo tables/figures.")
