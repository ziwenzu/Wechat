bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "ggplot2", "scales"))
args <- parse_args()
components_only <- isTRUE(args$components_only)

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)
fs::dir_create(paths$tables, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]

max_window <- 90L

# ---- safe_rdd with kernel / bwselect options ---------------------------------

safe_rdd_ext <- function(y, x, cl = NULL, p = 1, h = NULL,
                         kernel = "triangular", bwselect = "mserd",
                         label = "") {
  tryCatch({
    args <- list(y = y, x = x, p = p, kernel = kernel, all = TRUE)
    if (!is.null(cl)) args$cluster <- cl
    if (!is.null(h)) { args$h <- h; args$b <- h } else { args$bwselect <- bwselect }
    fit <- do.call(rdrobust::rdrobust, args)
    data.table::data.table(
      label  = label, tau_rb = fit$coef[3], se_rb = fit$se[3],
      ci_lo  = fit$ci[3, 1], ci_hi = fit$ci[3, 2], pvalue = fit$pv[3],
      bw_l   = fit$bws[1, 1], bw_r = fit$bws[1, 2],
      n_l    = fit$N_h[1], n_r = fit$N_h[2], p = p
    )
  }, error = function(e) { message("  SKIP: ", label, " - ", e$message); NULL })
}

# ---- Robustness specifications -----------------------------------------------

run_robustness <- function(sub, xvar, yvar, cutoff_tag = NULL, outcome_label = NULL) {
  y  <- sub[[yvar]]
  x  <- sub[[xvar]]
  cl <- sub$account_id

  label_prefix <- if (!is.null(outcome_label) && nzchar(outcome_label)) {
    paste0(outcome_label, ": ")
  } else {
    ""
  }
  label_suffix <- if (!is.null(cutoff_tag) && nzchar(cutoff_tag)) {
    paste0(" (", cutoff_tag, ")")
  } else {
    ""
  }
  make_label <- function(stub) paste0(label_prefix, stub, label_suffix)

  baseline <- safe_rdd(y, x, cl, label = make_label("Baseline"))
  h_opt <- if (!is.null(baseline)) (baseline$bw_l + baseline$bw_r) / 2 else NA_real_

  results <- list(baseline)

  # BW sensitivity
  for (m in c(0.50, 0.75, 1.25, 1.50, 1.75, 2.00)) {
    if (!is.na(h_opt)) {
      results <- c(results, list(
        safe_rdd(y, x, cl, h = m * h_opt,
                 label = make_label(sprintf("BW %.2fx", m)))
      ))
    }
  }

  # Polynomial order
  for (pp in c(0L, 1L, 2L)) {
    results <- c(results, list(
      safe_rdd(y, x, cl, p = pp,
               label = make_label(sprintf("Poly p=%d", pp)))
    ))
  }

  # Donut hole
  for (d in c(1L, 2L, 3L)) {
    keep <- abs(x) > d
    results <- c(results, list(
      safe_rdd(y[keep], x[keep], cl[keep],
               label = make_label(sprintf("Donut +/-%d days", d)))
    ))
  }

  # Alternative kernels
  for (k in c("triangular", "uniform", "epanechnikov")) {
    results <- c(results, list(
      safe_rdd_ext(y, x, cl, kernel = k,
                   label = make_label(sprintf("Kernel: %s", k)))
    ))
  }

  # CER-optimal bandwidth
  results <- c(results, list(
    safe_rdd_ext(y, x, cl, bwselect = "cerrd",
                 label = make_label("CER-optimal"))
  ))

  data.table::rbindlist(results)
}

if (!components_only) {
  rob_res <- data.table::rbindlist(list(
    run_robustness(dt[abs(days_from_2018) <= max_window],
                   "days_from_2018", "one_click_rate", cutoff_tag = "2018"),
    run_robustness(dt[abs(days_from_2020) <= max_window],
                   "days_from_2020", "one_click_rate", cutoff_tag = "2020")
  ))

  fmt_rdd_tex(rob_res,
              caption = "Robustness of RDD estimates for one-click rate",
              label = "tab:rdd-robustness",
              filepath = file.path(paths$tables, "rdd_robustness_all.tex"))
}

rob_res_2020_components <- data.table::rbindlist(list(
  run_robustness(dt[abs(days_from_2020) <= max_window],
                 "days_from_2020", "like_rate", outcome_label = "Like"),
  run_robustness(dt[abs(days_from_2020) <= max_window],
                 "days_from_2020", "look_rate", outcome_label = "Zaikan")
))

fmt_rdd_tex(rob_res_2020_components,
            caption = "Robustness of 2020 RDD estimates for like and zaikan rates",
            label = "tab:rdd-2020-component-robustness",
            filepath = file.path(paths$tables, "appendix_rdd_2020_component_robustness.tex"))

# ---- BW sensitivity coefficient plot -----------------------------------------

if (!components_only) {
  bw_mults <- seq(0.50, 2.00, by = 0.10)

  bw_plot_data <- data.table::rbindlist(lapply(
    list(
      list(xvar = "days_from_2018", tag = "2018"),
      list(xvar = "days_from_2020", tag = "2020")
    ),
    function(spec) {
      sub <- dt[abs(get(spec$xvar)) <= max_window]
      y  <- sub$one_click_rate
      x  <- sub[[spec$xvar]]
      cl <- sub$account_id

      base <- safe_rdd(y, x, cl, label = "base")
      if (is.null(base)) return(NULL)
      h_opt <- (base$bw_l + base$bw_r) / 2

      data.table::rbindlist(lapply(bw_mults, function(m) {
        r <- safe_rdd(y, x, cl, h = m * h_opt,
                      label = sprintf("%.2f", m))
        if (!is.null(r)) r[, `:=`(cutoff = spec$tag, bw = m * h_opt, mult = m)]
        r
      }))
    }
  ))

  if (nrow(bw_plot_data) > 0) {
    bw_plot_data[, cutoff_label := ifelse(
      cutoff == "2018", "Dec 2018 cutoff", "Jul 2020 cutoff")]

    p_bw <- ggplot2::ggplot(bw_plot_data,
                             ggplot2::aes(x = bw, y = tau_rb)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
                           fill = "grey75", alpha = 0.5) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "grey40") +
      ggplot2::facet_wrap(~cutoff_label, scales = "free") +
      ggplot2::labs(x = "Bandwidth (days)", y = "RDD estimate") +
      rd_theme

    save_figure(
      path = file.path(paths$figures, "appendix_bw_sensitivity.pdf"),
      plot = p_bw,
      width = 9,
      height = 4.5,
      bg = "white",
      useDingbats = FALSE
    )
  }

  message("Saved robustness table and BW sensitivity plot.")
} else {
  message("Saved 2020 component robustness table.")
}
