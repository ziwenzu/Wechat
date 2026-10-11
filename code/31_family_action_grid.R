bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

# ---------------------------------------------------------------------------
# 31_family_action_grid.R
#
# Headline "content family x action" figure for the paper's spine:
#   attention follows utility; endorsement follows audience.
#
# Panel A  reads (median) by family
# Panel B  like rate (low-visibility approval)
# Panel C  zaikan rate (socially legible endorsement)
# Panel D  share rate (highest-visibility amplification)
#
# Sample: the three-button regime (publish_date >= 2020-07-01), posts with
# read_num > 0, so that all four actions coexist in the same footer.
#
# DATA STATUS: this script reads data/wechat_instructional_dataset.rds, whose
# engagement counts are SIMULATED (see DATA_STATUS.md). All outputs carry a
# "simulated teaching data" note. Do not cite them as findings.
# ---------------------------------------------------------------------------

ensure_packages(c("data.table", "fs", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)
fs::dir_create(paths$tables, recurse = TRUE)
slide_output_dir <- file.path(paths$root, "slides", "figures")
fs::dir_create(slide_output_dir, recurse = TRUE)

sim_note <- "Simulated teaching data (see DATA_STATUS.md)."

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))
dt[, account_id := .GRP, by = public_account_name]
# Keep only what this script needs; the full file is memory-heavy.
keep_cols <- c("account_id", "publish_date", "content_family", "read_num",
               "like_rate", "look_rate", "share_rate")
dt <- dt[, ..keep_cols]

cut_like_return <- as.Date("2020-07-01")

family_gloss <- c(
  "public_service"   = "Public service",
  "soft_propaganda"  = "Soft propaganda",
  "state_governance" = "State governance",
  "hard_propaganda"  = "Hard propaganda"
)
family_order <- names(family_gloss)

post <- dt[
  publish_date >= cut_like_return &
  read_num > 0 &
  content_family %in% family_order
]
post[, log_reads := log1p(read_num)]
post[, one_click_rate := like_rate + look_rate]
post[, content_family := factor(content_family, levels = family_order)]

# --- Family means with account-clustered 95% CIs ----------------------------

cluster_mean_ci <- function(d, outcome) {
  # Family mean with account-clustered SE, computed without lm():
  # Var(mean) = sum_g (sum_{i in g} e_i)^2 / n^2, e_i = y_i - mean.
  loc <- d[is.finite(get(outcome)), .(y = get(outcome), content_family, account_id)]
  loc[, mu := mean(y), by = content_family]
  loc[, e := y - mu]
  out <- loc[, .(
    mean = mu[1],
    n = .N,
    cluster_sum_sq = sum(tapply(e, account_id, sum)^2),
    g = data.table::uniqueN(account_id)
  ), by = content_family]
  out[, se := sqrt(cluster_sum_sq * (g / (g - 1)) / n^2)]
  out[, `:=`(ci_lo = mean - 1.96 * se, ci_hi = mean + 1.96 * se)]
  out[, outcome := outcome]
  out[, .(content_family = as.character(content_family), mean, se, ci_lo, ci_hi, outcome)]
}

rate_outcomes <- c("like_rate", "look_rate", "share_rate")
rate_means <- data.table::rbindlist(lapply(rate_outcomes, function(o) cluster_mean_ci(post, o)))

read_stats <- post[, .(
  n = .N,
  median_reads = as.numeric(stats::median(read_num)),
  mean_log_reads = mean(log_reads)
), by = content_family]

# Bootstrap-free CI for the median: use the order-statistic normal approximation
# on the pooled sample (teaching illustration only; account clustering ignored).
read_stats[, median_se := post[, {
  q <- stats::quantile(read_num, c(0.5))
  dens <- stats::density(log1p(read_num), n = 512)
  f_hat <- stats::approx(dens$x, dens$y, xout = log1p(q))$y / (1 + q)
  sqrt(0.25 / (.N * f_hat^2))
}, by = content_family]$V1]
read_stats[, `:=`(ci_lo = median_reads - 1.96 * median_se, ci_hi = median_reads + 1.96 * median_se)]

plot_rates <- data.table::copy(rate_means)
plot_rates[, panel := data.table::fifelse(
  outcome == "like_rate", "B. Like rate (count only, not visible to friends)",
  data.table::fifelse(outcome == "look_rate", "C. Zaikan rate (may appear in friends' Look feed)",
                      "D. Share rate (sent to chats or Moments)")
)]
plot_rates[, value := mean]

plot_reads <- read_stats[, .(content_family, value = median_reads, ci_lo, ci_hi)]
plot_reads[, panel := "A. Reads (median per post)"]

plot_dt <- data.table::rbindlist(list(
  plot_reads[, .(content_family, panel, value, ci_lo, ci_hi)],
  plot_rates[, .(content_family, panel, value, ci_lo, ci_hi)]
))
plot_dt[, content_family := factor(content_family, levels = rev(family_order))]
plot_dt[, panel := factor(panel, levels = c(
  "A. Reads (median per post)",
  "B. Like rate (count only, not visible to friends)",
  "C. Zaikan rate (may appear in friends' Look feed)",
  "D. Share rate (sent to chats or Moments)"
))]
plot_dt[, family_label := family_gloss[as.character(content_family)]]
plot_dt[, family_label := factor(family_label, levels = rev(unname(family_gloss)))]

grid_theme <- ggplot2::theme_classic(base_size = 11, base_family = "serif") +
  ggplot2::theme(
    legend.position = "none",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 10, hjust = 0, margin = ggplot2::margin(b = 3)),
    panel.spacing = ggplot2::unit(1.1, "lines"),
    axis.text = ggplot2::element_text(size = 9.5, color = "#303030"),
    axis.title = ggplot2::element_text(size = 10, color = "#1A1A1A"),
    axis.line = ggplot2::element_line(linewidth = 0.4, color = "#303030"),
    axis.ticks = ggplot2::element_line(linewidth = 0.4, color = "#303030"),
    plot.caption = ggplot2::element_text(size = 8, color = "grey35", hjust = 0),
    plot.margin = ggplot2::margin(t = 4, r = 10, b = 6, l = 6)
  )

p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = value, y = family_label)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = ci_lo, xend = ci_hi, y = family_label, yend = family_label),
    linewidth = 0.6, color = "grey45"
  ) +
  ggplot2::geom_point(shape = 21, size = 3.4, fill = "grey15", color = "black", stroke = 0.3) +
  ggplot2::facet_wrap(~panel, scales = "free_x", ncol = 2) +
  ggplot2::scale_x_continuous(
    labels = function(x) ifelse(x >= 1, scales::comma(x), scales::percent(x, accuracy = 0.1)),
    expand = ggplot2::expansion(mult = c(0.05, 0.12))
  ) +
  ggplot2::labs(
    x = NULL, y = NULL,
    caption = paste0(
      "Three-button regime (Jul. 2020 - Dec. 2024), posts with at least one read. ",
      "Points are family means (rates) or medians (reads); bars are 95% CIs with account-clustered SEs.\n",
      sim_note
    )
  ) +
  grid_theme

save_figure(
  path = file.path(paths$figures, "main_family_action_grid.pdf"),
  plot = p, width = 9, height = 5.6, units = "in", bg = "white"
)

p_slide <- p +
  ggplot2::labs(caption = sim_note) +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans"),
    strip.text = ggplot2::element_text(family = "sans", size = 13, hjust = 0),
    axis.text = ggplot2::element_text(family = "sans", size = 12),
    plot.caption = ggplot2::element_text(family = "sans", size = 10)
  )
save_figure(
  path = file.path(slide_output_dir, "family_action_grid_slide.pdf"),
  plot = p_slide, width = 10.5, height = 5.2, units = "in", bg = "white"
)

# --- Summary table ------------------------------------------------------------

wide <- data.table::dcast(rate_means, content_family ~ outcome, value.var = "mean")
summary_tbl <- merge(read_stats[, .(content_family, n, median_reads)], wide, by = "content_family")
summary_tbl[, content_family := factor(content_family, levels = family_order)]
data.table::setorder(summary_tbl, content_family)
summary_tbl[, VAI := like_rate / (like_rate + look_rate)]

tex_tbl <- summary_tbl[, .(
  Family = family_gloss[as.character(content_family)],
  Posts = n,
  `Median reads` = median_reads,
  `Like rate (%)` = 100 * like_rate,
  `Zaikan rate (%)` = 100 * look_rate,
  `Share rate (%)` = 100 * share_rate,
  `VAI (%)` = 100 * VAI
)]

write_tex_table(
  tex_tbl,
  path = file.path(paths$tables, "main_family_action_grid.tex"),
  caption = paste0(
    "Attention and engagement by content family in the three-button regime (Jul. 2020 -- Dec. 2024). ",
    "Rates are means of article-level counts divided by reads; VAI = like / (like + zaikan). ",
    sim_note
  ),
  label = "tab:family-action-grid",
  digits = c(Posts = 0L, `Median reads` = 0L, `Like rate (%)` = 2L, `Zaikan rate (%)` = 2L,
             `Share rate (%)` = 2L, `VAI (%)` = 1L),
  align = "lrrrrrr"
)

# --- Within account-day contrasts (same audience, different content) ---------
#
# For each outcome, demean by account-day and regress on family dummies with
# hard propaganda as the omitted category. Only account-days that publish at
# least two different families contribute identifying variation.

post[, account_day := paste(account_id, publish_date, sep = "_")]
post[, n_families_day := data.table::uniqueN(content_family), by = account_day]
mixed <- post[n_families_day >= 2]

within_contrast <- function(d, outcome, label) {
  loc <- d[is.finite(get(outcome)), .(y = get(outcome), content_family, account_id, account_day)]
  loc[, `:=`(
    d_service = as.numeric(content_family == "public_service"),
    d_soft = as.numeric(content_family == "soft_propaganda"),
    d_state = as.numeric(content_family == "state_governance")
  )]
  demean_cols <- c("y", "d_service", "d_soft", "d_state")
  loc[, (demean_cols) := lapply(.SD, function(v) v - mean(v)), .SDcols = demean_cols, by = account_day]
  X <- as.matrix(loc[, .(d_service, d_soft, d_state)])
  y <- loc$y
  XtX_inv <- solve(crossprod(X))
  beta <- drop(XtX_inv %*% crossprod(X, y))
  e <- drop(y - X %*% beta)
  # Cluster-robust meat: sum over accounts of (X_g' e_g)(X_g' e_g)'
  score <- X * e
  score_g <- rowsum(score, loc$account_id)
  meat <- crossprod(score_g)
  G <- nrow(score_g); n <- length(y); k <- ncol(X)
  V <- XtX_inv %*% meat %*% XtX_inv * (G / (G - 1)) * ((n - 1) / (n - k))
  se <- sqrt(diag(V))
  pval <- 2 * stats::pnorm(-abs(beta / se))
  data.table::data.table(
    Outcome = label,
    Contrast = c("Public service vs. hard propaganda",
                 "Soft propaganda vs. hard propaganda",
                 "State governance vs. hard propaganda"),
    Estimate = beta,
    SE = se,
    `p-value` = pval,
    `Account-days` = data.table::uniqueN(loc$account_day),
    Posts = n
  )
}

contrast_rows <- data.table::rbindlist(list(
  within_contrast(mixed, "log_reads", "log(1 + reads)"),
  within_contrast(mixed, "like_rate", "Like rate"),
  within_contrast(mixed, "look_rate", "Zaikan rate"),
  within_contrast(mixed, "share_rate", "Share rate")
))

write_tex_table(
  contrast_rows,
  path = file.path(paths$tables, "main_family_within_account_day.tex"),
  caption = paste0(
    "Within account-day contrasts in the three-button regime. Each outcome is demeaned by account-day ",
    "and regressed on family indicators (hard propaganda omitted), so the comparison is between posts ",
    "published by the same account on the same day. Standard errors clustered by account. ",
    sim_note
  ),
  label = "tab:family-within-account-day",
  digits = c(Estimate = 4L, SE = 4L, `p-value` = 3L, `Account-days` = 0L, Posts = 0L),
  align = "llrrrrr"
)

message("Saved family x action grid figure, summary table, and within-account-day contrasts.")
