bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

cut_like_to_haokan <- as.Date("2018-12-21")
cut_like_return    <- as.Date("2020-07-01")

dt[, regime := "Pre-reform (before Dec 2018)"]
dt[publish_date >= cut_like_to_haokan & publish_date < cut_like_return,
   regime := "Haokan/Zaikan only (Dec 2018 -- Jun 2020)"]
dt[publish_date >= cut_like_return,
   regime := "Like + Zaikan + Share (Jul 2020 --)"]
dt[, regime := factor(regime, levels = c(
  "Pre-reform (before Dec 2018)",
  "Haokan/Zaikan only (Dec 2018 -- Jun 2020)",
  "Like + Zaikan + Share (Jul 2020 --)"
))]

family_gloss <- c(
  "public_service"   = "Public Service",
  "soft_propaganda"  = "Soft Propaganda",
  "state_governance" = "State Governance",
  "hard_propaganda"  = "Hard Propaganda"
)
family_order <- c("public_service", "soft_propaganda", "state_governance", "hard_propaganda")
dt[, family := factor(unname(family_gloss[content_family]),
                      levels = unname(family_gloss[family_order]))]

compute_margins <- function(sub_dt, regime_label) {
  like_avail <- sub_dt[like_rate >= 0 & !is.na(like_rate)]
  if (regime_label == "Haokan/Zaikan only (Dec 2018 -- Jun 2020)") {
    like_avail <- sub_dt[0]
  }
  look_avail <- sub_dt[look_rate >= 0 & !is.na(look_rate)]
  if (regime_label == "Pre-reform (before Dec 2018)") {
    look_avail <- sub_dt[0]
  }

  like_ext <- if (nrow(like_avail) > 0) 100 * mean(like_avail$like_num > 0) else NA_real_
  like_int <- if (nrow(like_avail[like_num > 0]) > 0) 100 * mean(like_avail[like_num > 0]$like_rate) else NA_real_

  look_ext <- if (nrow(look_avail) > 0) 100 * mean(look_avail$look_num > 0) else NA_real_
  look_int <- if (nrow(look_avail[look_num > 0]) > 0) 100 * mean(look_avail[look_num > 0]$look_rate) else NA_real_

  share_ext <- 100 * mean(sub_dt$share_num > 0)
  share_int <- if (nrow(sub_dt[share_num > 0]) > 0) 100 * mean(sub_dt[share_num > 0]$share_rate) else NA_real_

  data.table::data.table(
    `Like Ext.` = like_ext,
    `Like Int.` = like_int,
    `Zaikan Ext.` = look_ext,
    `Zaikan Int.` = look_int,
    `Share Ext.` = share_ext,
    `Share Int.` = share_int,
    N = as.integer(nrow(sub_dt))
  )
}

regime_rows <- data.table::rbindlist(lapply(levels(dt$regime), function(r) {
  sub <- dt[regime == r]
  row <- compute_margins(sub, r)
  row[, Regime := r]
  row
}))
regime_rows <- regime_rows[, c("Regime", setdiff(names(regime_rows), "Regime")), with = FALSE]

write_tex_table(
  regime_rows,
  file.path(paths$tables, "main_extensive_intensive_by_regime.tex"),
  caption = "Extensive and intensive margins of engagement by interface regime. All values in percent. Extensive: share of posts with any positive count. Intensive: mean rate conditional on positive count.",
  label = "tab:extensive-intensive-regime",
  align = "lrrrrrrr",
  digits = c(`Like Ext.` = 1L, `Like Int.` = 1L, `Zaikan Ext.` = 1L, `Zaikan Int.` = 1L, `Share Ext.` = 1L, `Share Int.` = 1L, N = 0L)
)

family_regime_rows <- data.table::rbindlist(lapply(levels(dt$regime), function(r) {
  data.table::rbindlist(lapply(levels(dt$family), function(f) {
    sub <- dt[regime == r & family == f]
    if (nrow(sub) == 0) return(NULL)
    row <- compute_margins(sub, r)
    row[, Regime := r]
    row[, Family := f]
    row
  }))
}))
family_regime_rows <- family_regime_rows[,
  c("Regime", "Family", setdiff(names(family_regime_rows), c("Regime", "Family"))),
  with = FALSE
]

write_tex_table(
  family_regime_rows,
  file.path(paths$tables, "main_extensive_intensive_by_regime_family.tex"),
  caption = "Extensive and intensive margins of engagement by interface regime and content family. All values in percent.",
  label = "tab:extensive-intensive-regime-family",
  align = "llrrrrrrr",
  digits = c(`Like Ext.` = 1L, `Like Int.` = 1L, `Zaikan Ext.` = 1L, `Zaikan Int.` = 1L, `Share Ext.` = 1L, `Share Int.` = 1L, N = 0L)
)

monthly_margins <- dt[, {
  r <- regime[1]
  like_ok <- if (r == "Haokan/Zaikan only (Dec 2018 -- Jun 2020)") FALSE else TRUE
    look_ok <- if (r == "Pre-reform (before Dec 2018)") FALSE else TRUE
    look_start_ok <- year_month >= as.Date("2019-01-01")
  .(
    like_ext  = if (like_ok) mean(like_num > 0) else NA_real_,
    look_ext  = if (look_ok && look_start_ok) mean(look_num > 0) else NA_real_,
    share_ext = mean(share_num > 0)
  )
}, by = .(year_month = as.Date(format(publish_date, "%Y-%m-01")), regime)]

plot_long <- data.table::melt(
  monthly_margins,
  id.vars     = c("year_month", "regime"),
  measure.vars = c("like_ext", "look_ext", "share_ext"),
  variable.name = "metric",
  value.name    = "value"
)
metric_levels <- c(
  "Like Rate (Dianzan)",
  "Haokan / Zaikan Rate (renamed Mar 2019)",
  "Share / Forward Rate"
)

ts_theme <- ggplot2::theme_classic(base_size = 11, base_family = "serif") +
  ggplot2::theme(
    legend.position = "none",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 10.5, face = "plain", margin = ggplot2::margin(b = 3)),
    panel.spacing = ggplot2::unit(1, "lines"),
    axis.text = ggplot2::element_text(size = 9.5, color = "#303030"),
    axis.title = ggplot2::element_text(size = 10.5, color = "#1A1A1A"),
    axis.line = ggplot2::element_line(linewidth = 0.4, color = "#303030"),
    axis.ticks = ggplot2::element_line(linewidth = 0.4, color = "#303030"),
    axis.ticks.length = ggplot2::unit(0.12, "cm"),
    plot.margin = ggplot2::margin(t = 4, r = 10, b = 8, l = 8)
  )

plot_long <- plot_long[!is.na(value)]
plot_long[, metric := factor(metric,
  levels = c("like_ext", "look_ext", "share_ext"),
  labels = metric_levels
)]

plot_long[, segment := "all"]
plot_long[metric == "Like Rate (Dianzan)",
  segment := ifelse(year_month < cut_like_to_haokan, "pre", "post")]

vline_dates <- as.Date(c("2018-12-21", "2020-07-01"))

like_ymin <- min(plot_long[metric == "Like Rate (Dianzan)"]$value)
like_ymax <- max(plot_long[metric == "Like Rate (Dianzan)"]$value)
like_yspan <- like_ymax - like_ymin

first_panel <- factor("Like Rate (Dianzan)", levels = metric_levels)

label_df <- data.frame(
  x = as.Date(c("2017-06-01", "2022-01-01")),
  y = c(
    like_ymax - 0.15 * like_yspan,
    like_ymax - 0.95 * like_yspan
  ),
  label = c(
    "Dec 2018: Like replaced\nby Haokan",
    "Jul 2020: Footer becomes\nLike + Zaikan + Share/Forward\n(current layout)"
  ),
  metric = first_panel,
  stringsAsFactors = FALSE
)

arrow_df <- data.frame(
  x    = as.Date(c("2018-08-01", "2020-10-01")),
  xend = as.Date(c("2018-12-10", "2020-07-15")),
  y    = label_df$y,
  yend = label_df$y,
  metric = first_panel,
  stringsAsFactors = FALSE
)

year_breaks <- seq(as.Date("2014-01-01"), as.Date("2025-01-01"), by = "1 year")

p <- ggplot2::ggplot(
  plot_long,
  ggplot2::aes(x = year_month, y = value,
               group = interaction(metric, segment))
) +
  ggplot2::geom_line(linewidth = 0.65, color = "#1F1F1F", lineend = "round") +
  ggplot2::geom_vline(xintercept = vline_dates, linetype = "22",
                      color = "grey45", linewidth = 0.45) +
  ggplot2::geom_text(
    data        = label_df,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust       = 0.5,
    size        = 3.1,
    color       = "grey20",
    lineheight  = 0.9,
    family      = "serif"
  ) +
  ggplot2::geom_segment(
    data        = arrow_df,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color       = "grey40",
    linewidth   = 0.4,
    arrow       = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")
  ) +
  ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_date(breaks = year_breaks, labels = scales::label_date("%Y"),
                        expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  ggplot2::labs(
    x = NULL,
    y = "Share of posts with any positive count (extensive margin)"
  ) +
  ts_theme

ggplot2::ggsave(
  filename = file.path(paths$figures, "main_extensive_margin_timeseries.pdf"),
  plot     = p,
  width    = 9,
  height   = 8,
  units    = "in",
  device   = "pdf",
  bg       = "white",
  useDingbats = FALSE
)

message("Saved extensive/intensive tables and figure.")
