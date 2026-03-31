bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

cut_like_to_haokan <- as.Date("2018-12-21")
cut_haokan_to_zaikan <- as.Date("2019-03-15")
cut_like_return  <- as.Date("2020-07-01")

dt[, year_month := as.Date(format(publish_date, "%Y-%m-01"))]

monthly_like <- dt[
  publish_date < cut_like_to_haokan | publish_date >= cut_like_return,
  .(value = mean(like_rate, na.rm = TRUE)),
  by = year_month
]
monthly_like[, metric := "Like Rate (Dianzan)"]
monthly_like[, segment := ifelse(year_month < cut_like_to_haokan, "pre", "post")]

monthly_look <- dt[
  publish_date >= as.Date("2019-01-01"),
  .(value = mean(look_rate, na.rm = TRUE)),
  by = year_month
]
monthly_look[, metric := "Haokan / Zaikan Rate"]
monthly_look[, segment := "all"]

monthly_share <- dt[, .(value = mean(share_rate, na.rm = TRUE)), by = year_month]
monthly_share[, metric := "Share Rate"]
monthly_share[, segment := "all"]

metric_levels <- c(
  "Like Rate (Dianzan)",
  "Haokan / Zaikan Rate (renamed Mar 2019)",
  "Share / Forward Rate"
)
monthly_look[, metric := "Haokan / Zaikan Rate (renamed Mar 2019)"]
monthly_share[, metric := "Share / Forward Rate"]

plot_dt <- data.table::rbindlist(list(monthly_like, monthly_look, monthly_share))
plot_dt[, metric := factor(metric, levels = metric_levels)]

vline_dates <- as.Date(c("2018-12-21", "2020-07-01"))

like_ymin <- min(monthly_like$value)
like_ymax <- max(monthly_like$value)
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

metric_colors <- c(
  "Like Rate (Dianzan)"                      = "#2563EB",
  "Haokan / Zaikan Rate (renamed Mar 2019)"  = "#059669",
  "Share / Forward Rate"                     = "#D97706"
)

year_breaks <- seq(
  from = as.Date("2014-01-01"),
  to   = as.Date("2025-01-01"),
  by   = "1 year"
)

p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = year_month, y = value, color = metric, group = interaction(metric, segment))) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_vline(
    xintercept = vline_dates,
    linetype   = "dashed",
    color      = "grey40",
    linewidth  = 0.5
  ) +
  ggplot2::geom_text(
    data        = label_df,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust       = 0.5,
    size        = 3.2,
    color       = "grey20",
    lineheight  = 0.9,
    fontface    = "bold"
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
  ggplot2::scale_color_manual(values = metric_colors) +
  ggplot2::scale_x_date(
    breaks = year_breaks,
    labels = scales::label_date("%Y"),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.01)) +
  ggplot2::labs(
    x = NULL,
    y = "Monthly mean engagement rate (metric / reads)"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    legend.position  = "none",
    strip.text       = ggplot2::element_text(face = "bold", size = 10),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing    = ggplot2::unit(0.8, "lines"),
    plot.margin      = ggplot2::margin(t = 4, r = 8, b = 8, l = 8)
  )

output_path <- file.path(paths$figures, "main_engagement_timeseries.pdf")

ggplot2::ggsave(
  filename = output_path,
  plot     = p,
  width    = 9,
  height   = 8,
  units    = "in",
  device   = "pdf"
)

message("Saved engagement time-series figure to: ", output_path)
