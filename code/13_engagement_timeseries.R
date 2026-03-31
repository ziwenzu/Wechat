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
monthly_look[, metric := "Haokan / Zaikan Rate (renamed Mar 2019)"]
monthly_share[, metric := "Share / Forward Rate"]

plot_dt <- data.table::rbindlist(list(monthly_like, monthly_look, monthly_share))
plot_dt[, metric := factor(metric, levels = metric_levels)]

vline_dates <- as.Date(c("2018-12-21", "2020-07-01"))

like_ymin <- min(monthly_like$value)
like_ymax <- max(monthly_like$value)
like_yspan <- like_ymax - like_ymin

first_panel <- factor("Like Rate (Dianzan)", levels = metric_levels)
arrow_span_days <- 120L

label_df <- data.frame(
  x = as.Date(c("2017-06-01", "2022-01-01")),
  y = c(
    like_ymax - 0.15 * like_yspan,
    like_ymax - 0.15 * like_yspan
  ),
  label = c(
    "Dec 2018: Like replaced\nby Haokan",
    "Jul 2020: Footer becomes\nLike + Zaikan + Share/Forward\n(current layout)"
  ),
  metric = first_panel,
  stringsAsFactors = FALSE
)

arrow_df <- data.frame(
  xend = as.Date(c("2018-12-10", "2020-07-15")),
  y    = label_df$y,
  yend = label_df$y,
  metric = first_panel,
  stringsAsFactors = FALSE
)
arrow_df$x <- as.Date(c(
  arrow_df$xend[1] - arrow_span_days,
  arrow_df$xend[2] + arrow_span_days
), origin = "1970-01-01")

year_breaks <- seq(
  from = as.Date("2014-01-01"),
  to   = as.Date("2025-01-01"),
  by   = "1 year"
)

p <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = year_month, y = value, group = interaction(metric, segment))) +
  ggplot2::geom_line(linewidth = 0.65, color = "#1F1F1F", lineend = "round") +
  ggplot2::geom_vline(
    xintercept = vline_dates,
    linetype   = "22",
    color      = "grey45",
    linewidth  = 0.45
  ) +
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
  ggplot2::scale_x_date(
    breaks = year_breaks,
    labels = scales::label_date("%Y"),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  ggplot2::labs(
    x = NULL,
    y = "Monthly mean engagement rate (metric / reads)"
  ) +
  ts_theme

output_path <- file.path(paths$figures, "main_engagement_timeseries.pdf")

save_figure(
  path = output_path,
  plot = p,
  width = 9,
  height = 8,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)

message("Saved engagement time-series figure to: ", output_path)
