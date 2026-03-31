bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))
dt[, one_click_rate := like_rate + look_rate]
dt[, days_from_2018 := as.integer(publish_date - as.Date("2018-12-21"))]
dt[, days_from_2020 := as.integer(publish_date - as.Date("2020-07-01"))]

window <- 30L

make_event_study <- function(dt, running_var, cutoff_label, output_file) {
  sub <- dt[abs(get(running_var)) <= window]
  sub[, day := get(running_var)]

  daily <- sub[, .(mean_one_click = mean(one_click_rate, na.rm = TRUE)), by = day]
  data.table::setorder(daily, day)

  daily[, side := ifelse(day < 0, "pre", "post")]

  p <- ggplot2::ggplot(daily, ggplot2::aes(x = day, y = mean_one_click)) +
    ggplot2::geom_point(size = 1.4, color = "grey30", alpha = 0.8) +
    ggplot2::geom_smooth(
      data    = daily[side == "pre"],
      method  = "loess",
      formula = y ~ x,
      span    = 0.5,
      se      = TRUE,
      color   = "steelblue",
      fill    = "steelblue",
      alpha   = 0.15,
      linewidth = 0.8
    ) +
    ggplot2::geom_smooth(
      data    = daily[side == "post"],
      method  = "loess",
      formula = y ~ x,
      span    = 0.5,
      se      = TRUE,
      color   = "steelblue",
      fill    = "steelblue",
      alpha   = 0.15,
      linewidth = 0.8
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
    ggplot2::scale_x_continuous(breaks = seq(-window, window, by = 10)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.01)) +
    ggplot2::labs(
      x = paste0("Days from ", cutoff_label),
      y = "Mean one-click rate (like + zaikan) / reads"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey92"),
      axis.title       = ggplot2::element_text(size = 10.5),
      plot.margin      = ggplot2::margin(t = 6, r = 8, b = 6, l = 6)
    )

  save_figure(
    path = file.path(paths$figures, output_file),
    plot = p,
    width = 7,
    height = 4.5,
    units = "in",
    bg = "white",
    useDingbats = FALSE
  )

  message("Saved ", output_file)
}

make_event_study(dt, "days_from_2018", "Dec 21 2018 cutoff", "appendix_event_study_2018.pdf")
make_event_study(dt, "days_from_2020", "Jul 1 2020 cutoff",  "appendix_event_study_2020.pdf")

message("Done: 22_appendix_event_study.R")
