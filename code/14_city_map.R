bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "sf", "ggspatial", "scales"))

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)

map_dir <- file.path(paths$data, "map")
pref_shp <- file.path(map_dir, "cn_admin_2021", "prefecture.shp")
mini_shp <- file.path(map_dir, "chinaprov2019mini", "chinaprov2019mini.shp")
mini_line_shp <- file.path(map_dir, "chinaprov2019mini", "chinaprov2019mini_line.shp")

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

city_summary <- dt[, .(
  posts      = .N,
  mean_reads = mean(read_num, na.rm = TRUE)
), by = .(city, province)]

provmap <- sf::read_sf(mini_shp)
provmap <- provmap[!is.na(provmap[["\u7701\u4ee3\u7801"]]), ]

provlinemap <- sf::read_sf(mini_line_shp)
provlinemap <- provlinemap[provlinemap$class %in%
  c("\u4e5d\u6bb5\u7ebf", "\u6d77\u5cb8\u7ebf", "\u5c0f\u5730\u56fe\u6846\u683c"), ]

line_class_color <- c("\u4e5d\u6bb5\u7ebf" = "#A29AC4",
                      "\u6d77\u5cb8\u7ebf" = "#0055AA",
                      "\u5c0f\u5730\u56fe\u6846\u683c" = "black")
line_class_lwd <- c("\u4e5d\u6bb5\u7ebf" = 0.6,
                    "\u6d77\u5cb8\u7ebf" = 0.3,
                    "\u5c0f\u5730\u56fe\u6846\u683c" = 0.3)

prefmap <- sf::read_sf(pref_shp)

city_summary[, city_code := NA_character_]
for (i in seq_len(nrow(city_summary))) {
  cn <- city_summary$city[i]
  idx <- grep(cn, prefmap[["\u5e02"]], fixed = TRUE)
  if (length(idx) > 0) city_summary$city_code[i] <- prefmap[["\u5e02\u4ee3\u7801"]][idx[1]]
}

prefmap_merged <- merge(prefmap, city_summary, by.x = "\u5e02\u4ee3\u7801", by.y = "city_code", all.x = TRUE)
prefmap_merged$posts_k <- prefmap_merged$posts / 1000

make_city_map <- function(fill_var, fill_label, palette_opt = "viridis", palette_dir = 1) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = provmap, fill = "grey95", color = "grey70", linewidth = 0.3) +
    ggplot2::geom_sf(data = prefmap_merged[!is.na(prefmap_merged[[fill_var]]), ],
                     ggplot2::aes(fill = .data[[fill_var]]),
                     color = "grey80", linewidth = 0.08) +
    ggplot2::geom_sf(data = provlinemap,
                     ggplot2::aes(color = class, linewidth = class),
                     show.legend = FALSE) +
    ggplot2::scale_color_manual(values = line_class_color) +
    ggplot2::scale_linewidth_manual(values = line_class_lwd) +
    ggplot2::scale_fill_viridis_c(
      name = fill_label, option = palette_opt, direction = palette_dir,
      na.value = "grey95",
      labels = scales::label_number(big.mark = ",")
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "false", pad_y = ggplot2::unit(0.1, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering()
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      legend.position.inside = c(0.13, 0.22),
      legend.position = "inside",
      legend.key.height = ggplot2::unit(0.35, "cm"),
      legend.key.width = ggplot2::unit(0.8, "cm"),
      legend.text = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_text(size = 9, face = "bold"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

save_figure(
  path = file.path(paths$figures, "main_city_map_posts.png"),
  plot = make_city_map("posts_k", "Posts (K)", "magma", -1),
  width = 8.5,
  height = 7,
  units = "in",
  dpi = 300,
  allow_png = TRUE
)

save_figure(
  path = file.path(paths$figures, "main_city_map_reads.png"),
  plot = make_city_map("mean_reads", "Mean Reads", "viridis", 1),
  width = 8.5,
  height = 7,
  units = "in",
  dpi = 300,
  allow_png = TRUE
)

message("Saved city-level maps.")
