bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "sf", "ggspatial", "scales", "cowplot"))

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)

map_dir <- file.path(paths$data, "map", "chinaprov2019mini")

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

province_summary <- dt[
  ,
  .(
    accounts = data.table::uniqueN(account_id),
    posts    = .N
  ),
  by = .("\u7701" = province)
]

provmap <- sf::read_sf(file.path(map_dir, "chinaprov2019mini.shp"))
provmap <- provmap[!is.na(provmap[["\u7701\u4ee3\u7801"]]), ]

provlinemap <- sf::read_sf(file.path(map_dir, "chinaprov2019mini_line.shp"))
provlinemap <- provlinemap[provlinemap$class %in%
  c("\u4e5d\u6bb5\u7ebf", "\u6d77\u5cb8\u7ebf", "\u5c0f\u5730\u56fe\u6846\u683c"), ]

province_en <- c(
  "\u5317\u4eac\u5e02" = "Beijing", "\u5929\u6d25\u5e02" = "Tianjin",
  "\u6cb3\u5317\u7701" = "Hebei", "\u5c71\u897f\u7701" = "Shanxi",
  "\u5185\u8499\u53e4\u81ea\u6cbb\u533a" = "Inner Mongolia",
  "\u8fbd\u5b81\u7701" = "Liaoning", "\u5409\u6797\u7701" = "Jilin",
  "\u9ed1\u9f99\u6c5f\u7701" = "Heilongjiang",
  "\u4e0a\u6d77\u5e02" = "Shanghai", "\u6c5f\u82cf\u7701" = "Jiangsu",
  "\u6d59\u6c5f\u7701" = "Zhejiang", "\u5b89\u5fbd\u7701" = "Anhui",
  "\u798f\u5efa\u7701" = "Fujian", "\u6c5f\u897f\u7701" = "Jiangxi",
  "\u5c71\u4e1c\u7701" = "Shandong", "\u6cb3\u5357\u7701" = "Henan",
  "\u6e56\u5317\u7701" = "Hubei", "\u6e56\u5357\u7701" = "Hunan",
  "\u5e7f\u4e1c\u7701" = "Guangdong",
  "\u5e7f\u897f\u58ee\u65cf\u81ea\u6cbb\u533a" = "Guangxi",
  "\u6d77\u5357\u7701" = "Hainan", "\u91cd\u5e86\u5e02" = "Chongqing",
  "\u56db\u5ddd\u7701" = "Sichuan", "\u8d35\u5dde\u7701" = "Guizhou",
  "\u4e91\u5357\u7701" = "Yunnan",
  "\u897f\u85cf\u81ea\u6cbb\u533a" = "Tibet",
  "\u9655\u897f\u7701" = "Shaanxi", "\u7518\u8083\u7701" = "Gansu",
  "\u9752\u6d77\u7701" = "Qinghai",
  "\u5b81\u590f\u56de\u65cf\u81ea\u6cbb\u533a" = "Ningxia",
  "\u65b0\u7586\u7ef4\u543e\u5c14\u81ea\u6cbb\u533a" = "Xinjiang",
  "\u53f0\u6e7e\u7701" = "Taiwan",
  "\u9999\u6e2f\u7279\u522b\u884c\u653f\u533a" = "Hong Kong",
  "\u6fb3\u95e8\u7279\u522b\u884c\u653f\u533a" = "Macao",
  "\u4e2d\u671d\u5171\u6709" = ""
)

provmap$province_en <- unname(province_en[provmap[["\u7701"]]])

provmap_merged <- merge(provmap, province_summary,
                        by = "\u7701", all.x = TRUE)

line_class_color <- c(
  "\u4e5d\u6bb5\u7ebf"     = "#A29AC4",
  "\u6d77\u5cb8\u7ebf"     = "#0055AA",
  "\u5c0f\u5730\u56fe\u6846\u683c" = "black"
)
line_class_lwd <- c(
  "\u4e5d\u6bb5\u7ebf"     = 0.6,
  "\u6d77\u5cb8\u7ebf"     = 0.3,
  "\u5c0f\u5730\u56fe\u6846\u683c" = 0.3
)

provmap_merged$posts_k <- provmap_merged$posts / 1000

make_map <- function(fill_var, fill_label, fill_breaks, fill_labels,
                     palette_option = "viridis", palette_dir = 1) {
  ggplot2::ggplot(provmap_merged) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[fill_var]]),
      color = "grey60", linewidth = 0.15
    ) +
    ggplot2::geom_sf(
      data = provlinemap,
      ggplot2::aes(color = class, linewidth = class),
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = line_class_color) +
    ggplot2::scale_linewidth_manual(values = line_class_lwd) +
    ggplot2::scale_fill_viridis_c(
      name      = fill_label,
      option    = palette_option,
      direction = palette_dir,
      na.value  = "grey90",
      breaks    = fill_breaks,
      labels    = fill_labels
    ) +
    ggspatial::annotation_scale(
      location   = "bl",
      width_hint = 0.25,
      text_cex   = 0.7
    ) +
    ggspatial::annotation_north_arrow(
      location    = "tr",
      which_north = "false",
      pad_y       = ggplot2::unit(0.1, "cm"),
      style       = ggspatial::north_arrow_fancy_orienteering()
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      legend.position.inside = c(0.13, 0.22),
      legend.position = "inside",
      legend.key.height = ggplot2::unit(0.35, "cm"),
      legend.key.width  = ggplot2::unit(0.8, "cm"),
      legend.text  = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_text(size = 9, face = "bold"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

acct_range <- range(provmap_merged$accounts, na.rm = TRUE)
acct_breaks <- pretty(acct_range, n = 5)

posts_k_range <- range(provmap_merged$posts_k, na.rm = TRUE)
posts_k_breaks <- pretty(posts_k_range, n = 5)

p_accounts <- make_map(
  "accounts", "Accounts",
  fill_breaks = acct_breaks,
  fill_labels = format(acct_breaks, big.mark = ",", trim = TRUE),
  palette_option = "viridis", palette_dir = 1
)

p_posts <- make_map(
  "posts_k", "Posts (thousands)",
  fill_breaks = posts_k_breaks,
  fill_labels = format(posts_k_breaks, big.mark = ",", trim = TRUE),
  palette_option = "magma", palette_dir = -1
)

ggplot2::ggsave(
  filename = file.path(paths$figures, "main_province_map_accounts.pdf"),
  plot     = p_accounts,
  width    = 8.5,
  height   = 7,
  units    = "in",
  device   = "pdf"
)

ggplot2::ggsave(
  filename = file.path(paths$figures, "main_province_map_posts.pdf"),
  plot     = p_posts,
  width    = 8.5,
  height   = 7,
  units    = "in",
  device   = "pdf"
)

message("Saved province maps to figures/")
