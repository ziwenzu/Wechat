bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "ggplot2", "scales", "cowplot", "stringi"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)
memo_dir <- file.path(paths$root, "memo")
fs::dir_create(memo_dir, recurse = TRUE)

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))

province_name_map <- c(
  "北京市" = "Beijing",
  "天津市" = "Tianjin",
  "河北省" = "Hebei",
  "山西省" = "Shanxi",
  "内蒙古自治区" = "Inner Mongolia",
  "辽宁省" = "Liaoning",
  "吉林省" = "Jilin",
  "黑龙江省" = "Heilongjiang",
  "上海市" = "Shanghai",
  "江苏省" = "Jiangsu",
  "浙江省" = "Zhejiang",
  "安徽省" = "Anhui",
  "福建省" = "Fujian",
  "江西省" = "Jiangxi",
  "山东省" = "Shandong",
  "河南省" = "Henan",
  "湖北省" = "Hubei",
  "湖南省" = "Hunan",
  "广东省" = "Guangdong",
  "广西壮族自治区" = "Guangxi",
  "海南省" = "Hainan",
  "重庆市" = "Chongqing",
  "四川省" = "Sichuan",
  "贵州省" = "Guizhou",
  "云南省" = "Yunnan",
  "西藏自治区" = "Tibet",
  "陕西省" = "Shaanxi",
  "甘肃省" = "Gansu",
  "青海省" = "Qinghai",
  "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang"
)

city_name_map <- c(
  "北京市" = "Beijing",
  "上海市" = "Shanghai",
  "南京市" = "Nanjing",
  "杭州市" = "Hangzhou",
  "成都市" = "Chengdu",
  "扬州市" = "Yangzhou",
  "宁波市" = "Ningbo",
  "佛山市" = "Foshan",
  "合肥市" = "Hefei",
  "西安市" = "Xi'an",
  "东莞市" = "Dongguan",
  "大连市" = "Dalian",
  "深圳市" = "Shenzhen",
  "无锡市" = "Wuxi",
  "南通市" = "Nantong",
  "衢州市" = "Quzhou",
  "连云港市" = "Lianyungang",
  "宜昌市" = "Yichang",
  "绍兴市" = "Shaoxing",
  "平顶山市" = "Pingdingshan",
  "重庆市" = "Chongqing",
  "天津市" = "Tianjin",
  "广州市" = "Guangzhou",
  "苏州市" = "Suzhou",
  "青岛市" = "Qingdao",
  "武汉市" = "Wuhan",
  "郑州市" = "Zhengzhou",
  "厦门市" = "Xiamen",
  "长沙市" = "Changsha"
)

normalize_spacing <- function(x) {
  trimws(gsub("\\s+", " ", x))
}

collapse_pinyin <- function(x) {
  x <- normalize_spacing(x)

  if (!nzchar(x)) {
    return("")
  }

  tokens <- strsplit(x, "\\s+")[[1]]
  collapsed <- tokens[[1]]

  if (length(tokens) > 1L) {
    for (token in tokens[-1]) {
      if (grepl("^[aeo]", token)) {
        collapsed <- paste0(collapsed, "'", token)
      } else {
        collapsed <- paste0(collapsed, token)
      }
    }
  }

  tools::toTitleCase(collapsed)
}

romanize_place <- function(x) {
  latin <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
  normalize_spacing(tolower(latin))
}

province_to_english <- function(x) {
  out <- unname(province_name_map[x])
  missing_idx <- is.na(out)

  if (any(missing_idx)) {
    out[missing_idx] <- vapply(
      x[missing_idx],
      city_to_english_one,
      character(1)
    )
  }

  out
}

city_to_english_one <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }

  if (x %in% names(city_name_map)) {
    return(unname(city_name_map[[x]]))
  }

  latin <- romanize_place(x)
  latin <- sub(" zi zhi zhou$", "", latin)
  latin <- sub(" di qu$", "", latin)
  latin <- sub(" meng$", "", latin)
  latin <- sub(" zhou$", "", latin)
  latin <- sub(" shi$", "", latin)
  collapse_pinyin(latin)
}

city_to_english <- function(x) {
  vapply(x, city_to_english_one, character(1))
}

family_gloss <- c(
  "public_service" = "Public Service",
  "soft_propaganda" = "Soft Propaganda",
  "state_governance" = "State Governance",
  "hard_propaganda" = "Hard Propaganda"
)
family_order <- c("public_service", "soft_propaganda", "state_governance", "hard_propaganda")
family_colors <- c(
  "Public Service" = "#0f6b6f",
  "Soft Propaganda" = "#b45309",
  "State Governance" = "#4b5563",
  "Hard Propaganda" = "#9f1d20"
)

fmt_int <- function(x) {
  format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)
}

dt[, year := as.integer(year)]
dt[, family := unname(family_gloss[content_family])]
dt[, family := factor(family, levels = unname(family_gloss[family_order]))]

year_summary <- dt[
  ,
  .(
    `Posts (M)` = .N / 1e6,
    `Mean Reads` = mean(read_num),
    `Median Reads` = stats::median(read_num),
    `One-Click (%)` = 100 * mean((like_num + look_num) / read_num),
    `Like/Read (%)` = 100 * mean(like_num / read_num),
    `Zaikan/Read (%)` = 100 * mean(look_num / read_num),
    `Share/Read (%)` = 100 * mean(share_num / read_num),
    `10w+ (%)` = 100 * mean(read_num >= 100001)
  ),
  by = year
]
data.table::setorder(year_summary, year)
year_summary[, year := as.character(year)]

write_tex_table(
  year_summary,
  file.path(paths$tables, "main_time_summary.tex"),
  caption = "Year-level descriptive summary of posting volume and engagement.",
  label = "tab:main-time-summary",
  digits = c(
    `Posts (M)` = 2L,
    `Mean Reads` = 0L,
    `Median Reads` = 0L,
    `One-Click (%)` = 2L,
    `Like/Read (%)` = 2L,
    `Zaikan/Read (%)` = 2L,
    `Share/Read (%)` = 2L,
    `10w+ (%)` = 3L
  )
)

year_family <- dt[
  ,
  .(
    posts = .N,
    mean_reads = mean(read_num)
  ),
  by = .(year, family)
]
year_totals <- year_family[, .(year_posts = sum(posts)), by = year]
year_family <- year_totals[year_family, on = "year"]
year_family[, share_pct := 100 * posts / year_posts]

plot_time_share <- ggplot2::ggplot(
  year_family,
  ggplot2::aes(x = year, y = share_pct, color = family)
) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_color_manual(values = family_colors) +
  ggplot2::scale_x_continuous(breaks = sort(unique(year_family$year))) +
  ggplot2::labs(
    x = NULL,
    y = "Share of annual posts (%)",
    color = NULL,
    title = "Content-family composition over time"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

plot_time_reads <- ggplot2::ggplot(
  year_family,
  ggplot2::aes(x = year, y = mean_reads, color = family)
) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_color_manual(values = family_colors) +
  ggplot2::scale_x_continuous(breaks = sort(unique(year_family$year))) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  ggplot2::labs(
    x = NULL,
    y = "Mean reads",
    color = NULL,
    title = "Average readership by family over time"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(paths$figures, "main_time_trends.pdf"),
  plot = cowplot::plot_grid(plot_time_share, plot_time_reads, ncol = 1, align = "v", rel_heights = c(1, 1)),
  width = 8.5,
  height = 8.5,
  units = "in",
  device = "pdf"
)

province_summary <- dt[
  ,
  .(
    posts = .N,
    mean_reads = mean(read_num)
  ),
  by = province
]
province_summary[, post_share_pct := 100 * posts / nrow(dt)]
province_summary[, province_en := province_to_english(province)]
data.table::setorder(province_summary, -posts)

city_summary <- dt[
  ,
  .(
    posts = .N,
    mean_reads = mean(read_num)
  ),
  by = .(account_id, province, city)
]
city_summary[, post_share_pct := 100 * posts / nrow(dt)]
city_summary[, province_en := province_to_english(province)]
city_summary[, city_en := city_to_english(city)]
data.table::setorder(city_summary, -posts)
city_summary[, city_label := paste0(city_en, " (", province_en, ")")]

space_summary <- data.table::data.table(
  Statistic = c(
    "Province-level units covered",
    "Prefecture-level cities covered",
    "Posts per city: mean",
    "Posts per city: median",
    "Posts per city: sd",
    "Posts per city: min",
    "Posts per city: max"
  ),
  Value = c(
    length(unique(dt$province)),
    data.table::uniqueN(dt$account_id),
    mean(city_summary$posts),
    stats::median(city_summary$posts),
    stats::sd(city_summary$posts),
    min(city_summary$posts),
    max(city_summary$posts)
  )
)

write_tex_table(
  space_summary,
  file.path(paths$tables, "main_space_summary.tex"),
  caption = "Spatial coverage and posting concentration across provinces and prefecture-level cities.",
  label = "tab:main-space-summary",
  digits = c(Value = 0L)
)

memo_lines <- c(
  "# 2026-03-30 Time and Space Descriptives",
  "",
  "Outputs:",
  paste0("- ", file.path(paths$tables, "main_time_summary.tex")),
  paste0("- ", file.path(paths$tables, "main_space_summary.tex")),
  paste0("- ", file.path(paths$figures, "main_time_trends.pdf")),
  paste0("- ", file.path(paths$figures, "main_space_distribution.pdf")),
  "",
  "High-level takeaways:",
  paste0("- Time coverage runs from ", min(dt$year), " to ", max(dt$year), "."),
  paste0("- Spatial coverage includes ", length(unique(dt$province)), " provinces and ", data.table::uniqueN(dt$account_id), " prefecture-level government accounts."),
  paste0("- Largest province by corpus share: ", province_summary$province_en[1], "."),
  paste0("- Highest-mean-read city: ", city_summary$city_en[which.max(city_summary$mean_reads)], " (", city_summary$province_en[which.max(city_summary$mean_reads)], ").")
)

writeLines(
  memo_lines,
  con = file.path(memo_dir, "2026-03-30_time_space_descriptives.md"),
  useBytes = TRUE
)
