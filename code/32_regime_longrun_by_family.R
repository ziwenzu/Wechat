bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

# ---------------------------------------------------------------------------
# 32_regime_longrun_by_family.R
#
# Long-run, regime-level view of the visibility argument:
#   Panel A  monthly one-click rate (like + haokan/zaikan) by content family,
#            2015-2024, with the three interface regimes shaded.
#   Panel B  monthly visibility-avoidance index VAI = like / (like + zaikan)
#            by family in the three-button regime (Jul. 2020 onward).
# Tables: regime x family means; annual VAI by family.
#
# The local RDDs in scripts 16-28 are slices of this long-run structure at
# the two cutoffs; this script shows the structure persists for years.
#
# DATA STATUS: reads data/wechat_instructional_dataset.rds (SIMULATED
# engagement counts, see DATA_STATUS.md). Outputs are teaching material.
# ---------------------------------------------------------------------------

ensure_packages(c("data.table", "fs", "ggplot2", "scales"))

paths <- project_paths()
fs::dir_create(paths$figures, recurse = TRUE)
fs::dir_create(paths$tables, recurse = TRUE)
slide_output_dir <- file.path(paths$root, "slides", "figures")
fs::dir_create(slide_output_dir, recurse = TRUE)

sim_note <- "Simulated teaching data (see DATA_STATUS.md)."

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))
keep_cols <- c("publish_date", "content_family", "read_num", "like_rate", "look_rate", "share_rate")
dt <- dt[, ..keep_cols]

cut_like_to_haokan <- as.Date("2018-12-21")
cut_like_return <- as.Date("2020-07-01")

family_gloss <- c(
  "public_service"   = "Public service",
  "soft_propaganda"  = "Soft propaganda",
  "state_governance" = "State governance",
  "hard_propaganda"  = "Hard propaganda"
)
family_order <- names(family_gloss)

dt <- dt[read_num > 0 & content_family %in% family_order]
dt[, one_click_rate := like_rate + look_rate]
dt[, year_month := as.Date(format(publish_date, "%Y-%m-01"))]
dt[, year := as.integer(format(publish_date, "%Y"))]
dt[, regime := data.table::fifelse(
  publish_date < cut_like_to_haokan, "Like only (2015 - Dec. 2018)",
  data.table::fifelse(publish_date < cut_like_return,
                      "Haokan/Zaikan only (Dec. 2018 - Jun. 2020)",
                      "Like + Zaikan + Share (Jul. 2020 - 2024)")
)]
regime_levels <- c(
  "Like only (2015 - Dec. 2018)",
  "Haokan/Zaikan only (Dec. 2018 - Jun. 2020)",
  "Like + Zaikan + Share (Jul. 2020 - 2024)"
)
dt[, regime := factor(regime, levels = regime_levels)]

# --- Monthly series ----------------------------------------------------------

monthly <- dt[, .(
  n = .N,
  one_click = mean(one_click_rate, na.rm = TRUE),
  like = mean(like_rate, na.rm = TRUE),
  look = mean(look_rate, na.rm = TRUE)
), by = .(year_month, content_family)]
monthly[, vai := data.table::fifelse(year_month >= cut_like_return, like / (like + look), NA_real_)]
monthly[, family_label := factor(family_gloss[content_family], levels = unname(family_gloss))]

panel_a <- monthly[, .(year_month, family_label, value = one_click)]
panel_a[, panel := "A. One-click engagement rate (like + haokan/zaikan) / reads"]
panel_b <- monthly[!is.na(vai), .(year_month, family_label, value = vai)]
panel_b[, panel := "B. Visibility-avoidance index: like / (like + zaikan), three-button regime"]
plot_dt <- data.table::rbindlist(list(panel_a, panel_b))
plot_dt[, panel := factor(panel, levels = unique(panel))]

panel_a_label <- factor("A. One-click engagement rate (like + haokan/zaikan) / reads", levels = levels(plot_dt$panel))
shade <- data.frame(xmin = cut_like_to_haokan, xmax = cut_like_return, panel = panel_a_label)
vlines <- data.frame(x = c(cut_like_to_haokan, cut_like_return), panel = panel_a_label)
vline_b <- data.frame(
  x = cut_like_return,
  panel = factor("B. Visibility-avoidance index: like / (like + zaikan), three-button regime", levels = levels(plot_dt$panel))
)

ts_theme <- ggplot2::theme_classic(base_size = 11, base_family = "serif") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 9.5),
    legend.key.width = ggplot2::unit(1.6, "lines"),
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

year_breaks <- seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by = "1 year")

p <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = shade,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey92", color = NA
  ) +
  ggplot2::geom_vline(data = vlines, ggplot2::aes(xintercept = x),
                      linetype = "22", color = "grey45", linewidth = 0.45) +
  ggplot2::geom_vline(data = vline_b, ggplot2::aes(xintercept = x),
                      linetype = "22", color = "grey45", linewidth = 0.45) +
  ggplot2::geom_line(
    data = plot_dt,
    ggplot2::aes(x = year_month, y = value, linetype = family_label, color = family_label),
    linewidth = 0.6, lineend = "round"
  ) +
  ggplot2::scale_color_manual(values = c("grey10", "grey35", "grey55", "grey10")) +
  ggplot2::scale_linetype_manual(values = c("solid", "longdash", "dotted", "dotdash")) +
  ggplot2::facet_wrap(~panel, scales = "free", ncol = 1) +
  ggplot2::scale_x_date(breaks = year_breaks, labels = scales::label_date("%Y"),
                        expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  ggplot2::labs(
    x = NULL, y = NULL,
    caption = paste0(
      "Monthly means over posts with at least one read. Shaded band: Haokan/Zaikan-only regime. ",
      "Dashed lines: Dec. 21, 2018 and Jul. 1, 2020 redesigns.\n", sim_note
    )
  ) +
  ts_theme

save_figure(path = file.path(paths$figures, "main_regime_longrun_by_family.pdf"),
            plot = p, width = 9, height = 7.2, units = "in", bg = "white")

p_slide <- p +
  ggplot2::labs(caption = sim_note) +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans"),
    strip.text = ggplot2::element_text(family = "sans", size = 13, hjust = 0),
    axis.text = ggplot2::element_text(family = "sans", size = 12),
    legend.text = ggplot2::element_text(family = "sans", size = 12),
    plot.caption = ggplot2::element_text(family = "sans", size = 10)
  )
save_figure(path = file.path(slide_output_dir, "regime_longrun_by_family_slide.pdf"),
            plot = p_slide, width = 10.5, height = 5.6, units = "in", bg = "white")

# --- Regime x family table ---------------------------------------------------

regime_tbl <- dt[, .(
  Posts = .N,
  `One-click rate (%)` = 100 * mean(one_click_rate, na.rm = TRUE),
  `Like rate (%)` = 100 * mean(like_rate, na.rm = TRUE),
  `Zaikan rate (%)` = 100 * mean(look_rate, na.rm = TRUE)
), by = .(Regime = regime, Family = factor(family_gloss[content_family], levels = unname(family_gloss)))]
data.table::setorder(regime_tbl, Regime, Family)
regime_tbl[, Regime := as.character(Regime)]
regime_tbl[, Family := as.character(Family)]

write_tex_table(
  regime_tbl,
  path = file.path(paths$tables, "main_regime_family_rates.tex"),
  caption = paste0(
    "One-click engagement by interface regime and content family. ",
    "Rates are means of article-level counts divided by reads over posts with at least one read. ",
    sim_note
  ),
  label = "tab:regime-family-rates",
  digits = c(Posts = 0L, `One-click rate (%)` = 2L, `Like rate (%)` = 2L, `Zaikan rate (%)` = 2L),
  align = "llrrrr"
)

# --- Annual VAI by family ----------------------------------------------------

annual <- dt[publish_date >= cut_like_return, .(
  like = mean(like_rate, na.rm = TRUE),
  look = mean(look_rate, na.rm = TRUE)
), by = .(year, content_family)]
annual[, vai := 100 * like / (like + look)]
annual_wide <- data.table::dcast(annual, content_family ~ year, value.var = "vai")
annual_wide[, content_family := factor(content_family, levels = family_order)]
data.table::setorder(annual_wide, content_family)
annual_wide[, Family := family_gloss[as.character(content_family)]]
annual_wide[, content_family := NULL]
data.table::setcolorder(annual_wide, "Family")
year_cols <- setdiff(names(annual_wide), "Family")
data.table::setnames(annual_wide, year_cols, paste0("VAI ", year_cols, " (%)"))

write_tex_table(
  annual_wide,
  path = file.path(paths$tables, "main_vai_family_annual.tex"),
  caption = paste0(
    "Visibility-avoidance index by content family and year in the three-button regime. ",
    "VAI = mean like rate / (mean like rate + mean zaikan rate); 2020 covers July--December. ",
    sim_note
  ),
  label = "tab:vai-family-annual",
  digits = 1L,
  align = paste0("l", paste(rep("r", length(year_cols)), collapse = ""))
)

message("Saved regime long-run figure, regime x family table, and annual VAI table.")
