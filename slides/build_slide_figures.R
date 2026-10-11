args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]))
project_root <- normalizePath(file.path(dirname(script_path), ".."))

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

output_dir <- file.path(project_root, "slides", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dt <- as.data.table(readRDS(
  file.path(project_root, "data", "wechat_instructional_dataset.rds")
))
dt[, days_from_2018 := as.integer(publish_date - as.Date("2018-12-21"))]
dt[, days_from_2020 := as.integer(publish_date - as.Date("2020-07-01"))]
dt[, one_click_rate := like_rate + look_rate]

navy <- "#14213D"
teal <- "#007F7B"
red <- "#B23A48"
blue <- "#4777A8"
slate <- "#606B7C"
mist <- "#E7ECEF"

slide_theme <- theme_minimal(base_size = 18, base_family = "sans") +
  theme(
    text = element_text(colour = navy),
    axis.text = element_text(size = 18, colour = slate),
    axis.title = element_text(size = 19, face = "bold", colour = navy),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = mist, linewidth = 0.45),
    plot.margin = margin(10, 18, 8, 10),
    legend.position = "none"
  )

daily_means <- function(data, running_var) {
  data[
    abs(get(running_var)) <= 30,
    .(
      n = .N,
      one_click_rate = mean(one_click_rate, na.rm = TRUE),
      like_rate = mean(like_rate, na.rm = TRUE),
      look_rate = mean(look_rate, na.rm = TRUE)
    ),
    by = .(x = get(running_var))
  ][order(x)]
}

weighted_bins <- function(data, outcome, width = 5) {
  out <- copy(data)
  out[, side := ifelse(x < 0, "Before", "After")]
  out[, bin := floor(x / width)]
  out[, .(
    x = weighted.mean(x, n),
    y = weighted.mean(get(outcome), n)
  ), by = .(side, bin)]
}

linear_fit <- function(data, outcome) {
  out <- copy(data)
  out[, y := get(outcome)]
  fit <- lm(y ~ x, data = out, weights = n)
  grid <- data.table(x = seq(min(out$x), max(out$x), length.out = 120))
  pred <- predict(fit, newdata = grid, se.fit = TRUE)
  grid[, `:=`(
    y = as.numeric(pred$fit),
    lo = as.numeric(pred$fit - 1.96 * pred$se.fit),
    hi = as.numeric(pred$fit + 1.96 * pred$se.fit)
  )]
  grid
}

save_pdf <- function(plot, filename, width = 8.4, height = 4.4) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = "pdf",
    bg = "white"
  )
}

# December 2018: the combined Like/Haokan engagement before and after the change.
daily_2018 <- daily_means(dt, "days_from_2018")
left_2018 <- daily_2018[x < 0]
right_2018 <- daily_2018[x >= 0]
bins_2018 <- rbind(
  weighted_bins(left_2018, "one_click_rate"),
  weighted_bins(right_2018, "one_click_rate")
)
fits_2018 <- rbind(
  linear_fit(left_2018, "one_click_rate")[, side := "Before"],
  linear_fit(right_2018, "one_click_rate")[, side := "After"]
)

p_2018 <- ggplot() +
  geom_ribbon(
    data = fits_2018,
    aes(x = x, ymin = lo, ymax = hi, group = side),
    fill = mist,
    alpha = 0.85
  ) +
  geom_line(
    data = fits_2018,
    aes(x = x, y = y, group = side),
    colour = navy,
    linewidth = 1.45
  ) +
  geom_point(
    data = bins_2018,
    aes(x = x, y = y),
    shape = 21,
    size = 4.1,
    stroke = 0.85,
    colour = navy,
    fill = "white"
  ) +
  geom_vline(xintercept = 0, colour = red, linewidth = 1.15, linetype = "22") +
  annotate(
    "text", x = 1.8, y = Inf, label = "FRIENDS' LOOK FEED",
    hjust = 0, vjust = 1.55, size = 5.5, fontface = "bold", colour = red
  ) +
  scale_x_continuous(breaks = seq(-30, 30, 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  coord_cartesian(clip = "off") +
  labs(x = "Days from the change", y = "Like/Haokan engagement rate") +
  slide_theme

save_pdf(p_2018, "rdd_2018_slide.pdf")

# July 2020: pre-redesign combined action versus the two post-redesign options.
daily_2020 <- daily_means(dt, "days_from_2020")
pre_2020 <- daily_2020[x < 0]
post_2020 <- daily_2020[x >= 0]

make_series <- function(data, outcome, label) {
  list(
    bins = weighted_bins(data, outcome)[, metric := label],
    fits = linear_fit(data, outcome)[, metric := label]
  )
}

combined <- make_series(pre_2020, "one_click_rate", "Zaikan only")
likes <- make_series(post_2020, "like_rate", "Like")
zaikan <- make_series(post_2020, "look_rate", "Zaikan")
bins_2020 <- rbind(combined$bins, likes$bins, zaikan$bins)
fits_2020 <- rbind(combined$fits, likes$fits, zaikan$fits)
colour_map <- c("Zaikan only" = blue, "Like" = teal, "Zaikan" = red)

end_labels <- fits_2020[
  , .SD[which.max(x)],
  by = metric
][, `:=`(
  label_x = fifelse(metric == "Zaikan only", -5, 25),
  label_y = y + fcase(
    metric == "Zaikan only", 0.003,
    metric == "Zaikan", 0.0014,
    default = 0
  ),
  label_hjust = 1
)]

p_2020 <- ggplot() +
  geom_line(
    data = fits_2020,
    aes(x = x, y = y, colour = metric, group = metric),
    linewidth = 1.55
  ) +
  geom_point(
    data = bins_2020,
    aes(x = x, y = y, colour = metric),
    shape = 21,
    fill = "white",
    size = 3.9,
    stroke = 1.0
  ) +
  geom_vline(xintercept = 0, colour = navy, linewidth = 1.05, linetype = "22") +
  geom_text(
    data = end_labels,
    aes(
      x = label_x,
      y = label_y,
      label = metric,
      colour = metric,
      hjust = label_hjust
    ),
    vjust = -0.65,
    size = 6.2,
    fontface = "bold"
  ) +
  scale_colour_manual(values = colour_map) +
  scale_x_continuous(breaks = seq(-30, 30, 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  coord_cartesian(clip = "off") +
  labs(x = "Days from the change", y = "Engagement rate") +
  slide_theme

save_pdf(p_2020, "rdd_2020_unbundling_slide.pdf")

# July 2020 reform: lower-visibility Like versus friend-visible Zaikan,
# separately by content family. Values reproduce the manuscript's
# family-specific temporal-RD estimates and account-clustered standard errors.
visibility_rd <- data.table(
  family = rep(
    c("Hard Propaganda", "Soft Propaganda", "State Governance", "Public Service"),
    each = 2
  ),
  response = rep(c("Like", "Zaikan"), 4),
  estimate = c(
    0.0472, -0.0174,
    0.0434, -0.0139,
    0.0338, -0.0094,
    0.0442, -0.0046
  ),
  se = c(
    0.0032, 0.0031,
    0.0037, 0.0038,
    0.0031, 0.0034,
    0.0032, 0.0044
  )
)

visibility_rd[, family := factor(
  family,
  levels = rev(c("Hard Propaganda", "Soft Propaganda", "State Governance", "Public Service"))
)]
visibility_rd[, `:=`(
  effect = 100 * estimate,
  lo = 100 * (estimate - 1.96 * se),
  hi = 100 * (estimate + 1.96 * se),
  response = factor(response, levels = c("Like", "Zaikan"))
)]
visibility_rd[, label := sprintf("%+.2f", effect)]

response_colours <- c(
  "Like" = teal,
  "Zaikan" = red
)
response_labels <- c(
  "Like" = "Like (count only)",
  "Zaikan" = "Zaikan (may reach friends)"
)

p_visibility_rd <- ggplot(
  visibility_rd,
  aes(x = effect, y = family, colour = response, shape = response)
) +
  geom_vline(xintercept = 0, colour = navy, linewidth = 0.95, linetype = "22") +
  geom_errorbarh(
    aes(xmin = lo, xmax = hi),
    height = 0.16,
    linewidth = 1.15,
    position = position_dodge(width = 0.40)
  ) +
  geom_point(
    size = 4.7,
    stroke = 1.15,
    position = position_dodge(width = 0.40)
  ) +
  geom_text(
    data = visibility_rd[response == "Like"],
    aes(x = hi + 0.28, label = label),
    hjust = 0,
    size = 5.2,
    fontface = "bold",
    show.legend = FALSE,
    position = position_dodge(width = 0.40)
  ) +
  geom_text(
    data = visibility_rd[response == "Zaikan"],
    aes(x = lo - 0.28, label = label),
    hjust = 1,
    size = 5.2,
    fontface = "bold",
    show.legend = FALSE,
    position = position_dodge(width = 0.40)
  ) +
  scale_colour_manual(
    values = response_colours,
    breaks = names(response_labels),
    labels = response_labels
  ) +
  scale_shape_manual(
    values = c("Like" = 16, "Zaikan" = 15),
    breaks = names(response_labels),
    labels = response_labels
  ) +
  scale_x_continuous(
    limits = c(-3.6, 6.7),
    breaks = c(-2, 0, 2, 4, 6),
    labels = function(x) ifelse(x > 0, paste0("+", x), x),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Change when Like returned (percentage points)",
    y = NULL,
    colour = NULL,
    shape = NULL
  ) +
  slide_theme +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 17, face = "bold", colour = navy),
    axis.title.x = element_text(size = 15.5),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.text = element_text(size = 14.5, face = "bold", colour = navy),
    legend.key.width = unit(1.2, "lines"),
    legend.spacing.x = unit(0.45, "lines"),
    plot.margin = margin(2, 28, 7, 8)
  )

save_pdf(
  p_visibility_rd,
  "visibility_reversal_slide.pdf",
  width = 8.2,
  height = 3.85
)

# What government accounts post: overall composition and annual change.
family_levels <- c(
  "Public Service",
  "Soft Propaganda",
  "Hard Propaganda",
  "State Governance"
)
family_labels <- c(
  public_service = "Public Service",
  soft_propaganda = "Soft Propaganda",
  hard_propaganda = "Hard Propaganda",
  state_governance = "State Governance"
)
family_colours <- c(
  "Public Service" = teal,
  "Soft Propaganda" = blue,
  "Hard Propaganda" = red,
  "State Governance" = slate
)
family_linetypes <- c(
  "Public Service" = "solid",
  "Soft Propaganda" = "longdash",
  "Hard Propaganda" = "dotdash",
  "State Governance" = "dotted"
)
family_shapes <- c(
  "Public Service" = 16,
  "Soft Propaganda" = 17,
  "Hard Propaganda" = 15,
  "State Governance" = 18
)

family_overall <- dt[, .(posts = .N), by = content_family]
family_overall[, family := unname(family_labels[content_family])]
family_overall[, share := posts / sum(posts)]
family_overall[, family := factor(family, levels = rev(family_levels))]

p_family_share <- ggplot(
  family_overall,
  aes(x = share, y = family, fill = family)
) +
  geom_col(width = 0.58, show.legend = FALSE) +
  geom_text(
    aes(label = label_percent(accuracy = 0.1)(share)),
    nudge_x = 0.010,
    hjust = 0,
    size = 5.6,
    fontface = "bold",
    colour = navy
  ) +
  scale_fill_manual(values = family_colours) +
  scale_x_continuous(
    limits = c(0, 0.42),
    breaks = seq(0, 0.4, 0.1),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(title = "Overall composition", x = "Share of all posts", y = NULL) +
  slide_theme +
  theme(
    plot.title = element_text(size = 21, face = "bold", colour = navy),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 16, face = "bold", colour = navy),
    axis.title.x = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(8, 30, 6, 8)
  )

family_year <- dt[, .(posts = .N), by = .(year, content_family)]
family_year[, family := unname(family_labels[content_family])]
family_year[, share := posts / sum(posts), by = year]
family_year[, family := factor(family, levels = family_levels)]

end_labels <- family_year[year == max(year)]
end_labels[, label_y := fcase(
  family == "Soft Propaganda", share + 0.008,
  family == "Public Service", share + 0.008,
  family == "Hard Propaganda", share - 0.008,
  default = share
)]

p_family_trend <- ggplot(
  family_year,
  aes(
    x = year,
    y = share,
    colour = family,
    linetype = family,
    shape = family,
    group = family
  )
) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 3.1, stroke = 0.7) +
  geom_segment(
    data = end_labels,
    aes(x = year + 0.08, xend = year + 0.32, y = share, yend = label_y),
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = end_labels,
    aes(x = year + 0.40, y = label_y, label = family),
    hjust = 0,
    size = 4.35,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = family_colours) +
  scale_linetype_manual(values = family_linetypes) +
  scale_shape_manual(values = family_shapes) +
  scale_x_continuous(
    limits = c(2015, 2025.8),
    breaks = c(2015, 2018, 2020, 2022, 2024),
    expand = expansion(mult = c(0.01, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 0.45),
    breaks = seq(0, 0.4, 0.1),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  coord_cartesian(clip = "off") +
  labs(title = "Composition over time", x = NULL, y = "Share of posts") +
  slide_theme +
  theme(
    plot.title = element_text(size = 21, face = "bold", colour = navy),
    axis.text = element_text(size = 15, colour = slate),
    axis.title.y = element_text(size = 16),
    plot.margin = margin(8, 116, 6, 8)
  )

save_pdf(p_family_share, "content_family_share_slide.pdf", width = 5.2, height = 4.5)
save_pdf(p_family_trend, "content_family_trend_slide.pdf", width = 8.1, height = 4.5)

# Regression results for the mixed-channel attention dilemma. Estimates and
# standard errors match the preferred specifications in
# tables/main_communication_dilemma_post_models.tex,
# tables/main_communication_dilemma_crowding.tex, and
# tables/appendix_communication_attention_persistence.tex.
readership_coef <- data.table(
  family = c("Public Service", "Soft Propaganda", "Hard Propaganda"),
  y = c(3, 2, 1),
  estimate = c(0.996, 0.464, 0),
  std_error = c(0.008, 0.007, NA_real_),
  colour = c(teal, blue, slate)
)
readership_coef[, `:=`(
  ratio = exp(estimate),
  lo = fifelse(is.na(std_error), 1, exp(estimate - 1.96 * std_error)),
  hi = fifelse(is.na(std_error), 1, exp(estimate + 1.96 * std_error)),
  label = fifelse(
    is.na(std_error),
    "1.0\u00d7 ref.",
    sprintf("%.1f\u00d7", exp(estimate))
  )
)]

p_service_readership <- ggplot(readership_coef, aes(y = y)) +
  geom_vline(xintercept = 1, colour = slate, linewidth = 0.9, linetype = "22") +
  geom_segment(
    data = readership_coef[!is.na(std_error)],
    aes(x = lo, xend = hi, yend = y, colour = colour),
    linewidth = 1.8,
    lineend = "round"
  ) +
  geom_point(
    aes(x = ratio, fill = colour),
    shape = 21,
    size = 6.2,
    stroke = 1.0,
    colour = "white"
  ) +
  geom_text(
    aes(x = ratio + 0.11, label = label, colour = colour),
    hjust = 0,
    size = 5.8,
    fontface = "bold"
  ) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(
    limits = c(0.80, 3.18),
    breaks = c(1, 1.5, 2, 2.5, 3),
    labels = function(x) sprintf("%.1f\u00d7", x),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0.55, 3.45),
    breaks = readership_coef$y,
    labels = readership_coef$family,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Service gets more reads",
    subtitle = "Hard propaganda = 1\nSame account-day",
    x = "Relative reads",
    y = NULL
  ) +
  slide_theme +
  theme(
    plot.title = element_text(size = 22, face = "bold", colour = navy),
    plot.subtitle = element_text(size = 14.5, colour = slate, margin = margin(b = 6)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 17, face = "bold", colour = navy),
    axis.title.x = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(8, 38, 8, 8)
  )

service_share_effects <- data.table(
  outcome = c(
    "Next observed week:\ntotal reads",
    "Propaganda that day:\nreads per post"
  ),
  y = c(2, 1),
  estimate = c(0.397, -0.876),
  std_error = c(0.027, 0.014),
  colour = c(teal, red)
)
service_share_effects[, `:=`(
  effect = 100 * (exp(0.10 * estimate) - 1),
  lo = 100 * (exp(0.10 * (estimate - 1.96 * std_error)) - 1),
  hi = 100 * (exp(0.10 * (estimate + 1.96 * std_error)) - 1)
)]
service_share_effects[, label := sprintf("%+.1f%%", effect)]
service_share_effects[, `:=`(
  label_x = effect + fifelse(effect > 0, -0.55, 0.55),
  label_hjust = fifelse(effect > 0, 1, 0)
)]

p_service_tradeoff <- ggplot(service_share_effects, aes(y = y)) +
  geom_vline(xintercept = 0, colour = slate, linewidth = 0.9, linetype = "22") +
  geom_segment(
    aes(x = lo, xend = hi, yend = y, colour = colour),
    linewidth = 1.8,
    lineend = "round"
  ) +
  geom_point(
    aes(x = effect, fill = colour),
    shape = 21,
    size = 6.2,
    stroke = 1.0,
    colour = "white"
  ) +
  geom_text(
    aes(
      x = label_x,
      label = label,
      colour = colour,
      hjust = label_hjust
    ),
    size = 5.8,
    fontface = "bold"
  ) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(
    limits = c(-10.5, 7.5),
    breaks = c(-10, -5, 0, 5),
    labels = function(x) ifelse(x > 0, paste0("+", x, "%"), paste0(x, "%")),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0.55, 2.45),
    breaks = service_share_effects$y,
    labels = service_share_effects$outcome,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "10-point shift to service",
    subtitle = "From propaganda; 0 = no association",
    x = "Associated difference in reads",
    y = NULL
  ) +
  slide_theme +
  theme(
    plot.title = element_text(size = 21, face = "bold", colour = navy),
    plot.subtitle = element_text(size = 14.5, colour = slate, margin = margin(b = 6)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15.5, face = "bold", colour = navy, lineheight = 0.95),
    axis.title.x = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(8, 48, 8, 8)
  )

save_pdf(p_service_readership, "service_readership_coef_slide.pdf", width = 5.7, height = 4.15)
save_pdf(p_service_tradeoff, "service_tradeoff_coef_slide.pdf", width = 7.0, height = 4.15)

message("Saved slide-specific figures to ", output_dir)
