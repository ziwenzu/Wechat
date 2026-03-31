bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))
source(file.path(dirname(bootstrap_path), "_rdd_helpers.R"))

ensure_packages(c("data.table", "fs", "rdrobust", "ggplot2", "scales", "sandwich", "lmtest"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
fs::dir_create(paths$figures, recurse = TRUE)

dt <- load_rdd_data(paths)
dt[, account_id := .GRP, by = public_account_name]

family_gloss <- c(
  "public_service"   = "Public Service",
  "state_governance" = "State Governance",
  "soft_propaganda"  = "Soft Propaganda",
  "hard_propaganda"  = "Hard Propaganda"
)
family_order <- names(family_gloss)

art_2020 <- dt[
  abs(days_from_2020) <= 30 &
  read_num > 0 &
  is.finite(one_click_rate) &
  is.finite(like_rate) &
  is.finite(look_rate)
]

base_fit <- rdrobust::rdrobust(
  y = art_2020$one_click_rate,
  x = art_2020$days_from_2020,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd",
  cluster = art_2020$account_id,
  all = TRUE
)

h_common <- mean(base_fit$bws[1, ])

triangular_weight <- function(x, h) {
  pmax(0, 1 - abs(x) / h)
}

fit_shared_local_linear <- function(sub, outcome, h, label = NULL) {
  loc <- data.table::copy(sub[
    abs(days_from_2020) <= h &
    is.finite(get(outcome))
  ])

  loc[, `:=`(
    x = days_from_2020,
    y = get(outcome),
    post = as.integer(days_from_2020 >= 0),
    weight = triangular_weight(days_from_2020, h)
  )]

  fit <- stats::lm(y ~ post + x + post:x, data = loc, weights = weight)
  vc <- suppressWarnings(sandwich::vcovCL(fit, cluster = loc$account_id, type = "HC1"))
  ct <- suppressWarnings(lmtest::coeftest(fit, vcov. = vc))
  cf <- stats::coef(fit)

  data.table::data.table(
    label = if (is.null(label)) outcome else label,
    tau = unname(cf["post"]),
    se = unname(ct["post", "Std. Error"]),
    pvalue = unname(ct["post", "Pr(>|t|)"]),
    mu_left = unname(cf["(Intercept)"]),
    mu_right = unname(cf["(Intercept)"] + cf["post"]),
    h = h,
    n_left = sum(loc$x < 0),
    n_right = sum(loc$x >= 0)
  )
}

overall_fits <- data.table::rbindlist(list(
  fit_shared_local_linear(art_2020, "one_click_rate", h_common, "One-click"),
  fit_shared_local_linear(art_2020, "like_rate", h_common, "Like"),
  fit_shared_local_linear(art_2020, "look_rate", h_common, "Zaikan")
))

tau_one <- overall_fits[label == "One-click", tau]
tau_like <- overall_fits[label == "Like", tau]
tau_look <- overall_fits[label == "Zaikan", tau]
mu_like_right <- overall_fits[label == "Like", mu_right]
mu_look_right <- overall_fits[label == "Zaikan", mu_right]

mechanism_rows <- data.table::data.table(
  Statistic = c(
    "Common bandwidth h (days)",
    "Shared-bandwidth one-click jump (tau_O, pp)",
    "Shared-bandwidth like jump (tau_L, pp)",
    "Shared-bandwidth zaikan jump (tau_Z, pp)",
    "Right-cutoff like mean (mu_L+, pp)",
    "Right-cutoff zaikan mean (mu_Z+, pp)",
    "Behavioral self-censorship proxy (tau_O / tau_L, %)",
    "Visible-to-private substitution share (-tau_Z / tau_L, %)",
    "Post-cutoff visibility-avoidance index (VAI, %)"
  ),
  Estimate = c(
    h_common,
    100 * tau_one,
    100 * tau_like,
    100 * tau_look,
    100 * mu_like_right,
    100 * mu_look_right,
    100 * tau_one / tau_like,
    100 * (-tau_look) / tau_like,
    100 * mu_like_right / (mu_like_right + mu_look_right)
  ),
  SE = c(
    NA_real_,
    100 * overall_fits[label == "One-click", se],
    100 * overall_fits[label == "Like", se],
    100 * overall_fits[label == "Zaikan", se],
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_
  ),
  `p-value` = c(
    NA_real_,
    overall_fits[label == "One-click", pvalue],
    overall_fits[label == "Like", pvalue],
    overall_fits[label == "Zaikan", pvalue],
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_
  )
)

write_tex_table(
  mechanism_rows,
  path = file.path(paths$tables, "main_mechanism_decomposition.tex"),
  caption = paste0(
    "Shared-bandwidth decomposition of the July 2020 redesign. All three component RDDs are re-estimated with a common local-linear bandwidth inherited from the one-click specification (h = ",
    formatC(h_common, format = "f", digits = 2),
    " days), triangular weights, and account-clustered robust standard errors. Derived ratios are descriptive summaries rather than separate causal estimands."
  ),
  label = "tab:mechanism-decomposition",
  digits = c(Estimate = 2L, SE = 2L, `p-value` = 3L),
  align = "lrrr"
)

family_rows <- data.table::rbindlist(lapply(family_order, function(fam) {
  sub <- art_2020[content_family == fam]
  fit_like <- fit_shared_local_linear(sub, "like_rate", h_common, "Like")
  fit_look <- fit_shared_local_linear(sub, "look_rate", h_common, "Zaikan")

  data.table::data.table(
    Family = family_gloss[[fam]],
    `Post-cutoff like (pp)` = 100 * fit_like$mu_right,
    `Post-cutoff zaikan (pp)` = 100 * fit_look$mu_right,
    `VAI (%)` = 100 * fit_like$mu_right / (fit_like$mu_right + fit_look$mu_right)
  )
}))

write_tex_table(
  family_rows,
  path = file.path(paths$tables, "main_vai_family.tex"),
  caption = paste0(
    "Visibility-avoidance index by content family at the July 2020 cutoff. For each family, the post-cutoff like and zaikan means are the right-side local-linear fitted values at x = 0 using the common bandwidth h = ",
    formatC(h_common, format = "f", digits = 2),
    " days. VAI = like / (like + zaikan)."
  ),
  label = "tab:vai-family",
  digits = c(`Post-cutoff like (pp)` = 2L, `Post-cutoff zaikan (pp)` = 2L, `VAI (%)` = 1L),
  align = "lrrr"
)

plot_rows <- data.table::copy(family_rows)
plot_rows[, Family := factor(Family, levels = family_gloss[family_order])]
overall_vai <- mechanism_rows[Statistic == "Post-cutoff visibility-avoidance index (VAI, %)", Estimate]

p_vai <- ggplot2::ggplot(plot_rows, ggplot2::aes(x = `VAI (%)`, y = Family)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, xend = `VAI (%)`, y = Family, yend = Family),
    linewidth = 0.7,
    color = "grey55"
  ) +
  ggplot2::geom_point(shape = 21, size = 3.2, fill = "white", color = "black", stroke = 0.7) +
  ggplot2::geom_vline(xintercept = overall_vai, linetype = "dashed", color = "grey35", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f", `VAI (%)`)),
    hjust = -0.15,
    size = 3.1,
    family = "serif"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, max(100, max(plot_rows$`VAI (%)`) + 8)),
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  ggplot2::labs(
    x = "Visibility-Avoidance Index",
    y = NULL
  ) +
  rd_theme +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

save_figure(
  path = file.path(paths$figures, "main_vai_family.pdf"),
  plot = p_vai,
  width = 7,
  height = 4.8,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)

message("Saved mechanism decomposition table, family VAI table, and VAI figure.")
