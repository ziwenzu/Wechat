bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

# ---------------------------------------------------------------------------
# 34_like_rate_within_read_bins.R
#
# Reviewer-facing check on the claim that count-only approval of propaganda
# matches approval of public service per read. Engagement rates fall
# mechanically with reach (viral posts attract casual readers), and service
# posts reach further, so the family comparison could be a denominator
# artifact. This script compares like and zaikan rates by family WITHIN
# quintiles of read counts (three-button regime), and reports the family
# ordering inside each quintile.
#
# DATA STATUS: uses data/wechat_instructional_dataset.rds (SIMULATED
# engagement counts, see DATA_STATUS.md). Outputs are teaching material.
# ---------------------------------------------------------------------------

ensure_packages(c("data.table", "fs"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
sim_note <- "Simulated teaching data (see DATA_STATUS.md)."

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))
keep_cols <- c("publish_date", "content_family", "read_num", "like_rate", "look_rate")
dt <- dt[, ..keep_cols]

family_gloss <- c(
  "public_service"   = "Public service",
  "soft_propaganda"  = "Soft propaganda",
  "state_governance" = "State governance",
  "hard_propaganda"  = "Hard propaganda"
)
family_order <- names(family_gloss)

post <- dt[publish_date >= as.Date("2020-07-01") & read_num > 0 & content_family %in% family_order]
post[, read_quintile := cut(
  read_num,
  breaks = stats::quantile(read_num, probs = seq(0, 1, 0.2)),
  include.lowest = TRUE,
  labels = c("Q1 (fewest reads)", "Q2", "Q3", "Q4", "Q5 (most reads)")
)]

bins <- post[, .(
  posts = .N,
  like_rate = 100 * mean(like_rate, na.rm = TRUE),
  look_rate = 100 * mean(look_rate, na.rm = TRUE)
), by = .(read_quintile, content_family)]
bins[, vai := 100 * like_rate / (like_rate + look_rate)]

like_wide <- data.table::dcast(bins, read_quintile ~ content_family, value.var = "like_rate")
look_wide <- data.table::dcast(bins, read_quintile ~ content_family, value.var = "look_rate")
vai_wide  <- data.table::dcast(bins, read_quintile ~ content_family, value.var = "vai")

make_tbl <- function(w, prefix) {
  data.table::setcolorder(w, c("read_quintile", family_order))
  data.table::setnames(w, family_order, paste0(prefix, ": ", family_gloss[family_order]))
  data.table::setnames(w, "read_quintile", "Read quintile")
  w[, `Read quintile` := as.character(`Read quintile`)]
  w
}
like_tbl <- make_tbl(data.table::copy(like_wide), "Like")
look_tbl <- make_tbl(data.table::copy(look_wide), "Zaikan")
vai_tbl  <- make_tbl(data.table::copy(vai_wide), "VAI")

out <- data.table::rbindlist(list(
  data.table::data.table(Measure = "Like rate (%)", `Read quintile` = like_tbl$`Read quintile`,
    `Public service` = like_wide$public_service, `Soft propaganda` = like_wide$soft_propaganda,
    `State governance` = like_wide$state_governance, `Hard propaganda` = like_wide$hard_propaganda),
  data.table::data.table(Measure = "Zaikan rate (%)", `Read quintile` = look_tbl$`Read quintile`,
    `Public service` = look_wide$public_service, `Soft propaganda` = look_wide$soft_propaganda,
    `State governance` = look_wide$state_governance, `Hard propaganda` = look_wide$hard_propaganda),
  data.table::data.table(Measure = "VAI (%)", `Read quintile` = vai_tbl$`Read quintile`,
    `Public service` = vai_wide$public_service, `Soft propaganda` = vai_wide$soft_propaganda,
    `State governance` = vai_wide$state_governance, `Hard propaganda` = vai_wide$hard_propaganda)
))

write_tex_table(
  out,
  path = file.path(paths$tables, "appendix_like_rate_within_read_bins.tex"),
  caption = paste0(
    "Count-only like rate, zaikan rate, and visibility-avoidance index by content family within ",
    "quintiles of read counts, three-button regime (Jul. 2020 -- Dec. 2024). Comparing families at ",
    "similar reach removes the mechanical decline of engagement rates with audience size. ",
    sim_note
  ),
  label = "tab:like-rate-within-read-bins",
  digits = 2L,
  align = "llrrrr"
)

message("Saved within-read-bin family comparison table.")
