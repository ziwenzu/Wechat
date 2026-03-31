bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs", "fixest"))

options(fixest_notes = FALSE)

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)
memo_dir <- file.path(paths$root, "memo")
fs::dir_create(memo_dir, recurse = TRUE)

table_gap_path <- file.path(paths$tables, "main_communication_attention_gap.tex")
table_post_path <- file.path(paths$tables, "main_communication_dilemma_post_models.tex")
table_crowd_path <- file.path(paths$tables, "main_communication_dilemma_crowding.tex")
table_persist_path <- file.path(paths$tables, "appendix_communication_attention_persistence.tex")
memo_path <- file.path(memo_dir, "2026-03-31_communication_dilemma_design.md")

family_gloss <- c(
  "public_service" = "Public Service",
  "soft_propaganda" = "Soft Propaganda",
  "hard_propaganda" = "Hard Propaganda"
)
family_order <- c("public_service", "soft_propaganda", "hard_propaganda")

fmt_num <- function(x, digits = 3L) {
  formatC(x, digits = digits, format = "f")
}

fmt_pct <- function(x, digits = 1L) {
  paste0(formatC(x, digits = digits, format = "f"), "%")
}

fmt_pp <- function(x, digits = 1L) {
  paste0(formatC(x, digits = digits, format = "f", flag = "+"), " pp")
}

sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  ""
}

coef_cell <- function(model, term, digits = 3L) {
  ct <- summary(model)$coeftable
  if (!(term %in% rownames(ct))) return("")
  est <- ct[term, "Estimate"]
  p <- ct[term, ncol(ct)]
  paste0(fmt_num(est, digits = digits), sig_stars(p))
}

se_cell <- function(model, term, digits = 3L) {
  ct <- summary(model)$coeftable
  if (!(term %in% rownames(ct))) return("")
  paste0("(", fmt_num(ct[term, "Std. Error"], digits = digits), ")")
}

dt <- data.table::as.data.table(readRDS(file.path(paths$data, "wechat_instructional_dataset.rds")))
dt_main <- dt[content_family %in% family_order]
dt_main[, family_label := factor(unname(family_gloss[content_family]), levels = unname(family_gloss[family_order]))]

# ------------------------------------------------------------------
# Overall attention gap: main families only
# ------------------------------------------------------------------

gap_family <- dt_main[
  ,
  .(
    posts = .N,
    reads = sum(read_num, na.rm = TRUE),
    mean_reads = mean(read_num, na.rm = TRUE)
  ),
  by = content_family
]
gap_family[, Family := factor(unname(family_gloss[content_family]), levels = unname(family_gloss[family_order]))]
data.table::setorder(gap_family, Family)
gap_family[, post_share := posts / sum(posts)]
gap_family[, read_share := reads / sum(reads)]
gap_family[, attention_gap_pp := 100 * (read_share - post_share)]
gap_family[, read_to_post_ratio := read_share / post_share]

gap_table <- gap_family[, .(
  Family = as.character(Family),
  `Posts (M)` = posts / 1e6,
  `Mean Reads` = mean_reads,
  `Post Share (%)` = 100 * post_share,
  `Read Share (%)` = 100 * read_share,
  `Attention Premium (pp)` = attention_gap_pp,
  `Read/Post Ratio` = read_to_post_ratio
)]

write_tex_table(
  gap_table,
  table_gap_path,
  caption = "Attention allocation across the three main content families. Positive attention premiums indicate that a family attracts a larger share of reads than of post slots.",
  label = "tab:main-communication-attention-gap",
  digits = c(
    `Posts (M)` = 2L,
    `Mean Reads` = 0L,
    `Post Share (%)` = 1L,
    `Read Share (%)` = 1L,
    `Attention Premium (pp)` = 1L,
    `Read/Post Ratio` = 2L
  ),
  align = "lrrrrrr"
)

# ------------------------------------------------------------------
# Post-level attention premium models
# ------------------------------------------------------------------

term_public <- "content_family::public_service"
term_soft <- "content_family::soft_propaganda"

m_post_1 <- fixest::feols(
  log1p(read_num) ~ i(content_family, ref = "hard_propaganda"),
  cluster = ~account_id,
  data = dt_main
)

m_post_2 <- fixest::feols(
  log1p(read_num) ~ i(content_family, ref = "hard_propaganda") | account_id + publish_date,
  cluster = ~account_id,
  data = dt_main
)

m_post_3 <- fixest::feols(
  log1p(read_num) ~ i(content_family, ref = "hard_propaganda") | account_id^publish_date,
  cluster = ~account_id,
  data = dt_main
)

post_table <- data.table::rbindlist(list(
  data.table::data.table(Term = "Public Service", `(1)` = coef_cell(m_post_1, term_public), `(2)` = coef_cell(m_post_2, term_public), `(3)` = coef_cell(m_post_3, term_public)),
  data.table::data.table(Term = "", `(1)` = se_cell(m_post_1, term_public), `(2)` = se_cell(m_post_2, term_public), `(3)` = se_cell(m_post_3, term_public)),
  data.table::data.table(Term = "Soft Propaganda", `(1)` = coef_cell(m_post_1, term_soft), `(2)` = coef_cell(m_post_2, term_soft), `(3)` = coef_cell(m_post_3, term_soft)),
  data.table::data.table(Term = "", `(1)` = se_cell(m_post_1, term_soft), `(2)` = se_cell(m_post_2, term_soft), `(3)` = se_cell(m_post_3, term_soft)),
  data.table::data.table(Term = "Account FE", `(1)` = "No", `(2)` = "Yes", `(3)` = "No"),
  data.table::data.table(Term = "Date FE", `(1)` = "No", `(2)` = "Yes", `(3)` = "No"),
  data.table::data.table(Term = "Account-Day FE", `(1)` = "No", `(2)` = "No", `(3)` = "Yes"),
  data.table::data.table(Term = "Observations", `(1)` = format(nobs(m_post_1), big.mark = ","), `(2)` = format(nobs(m_post_2), big.mark = ","), `(3)` = format(nobs(m_post_3), big.mark = ",")),
  data.table::data.table(Term = "Within R2", `(1)` = "", `(2)` = fmt_num(as.numeric(fixest::fitstat(m_post_2, "wr2")), 3L), `(3)` = fmt_num(as.numeric(fixest::fitstat(m_post_3, "wr2")), 3L))
))

write_tex_table(
  post_table,
  table_post_path,
  caption = "Post-level attention premium models. Outcome is log(1 + reads). Baseline family is hard propaganda. Standard errors clustered by account.",
  label = "tab:main-communication-dilemma-post",
  align = "lccc"
)

# ------------------------------------------------------------------
# Daily crowding models: full channel with state share control
# ------------------------------------------------------------------

daily <- dt[
  ,
  .(
    service_posts = sum(content_family == "public_service"),
    propaganda_posts = sum(content_family %in% c("soft_propaganda", "hard_propaganda")),
    state_posts = sum(content_family == "state_governance"),
    total_posts = .N,
    service_reads = sum(read_num[content_family == "public_service"], na.rm = TRUE),
    propaganda_reads = sum(read_num[content_family %in% c("soft_propaganda", "hard_propaganda")], na.rm = TRUE)
  ),
  by = .(account_id, publish_date)
]

daily <- daily[service_posts > 0 & propaganda_posts > 0]
daily[, service_share := service_posts / total_posts]
daily[, state_share := state_posts / total_posts]
daily[, propaganda_mean_reads := propaganda_reads / propaganda_posts]
daily[, service_post_share_main := service_posts / (service_posts + propaganda_posts)]
daily[, service_read_share_main := service_reads / (service_reads + propaganda_reads)]
daily[, service_gap_pp := 100 * (service_read_share_main - service_post_share_main)]

m_crowd_1 <- fixest::feols(
  log1p(propaganda_mean_reads) ~ service_share | account_id + publish_date,
  cluster = ~account_id,
  data = daily
)

m_crowd_2 <- fixest::feols(
  log1p(propaganda_mean_reads) ~ service_share + state_share + log1p(total_posts) | account_id + publish_date,
  cluster = ~account_id,
  data = daily
)

crowd_table <- data.table::rbindlist(list(
  data.table::data.table(Term = "Service Post Share", `(1)` = coef_cell(m_crowd_1, "service_share"), `(2)` = coef_cell(m_crowd_2, "service_share")),
  data.table::data.table(Term = "", `(1)` = se_cell(m_crowd_1, "service_share"), `(2)` = se_cell(m_crowd_2, "service_share")),
  data.table::data.table(Term = "State-Governance Share", `(1)` = "", `(2)` = coef_cell(m_crowd_2, "state_share")),
  data.table::data.table(Term = "", `(1)` = "", `(2)` = se_cell(m_crowd_2, "state_share")),
  data.table::data.table(Term = "Log Total Posts", `(1)` = "", `(2)` = coef_cell(m_crowd_2, "log1p(total_posts)")),
  data.table::data.table(Term = "", `(1)` = "", `(2)` = se_cell(m_crowd_2, "log1p(total_posts)")),
  data.table::data.table(Term = "Account FE", `(1)` = "Yes", `(2)` = "Yes"),
  data.table::data.table(Term = "Date FE", `(1)` = "Yes", `(2)` = "Yes"),
  data.table::data.table(Term = "Observations", `(1)` = format(nobs(m_crowd_1), big.mark = ","), `(2)` = format(nobs(m_crowd_2), big.mark = ",")),
  data.table::data.table(Term = "Within R2", `(1)` = fmt_num(as.numeric(fixest::fitstat(m_crowd_1, "wr2")), 3L), `(2)` = fmt_num(as.numeric(fixest::fitstat(m_crowd_2, "wr2")), 3L))
))

write_tex_table(
  crowd_table,
  table_crowd_path,
  caption = "Daily crowding models on mixed service-propaganda account-days. Outcome is log(1 + mean reads per propaganda post). Standard errors clustered by account.",
  label = "tab:main-communication-dilemma-crowding",
  align = "lcc"
)

# ------------------------------------------------------------------
# Weekly persistence models: suggestive dynamic evidence
# ------------------------------------------------------------------

dt[, week_start := publish_date - (as.integer(format(publish_date, "%u")) - 1L)]

weekly <- dt[
  ,
  .(
    service_posts = sum(content_family == "public_service"),
    state_posts = sum(content_family == "state_governance"),
    total_posts = .N,
    total_reads = sum(read_num, na.rm = TRUE)
  ),
  by = .(account_id, week_start)
]

data.table::setorder(weekly, account_id, week_start)
weekly[, service_share := service_posts / pmax(total_posts, 1)]
weekly[, state_share := state_posts / pmax(total_posts, 1)]
weekly[, log_total_reads := log1p(total_reads)]
weekly[, lead_log_total_reads := data.table::shift(log_total_reads, -1L), by = account_id]
weekly <- weekly[!is.na(lead_log_total_reads)]

m_persist_1 <- fixest::feols(
  lead_log_total_reads ~ service_share + state_share + log1p(total_posts) | account_id + week_start,
  cluster = ~account_id,
  data = weekly
)

m_persist_2 <- fixest::feols(
  lead_log_total_reads ~ service_share + state_share + log_total_reads + log1p(total_posts) | account_id + week_start,
  cluster = ~account_id,
  data = weekly
)

persist_table <- data.table::rbindlist(list(
  data.table::data.table(Term = "Service Post Share", `(1)` = coef_cell(m_persist_1, "service_share"), `(2)` = coef_cell(m_persist_2, "service_share")),
  data.table::data.table(Term = "", `(1)` = se_cell(m_persist_1, "service_share"), `(2)` = se_cell(m_persist_2, "service_share")),
  data.table::data.table(Term = "State-Governance Share", `(1)` = coef_cell(m_persist_1, "state_share"), `(2)` = coef_cell(m_persist_2, "state_share")),
  data.table::data.table(Term = "", `(1)` = se_cell(m_persist_1, "state_share"), `(2)` = se_cell(m_persist_2, "state_share")),
  data.table::data.table(Term = "Current Log Reads", `(1)` = "", `(2)` = coef_cell(m_persist_2, "log_total_reads")),
  data.table::data.table(Term = "", `(1)` = "", `(2)` = se_cell(m_persist_2, "log_total_reads")),
  data.table::data.table(Term = "Log Total Posts", `(1)` = coef_cell(m_persist_1, "log1p(total_posts)"), `(2)` = coef_cell(m_persist_2, "log1p(total_posts)")),
  data.table::data.table(Term = "", `(1)` = se_cell(m_persist_1, "log1p(total_posts)"), `(2)` = se_cell(m_persist_2, "log1p(total_posts)")),
  data.table::data.table(Term = "Account FE", `(1)` = "Yes", `(2)` = "Yes"),
  data.table::data.table(Term = "Week FE", `(1)` = "Yes", `(2)` = "Yes"),
  data.table::data.table(Term = "Observations", `(1)` = format(nobs(m_persist_1), big.mark = ","), `(2)` = format(nobs(m_persist_2), big.mark = ",")),
  data.table::data.table(Term = "Within R2", `(1)` = fmt_num(as.numeric(fixest::fitstat(m_persist_1, "wr2")), 3L), `(2)` = fmt_num(as.numeric(fixest::fitstat(m_persist_2, "wr2")), 3L))
))

write_tex_table(
  persist_table,
  table_persist_path,
  caption = "Weekly persistence models. Outcome is next-week log(1 + total reads). These estimates are suggestive rather than causal because content mix can respond to anticipated demand.",
  label = "tab:appendix-communication-attention-persistence",
  align = "lcc"
)

# ------------------------------------------------------------------
# Memo
# ------------------------------------------------------------------

service_gap_overall <- gap_family[content_family == "public_service"]
service_gap_mean <- mean(daily$service_gap_pp, na.rm = TRUE)
service_gap_median <- stats::median(daily$service_gap_pp, na.rm = TRUE)
share_mixed_days <- nrow(daily) / data.table::uniqueN(dt_main[, .(account_id, publish_date)])

memo_lines <- c(
  "# 2026-03-31 Communication Dilemma Design",
  "",
  "Outputs:",
  paste0("- ", table_gap_path),
  paste0("- ", table_post_path),
  paste0("- ", table_crowd_path),
  paste0("- ", table_persist_path),
  "",
  "Recommended Main-Text Package:",
  "- 3 main tables: the family-level attention gap, the post-level attention-premium models, and the daily crowding models.",
  "- 1 appendix table: the weekly persistence / AR(1)-style model for attention maintenance if you want a supplementary check on the phrase `sustains attention`.",
  "",
  "Empirical Strategy:",
  "1. Attention premium. Estimate post-level OLS models with outcome log(1 + reads). The preferred specification uses account-day fixed effects, so identification comes from comparing service, soft-propaganda, and hard-propaganda posts published by the same account on the same day.",
  "2. Daily crowding. Collapse to account-day and keep days with both service and propaganda posts. Regress log(1 + mean propaganda reads per post) on the service-post share. The preferred model also controls for the state-governance share and log total posts, with account and calendar-date fixed effects.",
  "3. Weekly persistence. Estimate next-week log(1 + total reads) on the current week's service share. Keep this as suggestive evidence only; it is useful for the phrase 'service sustains attention' but not as the core identification strategy.",
  "",
  "Why Not Lead With AR In The Main Text:",
  "- The dynamic model adds stronger assumptions about demand persistence and strategic posting decisions.",
  "- The account-day fixed-effect model is cleaner for the sentence 'service draws higher readership than propaganda in the same channel.'",
  "- The weekly model is still worth keeping because it helps justify the phrase 'sustains attention,' but it should not carry the main descriptive claim by itself.",
  "",
  "Controls and Modeling Choices:",
  "- Post-level main model: no extra covariates beyond fixed effects; the account-day FE already absorbs account baseline demand, day-level local news shocks, and channel size for that day.",
  "- Daily crowding model: add state-governance share and log total posts. These make the service-share coefficient interpretable as a shift away from propaganda rather than a generic increase in activity or a substitution driven by another content family.",
  "- Clustering: account level throughout.",
  "- Functional form: log(1 + reads) / log(1 + mean reads) to handle zeros and extreme skew without dropping low-read posts.",
  "",
  "Interpretation Obstacles:",
  "- This section is descriptive, not a clean causal design. Content mix can respond to anticipated local demand, emergencies, or bureaucratic calendar pressures.",
  "- There is no intra-day timestamp/order variable in the cleaned data, so the design identifies within-day competition, not exact feed-slot displacement.",
  "- Read counters are top-coded at 100,000+, which likely attenuates the estimated service advantage for the most viral posts.",
  "- Classification error should mostly attenuate family contrasts, but a high-confidence or audited-sample robustness check would still be useful before writing the final prose.",
  "",
  "Headline Results From The Current Run:",
  paste0(
    "- Public service accounts for ",
    fmt_pct(100 * service_gap_overall$post_share, 1L),
    " of service-vs-propaganda posts but ",
    fmt_pct(100 * service_gap_overall$read_share, 1L),
    " of service-vs-propaganda reads, for an attention premium of ",
    fmt_pp(service_gap_overall$attention_gap_pp, 1L),
    " and a read/post ratio of ",
    fmt_num(service_gap_overall$read_to_post_ratio, 2L),
    "."
  ),
  paste0(
    "- Mixed service-propaganda account-days make up ",
    fmt_pct(100 * share_mixed_days, 1L),
    " of all three-family account-days. On those days, the mean service attention premium is ",
    fmt_pp(service_gap_mean, 1L),
    " and the median is ",
    fmt_pp(service_gap_median, 1L),
    "."
  ),
  paste0(
    "- Preferred post-level model (account-day FE): service coefficient = ",
    fmt_num(stats::coef(m_post_3)[term_public], 3L),
    ", soft-propaganda coefficient = ",
    fmt_num(stats::coef(m_post_3)[term_soft], 3L),
    ", relative to hard propaganda."
  ),
  paste0(
    "- Preferred daily crowding model: service-share coefficient = ",
    fmt_num(stats::coef(m_crowd_2)["service_share"], 3L),
    "; state-governance share coefficient = ",
    fmt_num(stats::coef(m_crowd_2)["state_share"], 3L),
    "."
  ),
  paste0(
    "- Weekly persistence appendix: service-share coefficient = ",
    fmt_num(stats::coef(m_persist_1)["service_share"], 3L),
    " without current reads and ",
    fmt_num(stats::coef(m_persist_2)["service_share"], 3L),
    " with current reads controlled."
  ),
  "",
  "Main Interpretation To Carry Forward Into Writing:",
  "- Service content clearly over-performs propaganda on readership within the same channel.",
  "- The strongest defensible wording is that service content absorbs a disproportionate share of attention and leaves less average readership for propaganda on mixed channel-days.",
  "- The phrase 'crowding out' is acceptable if it is written as a descriptive allocation result rather than a fully causal displacement claim.",
  "- The phrase 'sustains attention' should be tied to the weekly persistence result and explicitly described as suggestive."
)

writeLines(memo_lines, memo_path, useBytes = TRUE)

message("Saved tables to: ", table_gap_path, ", ", table_post_path, ", ", table_crowd_path, ", ", table_persist_path)
message("Saved memo to: ", memo_path)
