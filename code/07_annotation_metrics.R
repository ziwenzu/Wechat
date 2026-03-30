bootstrap_args <- commandArgs(trailingOnly = FALSE)
bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
bootstrap_path <- normalizePath(sub("^--file=", "", bootstrap_file[[1]]))
source(file.path(dirname(bootstrap_path), "_common.R"))

ensure_packages(c("data.table", "fs"))

paths <- project_paths()
fs::dir_create(paths$tables, recurse = TRUE)

input_path <- file.path(paths$data, "annotation_sample_2000_blind.rds")
dt <- data.table::as.data.table(readRDS(input_path))

required_ra_cols <- c(
  "primary_label_ra1",
  "family_label_ra1",
  "primary_label_ra2",
  "family_label_ra2",
  "adjudicated_primary_label",
  "adjudicated_family_label"
)

missing_required <- setdiff(required_ra_cols, names(dt))
if (length(missing_required) > 0) {
  stop(
    paste("Missing required annotation columns:", paste(missing_required, collapse = ", ")),
    call. = FALSE
  )
}

has_real_annotations <- function(...) {
  cols <- list(...)
  all(vapply(cols, function(x) any(!is.na(x) & nzchar(trimws(as.character(x)))), logical(1)))
}

cohen_kappa <- function(x, y) {
  keep <- !(is.na(x) | is.na(y) | !nzchar(trimws(as.character(x))) | !nzchar(trimws(as.character(y))))
  x <- as.character(x[keep])
  y <- as.character(y[keep])

  if (length(x) == 0) {
    return(NA_real_)
  }

  levels_all <- sort(unique(c(x, y)))
  tab <- table(factor(x, levels = levels_all), factor(y, levels = levels_all))
  n <- sum(tab)
  p0 <- sum(diag(tab)) / n
  px <- rowSums(tab) / n
  py <- colSums(tab) / n
  pe <- sum(px * py)

  if (isTRUE(all.equal(1 - pe, 0))) {
    return(NA_real_)
  }

  (p0 - pe) / (1 - pe)
}

classification_metrics <- function(truth, pred) {
  keep <- !(is.na(truth) | is.na(pred) | !nzchar(trimws(as.character(truth))) | !nzchar(trimws(as.character(pred))))
  truth <- as.character(truth[keep])
  pred <- as.character(pred[keep])

  if (length(truth) == 0) {
    return(list(
      summary = data.table::data.table(
        accuracy = NA_real_,
        macro_f1 = NA_real_,
        n_eval = 0
      ),
      by_class = data.table::data.table(
        class = character(),
        precision = numeric(),
        recall = numeric(),
        f1 = numeric(),
        support = integer()
      ),
      confusion = data.table::data.table()
    ))
  }

  classes <- sort(unique(c(truth, pred)))
  conf_mat <- table(
    truth = factor(truth, levels = classes),
    pred = factor(pred, levels = classes)
  )
  conf <- data.table::as.data.table(as.matrix(conf_mat), keep.rownames = "truth")

  by_class <- data.table::rbindlist(lapply(classes, function(cls) {
    tp <- sum(truth == cls & pred == cls)
    fp <- sum(truth != cls & pred == cls)
    fn <- sum(truth == cls & pred != cls)
    precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
      NA_real_
    } else {
      2 * precision * recall / (precision + recall)
    }

    data.table::data.table(
      class = cls,
      precision = precision,
      recall = recall,
      f1 = f1,
      support = sum(truth == cls)
    )
  }))

  summary <- data.table::data.table(
    accuracy = mean(truth == pred),
    macro_f1 = mean(by_class$f1, na.rm = TRUE),
    n_eval = length(truth)
  )

  list(summary = summary, by_class = by_class, confusion = conf)
}

if (!has_real_annotations(
  dt$primary_label_ra1,
  dt$primary_label_ra2,
  dt$adjudicated_primary_label
)) {
  stop(
    paste(
      "No completed annotations detected in annotation_sample_2000_blind.rds.",
      "Populate RA and adjudication columns before running annotation metrics."
    ),
    call. = FALSE
  )
}

agreement_summary <- data.table::data.table(
  metric = c(
    "primary_exact_agreement",
    "family_exact_agreement",
    "primary_cohen_kappa",
    "family_cohen_kappa",
    "adjudication_rate"
  ),
  value = c(
    mean(dt$primary_label_ra1 == dt$primary_label_ra2, na.rm = TRUE),
    mean(dt$family_label_ra1 == dt$family_label_ra2, na.rm = TRUE),
    cohen_kappa(dt$primary_label_ra1, dt$primary_label_ra2),
    cohen_kappa(dt$family_label_ra1, dt$family_label_ra2),
    mean(
      dt$adjudicated_primary_label != dt$primary_label_ra1 |
        dt$adjudicated_primary_label != dt$primary_label_ra2,
      na.rm = TRUE
    )
  )
)

write_tex_table(
  agreement_summary,
  file.path(paths$tables, "annotation_reliability_summary.tex"),
  caption = "Inter-coder reliability summary for the annotated 2,000-post sample.",
  label = "tab:annotation-reliability-summary",
  digits = c(value = 4)
)

if ("llm_primary_label" %in% names(dt) && any(!is.na(dt$llm_primary_label) & nzchar(trimws(dt$llm_primary_label)))) {
  detailed_metrics <- classification_metrics(dt$adjudicated_primary_label, dt$llm_primary_label)

  write_tex_table(
    detailed_metrics$summary,
    file.path(paths$tables, "llm_vs_gold_detailed_summary.tex"),
    caption = "Detailed-label LLM validation against adjudicated gold labels.",
    label = "tab:llm-vs-gold-detailed-summary",
    digits = c(accuracy = 4, macro_f1 = 4, n_eval = 0)
  )

  write_tex_table(
    detailed_metrics$by_class,
    file.path(paths$tables, "llm_vs_gold_detailed_by_class.tex"),
    caption = "Class-wise detailed-label validation metrics for the LLM.",
    label = "tab:llm-vs-gold-detailed-by-class",
    digits = c(precision = 4, recall = 4, f1 = 4, support = 0)
  )

  write_tex_table(
    detailed_metrics$confusion,
    file.path(paths$tables, "llm_vs_gold_detailed_confusion.tex"),
    caption = "Detailed-label confusion matrix comparing LLM predictions to adjudicated gold labels.",
    label = "tab:llm-vs-gold-detailed-confusion",
    digits = 0
  )
}

if ("llm_family_label" %in% names(dt) && any(!is.na(dt$llm_family_label) & nzchar(trimws(dt$llm_family_label)))) {
  family_metrics <- classification_metrics(dt$adjudicated_family_label, dt$llm_family_label)

  write_tex_table(
    family_metrics$summary,
    file.path(paths$tables, "llm_vs_gold_family_summary.tex"),
    caption = "Family-label LLM validation against adjudicated gold labels.",
    label = "tab:llm-vs-gold-family-summary",
    digits = c(accuracy = 4, macro_f1 = 4, n_eval = 0)
  )

  write_tex_table(
    family_metrics$by_class,
    file.path(paths$tables, "llm_vs_gold_family_by_class.tex"),
    caption = "Class-wise family-label validation metrics for the LLM.",
    label = "tab:llm-vs-gold-family-by-class",
    digits = c(precision = 4, recall = 4, f1 = 4, support = 0)
  )

  write_tex_table(
    family_metrics$confusion,
    file.path(paths$tables, "llm_vs_gold_family_confusion.tex"),
    caption = "Family-label confusion matrix comparing LLM predictions to adjudicated gold labels.",
    label = "tab:llm-vs-gold-family-confusion",
    digits = 0
  )
}
