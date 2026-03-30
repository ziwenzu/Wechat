get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]])))
  }

  normalizePath(".")
}

get_script_dir <- function() {
  script_path <- get_script_path()
  if (dir.exists(script_path)) {
    return(script_path)
  }
  dirname(script_path)
}

project_paths <- function() {
  script_dir <- get_script_dir()
  root_dir <- normalizePath(file.path(script_dir, ".."))

  articles_candidates <- c(
    file.path(root_dir, "data", "articles.csv"),
    file.path(root_dir, "analysis", "data", "articles.csv")
  )
  articles_csv <- articles_candidates[file.exists(articles_candidates)][1]

  if (is.na(articles_csv)) {
    articles_csv <- file.path(root_dir, "data", "articles.csv")
  }

  list(
    root = root_dir,
    code = file.path(root_dir, "code"),
    data = file.path(root_dir, "data"),
    figures = file.path(root_dir, "figures"),
    tables = file.path(root_dir, "tables"),
    articles_csv = articles_csv
  )
}

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  parsed <- list()

  for (arg in args) {
    if (!grepl("^--", arg)) {
      next
    }

    clean_arg <- sub("^--", "", arg)

    if (grepl("=", clean_arg, fixed = TRUE)) {
      pieces <- strsplit(clean_arg, "=", fixed = TRUE)[[1]]
      parsed[[pieces[[1]]]] <- paste(pieces[-1], collapse = "=")
    } else {
      parsed[[clean_arg]] <- TRUE
    }
  }

  parsed
}

ensure_packages <- function(packages) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      paste(
        "Missing R packages:",
        paste(missing_packages, collapse = ", "),
        "\nInstall them before running this script."
      ),
      call. = FALSE
    )
  }
}

coalesce_zero <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  x
}

safe_rate <- function(numerator, denominator) {
  out <- numerator / denominator
  out[is.na(denominator) | denominator <= 0] <- NA_real_
  out
}

weighted_mean_or_na <- function(x, w) {
  keep <- !(is.na(x) | is.na(w))

  if (!any(keep) || sum(w[keep]) <= 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x[keep], w[keep])
}

latex_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
  x
}

format_numeric_for_latex <- function(x, digits = 3L) {
  out <- character(length(x))
  out[is.na(x)] <- ""

  non_missing <- !is.na(x)
  integer_like <- non_missing & abs(x - round(x)) < 1e-9 & abs(x) >= 1
  small_value <- non_missing & !integer_like & x != 0 & abs(x) < 10^(-digits)
  regular_numeric <- non_missing & !integer_like & !small_value

  out[integer_like] <- format(
    round(x[integer_like]),
    trim = TRUE,
    scientific = FALSE,
    big.mark = ","
  )
  out[small_value] <- formatC(x[small_value], format = "e", digits = digits)
  out[regular_numeric] <- formatC(
    x[regular_numeric],
    format = "f",
    digits = digits,
    big.mark = ","
  )

  out
}

format_column_for_latex <- function(x, digits = 3L) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }

  if (is.numeric(x)) {
    return(format_numeric_for_latex(x, digits = digits))
  }

  if (is.logical(x)) {
    out <- ifelse(is.na(x), "", ifelse(x, "TRUE", "FALSE"))
    return(out)
  }

  as.character(x)
}

resolve_digits <- function(digits, column_name, default_digits = 3L) {
  if (length(digits) == 1L && is.null(names(digits))) {
    return(as.integer(digits[[1]]))
  }

  if (!is.null(names(digits)) && column_name %in% names(digits)) {
    return(as.integer(digits[[column_name]]))
  }

  as.integer(default_digits)
}

write_tex_table <- function(
  data,
  path,
  caption = NULL,
  label = NULL,
  digits = 3L,
  default_digits = 3L,
  align = NULL
) {
  table_df <- as.data.frame(data, stringsAsFactors = FALSE)

  if (ncol(table_df) == 0L) {
    stop("Cannot write a LaTeX table with zero columns.", call. = FALSE)
  }

  formatted_df <- table_df

  for (col_name in names(table_df)) {
    col_digits <- resolve_digits(
      digits = digits,
      column_name = col_name,
      default_digits = default_digits
    )
    formatted_df[[col_name]] <- latex_escape(
      format_column_for_latex(table_df[[col_name]], digits = col_digits)
    )
  }

  header <- latex_escape(names(formatted_df))
  body_lines <- apply(formatted_df, 1, function(row) {
    paste0(paste(row, collapse = " & "), " \\\\")
  })

  if (is.null(align)) {
    column_align <- ifelse(vapply(table_df, is.numeric, logical(1)), "r", "l")
    align <- paste(column_align, collapse = "")
  }

  lines <- c(
    "% Auto-generated table.",
    "\\begin{table}[!htbp]",
    "\\centering"
  )

  if (!is.null(caption)) {
    lines <- c(lines, paste0("\\caption{", latex_escape(caption), "}"))
  }

  if (!is.null(label)) {
    lines <- c(lines, paste0("\\label{", label, "}"))
  }

  lines <- c(
    lines,
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste0(paste(header, collapse = " & "), " \\\\"),
    "\\hline",
    body_lines,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(lines, con = path, useBytes = TRUE)
}

category_normalization_map <- data.frame(
  raw_label = c(
    "纪检监察与党风廉政",
    "公众服务信息",
    "文化活动与城市形象",
    "文化形象与城市活动",
    "政治与政务公开",
    "宣传教育与意识形态",
    "社会动员与社会参与",
    "纪委监察与党风廉政",
    "纪律监察与党风廉政"
  ),
  canonical_label = c(
    "意识形态与宣传教育",
    "公共服务信息",
    "城市形象与文化活动",
    "城市形象与文化活动",
    "政策与政务公开",
    "意识形态与宣传教育",
    "群众动员与社会参与",
    "意识形态与宣传教育",
    "意识形态与宣传教育"
  ),
  stringsAsFactors = FALSE
)

normalize_category <- function(category) {
  out <- as.character(category)
  lookup <- stats::setNames(
    category_normalization_map$canonical_label,
    category_normalization_map$raw_label
  )
  normalized <- unname(lookup[out])
  out[!is.na(normalized)] <- normalized[!is.na(normalized)]
  out
}

content_map <- data.frame(
  category = c(
    "意识形态与宣传教育",
    "时政与领导活动",
    "公共服务信息",
    "社会保障与公共福利",
    "应急管理与风险沟通",
    "政策与政务公开",
    "社会治理与执法通报",
    "群众动员与社会参与",
    "经济与发展建设",
    "城市形象与文化活动"
  ),
  content_group = c(
    "propaganda_hard",
    "propaganda_hard",
    "service",
    "service",
    "service",
    "state_governance",
    "state_governance",
    "state_governance",
    "propaganda_soft",
    "propaganda_soft"
  ),
  content_family = c(
    "hard_propaganda",
    "hard_propaganda",
    "public_service",
    "public_service",
    "public_service",
    "state_governance",
    "state_governance",
    "state_governance",
    "soft_propaganda",
    "soft_propaganda"
  ),
  stringsAsFactors = FALSE
)

classify_content_group <- function(category) {
  lookup <- stats::setNames(content_map$content_group, content_map$category)
  out <- unname(lookup[category])
  out[is.na(out)] <- "uncoded"
  out
}

classify_content_family <- function(category) {
  lookup <- stats::setNames(content_map$content_family, content_map$category)
  out <- unname(lookup[category])
  out[is.na(out)] <- "other"
  out
}
