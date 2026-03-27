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
    "development_culture",
    "development_culture"
  ),
  content_family = c(
    "propaganda",
    "propaganda",
    "public_service",
    "public_service",
    "public_service",
    "other",
    "other",
    "other",
    "other",
    "other"
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
