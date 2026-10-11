# Report family contrasts implied by H1 and H2 from saved estimates.
# No models or original engagement counters are re-estimated or altered.
local({
  suppressPackageStartupMessages(library(fixest))
  design_root <- getwd()
  design_out <- file.path(design_root, 'analysis/output/observational')
  design_models <- readRDS(file.path(design_out, 'data/observational_results.rds'))
  contrast_weights <- list(
    service_minus_hard = c('family::public_service' = 1),
    service_minus_soft = c('family::public_service' = 1, 'family::soft_propaganda' = -1),
    soft_minus_hard = c('family::soft_propaganda' = 1))
  contrast_rows <- list()
  for (group in c('attention', 'reactions')) {
    for (i in seq_along(design_models[[group]])) {
      model <- design_models[[group]][[i]]
      coefficients <- coef(model)
      covariance <- vcov(model)
      df_t <- degrees_freedom(model, type = 't')
      scale <- if (group == 'reactions') 100 else 1
      for (name in names(contrast_weights)) {
        weights <- setNames(rep(0, length(coefficients)), names(coefficients))
        weights[names(contrast_weights[[name]])] <- contrast_weights[[name]]
        estimate <- sum(weights * coefficients)
        standard_error <- sqrt(drop(t(weights) %*% covariance %*% weights))
        contrast_rows[[length(contrast_rows) + 1L]] <- data.frame(
          group = group, model = i, contrast = name,
          units = if (group == 'reactions') 'percentage_points' else 'log_points',
          estimate = scale * estimate, standard_error = scale * standard_error,
          df = df_t, p_value = 2 * pt(-abs(estimate / standard_error), df_t),
          lower_95 = scale * (estimate - qt(.975, df_t) * standard_error),
          upper_95 = scale * (estimate + qt(.975, df_t) * standard_error),
          N = nobs(model))
      }
    }
  }
  contrasts <- do.call(rbind, contrast_rows)
  stopifnot(nrow(contrasts) == 18L,
            all(is.finite(contrasts$estimate)),
            all(is.finite(contrasts$standard_error)),
            all(contrasts$standard_error > 0))
  write.csv(contrasts, file.path(design_out, 'data/hypothesis_contrasts.csv'), row.names = FALSE)
  format_design <- function(x) sprintf('%.3f', x)
  stars_design <- function(p) ifelse(p < .01, '$^{**}$', ifelse(p < .05, '$^{*}$', ''))
  for (group in c('attention', 'reactions')) {
    file <- file.path(design_out, 'tables',
                      if (group == 'attention') 'attention_regressions.tex' else 'engagement_regressions.tex')
    lines <- readLines(file)
    start <- match('% DESIGN_CONTRAST_BEGIN', lines)
    end <- match('% DESIGN_CONTRAST_END', lines)
    if (!is.na(start) && !is.na(end)) lines <- lines[-seq.int(start, end)]
    rows <- contrasts[contrasts$group == group & contrasts$contrast == 'service_minus_soft', ]
    rows <- rows[order(rows$model), ]
    block <- c('% DESIGN_CONTRAST_BEGIN', '\\midrule',
               paste0('Service--soft contrast & ',
                      paste0(format_design(rows$estimate), stars_design(rows$p_value), collapse = ' & '), ' \\\\'),
               paste0(' & ', paste0('(', format_design(rows$standard_error), ')', collapse = ' & '), ' \\\\'),
               '% DESIGN_CONTRAST_END')
    index <- which(grepl('^Controls &', lines))
    stopifnot(length(index) == 1L)
    lines <- append(lines, block, after = index - 1L)
    note_index <- which(grepl('^\\\\item\\[\\] Notes:', lines))
    lines[note_index] <- sub(' \\* \\$p<0.05',
       ' The service--soft row is a linear contrast using the same clustered covariance and degrees of freedom. * $p<0.05',
       lines[note_index])
    # Keep repeated presentation regeneration idempotent.
    lines[note_index] <- gsub('( The service--soft row is a linear contrast using the same clustered covariance and degrees of freedom\\.){2,}',
       ' The service--soft row is a linear contrast using the same clustered covariance and degrees of freedom.', lines[note_index])
    writeLines(lines, file)
  }
  macros <- c(
    paste0('\\newcommand{\\DesignReadSoft}{', format_design(contrasts$estimate[contrasts$group == 'attention' & contrasts$model == 3 & contrasts$contrast == 'service_minus_soft']), '}'),
    paste0('\\newcommand{\\DesignReadSoftP}{', format_design(contrasts$p_value[contrasts$group == 'attention' & contrasts$model == 3 & contrasts$contrast == 'service_minus_soft']), '}'),
    paste0('\\newcommand{\\DesignLikeSoft}{', format_design(-contrasts$estimate[contrasts$group == 'reactions' & contrasts$model == 1 & contrasts$contrast == 'service_minus_soft']), '}'))
  writeLines(macros, file.path(design_out, 'tables/research_design_macros.tex'))
  proposal <- c(
    '\\begin{table}[H]\\centering\\setstretch{1}\\footnotesize',
    '\\caption{Proposed tests of peer visibility, operator visibility, and menu choice}\\label{tab:obsproposal}',
    '\\begin{threeparttable}',
    '\\begin{tabular}{@{}>{\\raggedright\\arraybackslash}p{.16\\linewidth}>{\\raggedright\\arraybackslash}p{.30\\linewidth}>{\\raggedright\\arraybackslash}p{.47\\linewidth}@{}}\\toprule',
    'Component & Comparison & What the comparison tests \\\\ \\midrule',
    'Peer visibility $P$ & Identifiable versus concealed reaction & H3: peer effect within each common message \\\\',
    'Operator visibility $O$ & Identified versus aggregate operator record & Peer effect at fixed operator visibility; institutional effects \\\\',
    'Content contrasts & Common items from each family across audience arms & H3: hard--soft and soft--service differences in peer effects \\\\',
    'Menu & Named-only versus the same named option plus count-only & Net participation and channel reallocation \\\\',
    '\\bottomrule\\end{tabular}',
    '\\renewcommand{\\TPTnoteSettings}{\\leftmargin0pt\\labelwidth0pt\\labelsep0pt\\topsep0pt\\partopsep0pt\\itemsep0pt\\parsep0pt}',
    '\\begin{tablenotes}\\footnotesize\\setstretch{1}\\justifying',
    '\\item[] Notes: Design proposals. Peer and operator visibility form a between-respondent $2\\times2$ factorial design. The message pool, reaction label, menu, and circulation are held fixed across audience arms. Each respondent evaluates multiple items in randomized order. Content rows compare randomized peer effects across message families; they do not randomize political salience independently of message content. The menu module uses an independent sample with mutually exclusive choices. No human-subject results or registration are claimed.',
    '\\end{tablenotes}\\end{threeparttable}\\end{table}')
  writeLines(proposal, file.path(design_out, 'tables/proposed_design.tex'))
  message('Reported 18 linear contrasts; regenerated H1/H2 contrast rows and the experimental design table.')
})
