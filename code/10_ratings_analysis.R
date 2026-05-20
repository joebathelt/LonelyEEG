#!/usr/bin/env Rscript
# Image-ratings analysis: 2-way mixed ANOVA (emotion x group) per rating
# dimension (arousal, dominance, valence), with follow-up between-group Welch
# t-tests within each emotion (Lonely vs Non-Lonely at angry; and at happy).
# This is an exploratory secondary analysis to complement the main ERP
# analysis in 6_main_analysis.R; the participant-inclusion filter is the
# same as for the main analysis.

suppressPackageStartupMessages({
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (nzchar(user_lib) && dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(afex)
})
afex::afex_options(check_contrasts = FALSE)

ALPHA <- 0.05  # exploratory threshold; applied to ANOVA and follow-ups alike

DIMENSIONS <- c("arousal", "dominance", "valence")
EMOTIONS <- c("angry", "happy")

# ---------------------------------------------------------------------------
# CLI parsing
# ---------------------------------------------------------------------------
parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list(); i <- 1
  while (i <= length(args)) {
    key <- args[[i]]; val <- args[[i + 1]]
    out[[sub("^--", "", key)]] <- val
    i <- i + 2
  }
  required <- c("ratings-tsv", "out-report", "out-prose-tex", "out-table-tex")
  missing <- setdiff(required, names(out))
  if (length(missing)) stop("Missing required args: ", paste(missing, collapse = ", "))
  out
}

# ---------------------------------------------------------------------------
# Formatting helpers (APA-style; copied from 6_main_analysis.R)
# ---------------------------------------------------------------------------
fmt_p_sci_parts <- function(p, digits = 2) {
  parts <- strsplit(formatC(p, format = "e", digits = digits), "e")[[1]]
  list(m = parts[1], e = as.integer(parts[2]))
}

fmt_p <- function(p, digits = 3) {
  if (!is.finite(p)) return("n/a")
  if (p < 0.001) return(formatC(p, format = "e", digits = 2))
  sub("0\\.", ".", sprintf(paste0("%.", digits, "f"), p))
}

fmt_p_apa <- function(p) {
  if (!is.finite(p)) return("n/a")
  if (p < 0.001) {
    s <- fmt_p_sci_parts(p)
    return(sprintf("\\textit{p} = $%s \\times 10^{%d}$", s$m, s$e))
  }
  paste0("\\textit{p} = ", sub("0\\.", ".", sprintf("%.3f", p)))
}

fmt_p_tex <- function(p) {
  if (!is.finite(p)) return("n/a")
  if (p < 0.001) {
    s <- fmt_p_sci_parts(p)
    return(sprintf("$%s \\times 10^{%d}$", s$m, s$e))
  }
  sub("0\\.", ".", sprintf("%.3f", p))
}

fmt_num <- function(x, digits = 2) {
  if (!is.finite(x)) return("n/a")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_eta2 <- function(x) {
  if (!is.finite(x)) return("n/a")
  sub("0\\.", ".", sprintf("%.3f", x))
}

apa_f <- function(F_val, df1, df2, p, eta2) {
  paste0("\\textit{F}(", df1, ", ", df2, ") = ", fmt_num(F_val, 2),
         ", ", fmt_p_apa(p), ", $\\eta^{2}_{p}$ = ", fmt_eta2(eta2))
}

apa_t <- function(t_val, df, p, d) {
  paste0("\\textit{t}(", fmt_num(df, 1), ") = ", fmt_num(t_val, 2),
         ", ", fmt_p_apa(p), ", \\textit{d} = ", fmt_num(d, 2))
}

# ---------------------------------------------------------------------------
# Statistics
# ---------------------------------------------------------------------------
welch_d <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  na <- length(a); nb <- length(b)
  if (na < 2 || nb < 2) {
    return(list(t = NA_real_, df = NA_real_, p = NA_real_, d = NA_real_,
                mean_a = if (na) mean(a) else NA_real_,
                mean_b = if (nb) mean(b) else NA_real_,
                se_a = if (na > 1) sd(a) / sqrt(na) else NA_real_,
                se_b = if (nb > 1) sd(b) / sqrt(nb) else NA_real_,
                n_a = na, n_b = nb))
  }
  tt <- t.test(a, b, var.equal = FALSE)
  va <- var(a); vb <- var(b)
  s_pool <- sqrt(((na - 1) * va + (nb - 1) * vb) / (na + nb - 2))
  d <- if (s_pool == 0) NA_real_ else (mean(a) - mean(b)) / s_pool
  list(t = unname(tt$statistic), df = unname(tt$parameter),
       p = unname(tt$p.value), d = d,
       mean_a = mean(a), mean_b = mean(b),
       se_a = sd(a) / sqrt(na), se_b = sd(b) / sqrt(nb),
       n_a = na, n_b = nb)
}

fit_mixed_anova_2way <- function(d) {
  # 2-way mixed ANOVA via afex::aov_ez (Type III, classical multi-stratum
  # error terms; partial eta-squared as effect size).
  fit <- afex::aov_ez(
    id = "participant_id", dv = "rating_mean", data = d,
    within = "emotion", between = "group",
    type = 3, anova_table = list(es = "pes")
  )
  tbl <- as.data.frame(fit$anova_table)
  tbl$Source <- rownames(tbl); rownames(tbl) <- NULL
  rename_map <- c(
    "group" = "group",
    "emotion" = "emotion",
    "group:emotion" = "emotion x group"
  )
  tbl$Source <- ifelse(tbl$Source %in% names(rename_map),
                       rename_map[tbl$Source], tbl$Source)
  tbl <- tibble(
    Source = tbl$Source,
    df1 = tbl[["num Df"]],
    df2 = tbl[["den Df"]],
    F = tbl[["F"]],
    p_unc = tbl[["Pr(>F)"]],
    pes = tbl[["pes"]]
  )
  desired <- c("group", "emotion", "emotion x group")
  tbl <- tbl[match(desired, tbl$Source), ]
  tbl$significant <- !is.na(tbl$p_unc) & tbl$p_unc < ALPHA
  tbl
}

# Require both emotion cells present for a participant within a given
# dimension's sub-frame; otherwise the within-subject ANOVA can't include them.
complete_subjects <- function(d_dim) {
  d_dim %>%
    filter(!is.na(rating_mean)) %>%
    group_by(participant_id) %>%
    summarise(n_cells = n(), .groups = "drop") %>%
    filter(n_cells == length(EMOTIONS)) %>%
    pull(participant_id)
}

welch_row <- function(d_cell, label) {
  a <- d_cell$rating_mean[d_cell$group == "Lonely"]
  b <- d_cell$rating_mean[d_cell$group == "Non-Lonely"]
  r <- welch_d(a, b)
  tibble(
    comparison = label,
    n_lonely = r$n_a, n_nonlonely = r$n_b,
    mean_lonely = r$mean_a, se_lonely = r$se_a,
    mean_nonlonely = r$mean_b, se_nonlonely = r$se_b,
    t = r$t, df = r$df, p = r$p, d = r$d
  )
}

# Always report Lonely vs Non-Lonely within angry and within happy. These are
# the comparisons the user asked for, regardless of which ANOVA term is sig.
within_emotion_tests <- function(d) {
  rows <- list()
  for (em in EMOTIONS) {
    cell <- d %>% filter(emotion == em) %>%
      select(participant_id, group, rating_mean)
    rows[[length(rows) + 1]] <- welch_row(
      cell, sprintf("Lonely vs Non-Lonely at %s", em))
  }
  tests <- bind_rows(rows)
  tests$significant <- !is.na(tests$p) & tests$p < ALPHA
  tests
}

# ---------------------------------------------------------------------------
# Report builders
# ---------------------------------------------------------------------------
render_markdown <- function(per_dim, sample_info, out_path) {
  lines <- c(
    "# Image Ratings Analysis Report", "",
    "## Sample", "",
    sprintf("- Analytic sample (post-QC, same filter as main analysis): **%d** participants (Lonely n = %d; Non-Lonely n = %d).",
            sample_info$n_total, sample_info$n_lonely, sample_info$n_nonlonely),
    ""
  )

  for (dim in per_dim) {
    lines <- c(lines, sprintf("## %s", str_to_title(dim$name)), "",
               sprintf("Effective n: %d", dim$n), "",
               "### Descriptives (mean rating, 1-5)", "",
               "| Group | Emotion | n | Mean | SE |",
               "|---|---|---|---|---|")
    for (i in seq_len(nrow(dim$desc))) {
      r <- dim$desc[i, ]
      lines <- c(lines, sprintf("| %s | %s | %d | %s | %s |",
                                r$group, r$emotion, r$n,
                                fmt_num(r$mean, 2), fmt_num(r$se, 2)))
    }
    lines <- c(lines, "",
               "### Mixed ANOVA (2-way, afex::aov_ez, Type III SS)", "",
               sprintf("| Source | df1 | df2 | F | p | partial eta^2 | sig. (alpha<%.2f) |",
                       ALPHA),
               "|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(dim$anova))) {
      r <- dim$anova[i, ]
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                                r$Source, fmt_num(r$df1, 0), fmt_num(r$df2, 0),
                                fmt_num(r$F, 2), fmt_p(r$p_unc, 3),
                                fmt_eta2(r$pes),
                                if (isTRUE(r$significant)) "**yes**" else "no"))
    }
    lines <- c(lines, "",
               "### Between-group comparisons within each emotion (Welch t-tests)", "",
               sprintf("Reported regardless of ANOVA outcome (alpha = %.2f, uncorrected).",
                       ALPHA),
               "",
               "| Comparison | Lonely mean (SE) | Non-Lonely mean (SE) | t | df | p | Cohen d | sig. |",
               "|---|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(dim$tests))) {
      r <- dim$tests[i, ]
      lines <- c(lines, sprintf("| %s | %s (%s) | %s (%s) | %s | %s | %s | %s | %s |",
                                r$comparison,
                                fmt_num(r$mean_lonely, 2), fmt_num(r$se_lonely, 2),
                                fmt_num(r$mean_nonlonely, 2), fmt_num(r$se_nonlonely, 2),
                                fmt_num(r$t, 2), fmt_num(r$df, 1),
                                fmt_p(r$p, 3),
                                fmt_num(r$d, 2),
                                if (isTRUE(r$significant)) "**yes**" else "no"))
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "## Methods", "",
             "- Sample filter: identical to the main ERP analysis (>= 50 epochs per angry/happy x rep 1/5; <= 4 bad channels; `exclude != TRUE`; valid group).",
             "- Per-participant rating means were extracted in Python (`9_extract_ratings.py`) by averaging trial-level ratings within each (emotion, dimension) cell, and supplied as a long-format TSV.",
             "- For each rating dimension (arousal, dominance, valence): a 2-way mixed ANOVA via `afex::aov_ez` (Type III SS; classical multi-stratum error terms; partial eta-squared as effect size) with emotion (angry, happy) as the within-subject factor and group (Lonely, Non-Lonely) as the between-subject factor; followed by two between-group Welch t-tests on subject means within each emotion. Cohen's d is computed from the pooled within-group standard deviation.",
             sprintf("- Significance threshold: alpha < %.2f, applied uncorrected to ANOVA terms and to follow-up t-tests. This is an exploratory secondary analysis (not pre-registered); no correction across the three dimensions.",
                     ALPHA),
             "- Only participants with non-missing means in both emotion cells (within a given dimension) were included in that dimension's analysis.",
             "")
  writeLines(lines, out_path)
}

render_table_tex <- function(per_dim, out_path) {
  # APA-style ANOVA table mirroring 6_main_analysis.R's render_table_tex.
  note_text <- sprintf(paste0(
    "\\textit{Note.} 2-way mixed ANOVA fitted with \\texttt{afex::aov\\_ez} ",
    "(Type III sums of squares; classical multi-stratum error terms; ",
    "partial $\\eta^{2}$ as effect size). Asterisks mark effects ",
    "significant at $\\alpha<%.2f$ (uncorrected; exploratory). ",
    "\\textit{p}-values below $10^{-3}$ are reported in scientific notation."),
    ALPHA)

  lines <- c(
    "% Requires: booktabs, xltabular, array.",
    "\\begingroup", "\\scriptsize",
    "\\setlength{\\tabcolsep}{6pt}",
    "\\renewcommand{\\arraystretch}{1.15}",
    paste0("\\begin{xltabular}{\\textwidth}{@{}",
           ">{\\raggedright\\arraybackslash}X",
           ">{\\centering\\arraybackslash}p{1.4cm}",
           ">{\\centering\\arraybackslash}p{1.4cm}",
           ">{\\centering\\arraybackslash}p{2.4cm}",
           ">{\\centering\\arraybackslash}p{1.2cm}@{}}"),
    sprintf("\\caption{2-way mixed ANOVA (emotion $\\times$ group) on image ratings (arousal, dominance, valence). Exploratory; effects evaluated at $\\alpha<%.2f$ uncorrected.} \\\\",
            ALPHA),
    "\\label{tab:ratings_analysis} \\\\",
    "\\toprule",
    "\\textbf{Source} & \\textit{df} & \\textit{F} & \\textit{p} & $\\eta^{2}_{p}$ \\\\",
    "\\midrule \\endfirsthead",
    "\\toprule",
    "\\textbf{Source} & \\textit{df} & \\textit{F} & \\textit{p} & $\\eta^{2}_{p}$ \\\\",
    "\\midrule \\endhead",
    "\\bottomrule",
    sprintf("\\multicolumn{5}{@{}p{\\textwidth}@{}}{%s} \\\\", note_text),
    "\\endlastfoot"
  )

  for (dim in per_dim) {
    lines <- c(lines, sprintf("\\multicolumn{5}{@{}l}{\\textbf{%s} (effective $n=%d$)} \\\\",
                              str_to_title(dim$name), dim$n))
    for (i in seq_len(nrow(dim$anova))) {
      r <- dim$anova[i, ]
      F_str <- fmt_num(r$F, 2)
      if (isTRUE(r$significant)) F_str <- paste0(F_str, "*")
      df_str <- sprintf("%s, %s", fmt_num(r$df1, 0), fmt_num(r$df2, 0))
      lines <- c(lines, sprintf("\\hspace{1em}%s & %s & %s & %s & %s \\\\",
                                r$Source, df_str,
                                F_str, fmt_p_tex(r$p_unc),
                                fmt_eta2(r$pes)))
    }
    lines <- c(lines, "\\addlinespace")
  }

  lines <- c(lines, "\\end{xltabular}", "\\endgroup")
  writeLines(lines, out_path)
}

render_prose_tex <- function(per_dim, sample_info, out_path) {
  lines <- c(sprintf(
    paste("As a secondary, exploratory analysis, subjective image ratings",
          "(arousal, dominance, valence; 1--5 manikin scale) were compared",
          "between the lonely and non-lonely groups for angry and happy",
          "faces. Per-participant rating means were computed by averaging",
          "trial-level ratings within each emotion $\\times$ dimension cell.",
          "For each rating dimension, these means were submitted to a",
          "$2 \\times 2$ mixed ANOVA with emotion (angry, happy) as the",
          "within-subject factor and group (lonely, non-lonely) as the",
          "between-subject factor, fitted with \\texttt{aov\\_ez} from the",
          "\\texttt{afex} package in \\texttt{R} using Type III sums of",
          "squares and classical multi-stratum error terms. Partial",
          "$\\eta^{2}$ is reported as the effect size for each ANOVA term.",
          "Two between-group Welch $t$-tests on subject means (Lonely vs.",
          "Non-Lonely at angry, and at happy) were reported regardless of",
          "the ANOVA outcome to address the planned comparison. Welch",
          "$t$-tests used the Satterthwaite degrees of freedom, and",
          "Cohen's $d$ was computed from the pooled within-group standard",
          "deviation. Effects were evaluated at $\\alpha<%.2f$ uncorrected;",
          "no correction was applied across the three rating dimensions.",
          "Only participants with non-missing means in both emotion cells",
          "(within a given dimension) were included in that dimension's",
          "analysis. The participant-inclusion filter was identical to the",
          "main ERP analysis. \\textit{p}-values below $10^{-3}$ are",
          "reported in scientific notation. The analytic sample comprised",
          "%d participants (lonely $n=%d$; non-lonely $n=%d$).",
          sep = " "),
    ALPHA,
    sample_info$n_total, sample_info$n_lonely, sample_info$n_nonlonely))

  describe_dim <- function(dim) {
    sig_terms <- dim$anova[dim$anova$significant, ]
    if (nrow(sig_terms) > 0) {
      sentences <- character()
      for (i in seq_len(nrow(sig_terms))) {
        r <- sig_terms[i, ]
        sentences <- c(sentences,
                       sprintf("a significant effect of %s (%s)",
                               r$Source, apa_f(r$F, r$df1, r$df2,
                                               r$p_unc, r$pes)))
      }
      if (length(sentences) == 1) {
        effect_sentence <- paste0("revealed ", sentences[[1]])
      } else {
        effect_sentence <- paste0("revealed ",
                                   paste(sentences[-length(sentences)], collapse = ", "),
                                   " and ", sentences[[length(sentences)]])
      }
    } else {
      effect_sentence <- paste0(
        "did not reveal any effect significant at $\\alpha < ",
        sprintf("%.2f", ALPHA), "$"
      )
    }

    grp_row <- dim$anova[dim$anova$Source == "group", ]
    grp_clause <- if (nrow(grp_row) == 1 && !isTRUE(grp_row$significant)) {
      sprintf("; the group main effect was %s",
              apa_f(grp_row$F, grp_row$df1, grp_row$df2,
                    grp_row$p_unc, grp_row$pes))
    } else ""

    parts <- character()
    for (i in seq_len(nrow(dim$tests))) {
      r <- dim$tests[i, ]
      direction <- if (is.na(r$mean_lonely) || is.na(r$mean_nonlonely)) {
        "n/a"
      } else if (r$mean_lonely < r$mean_nonlonely) {
        "lower in lonely than non-lonely"
      } else {
        "higher in lonely than non-lonely"
      }
      verdict <- if (isTRUE(r$significant)) "significant" else "not significant"
      parts <- c(parts,
                 sprintf("%s (%s; %s; %s)", r$comparison,
                         apa_t(r$t, r$df, r$p, r$d), direction, verdict))
    }
    ph_sentence <- paste0(" Between-group Welch $t$-tests within each emotion: ",
                          paste(parts, collapse = "; "), ".")

    paste0("For ", dim$name,
           " ratings (effective $n=", dim$n, "$), the mixed ANOVA ",
           effect_sentence, grp_clause, ".", ph_sentence)
  }

  for (dim in per_dim) {
    lines <- c(lines, "", describe_dim(dim))
  }
  writeLines(lines, out_path)
}

# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------
main <- function() {
  args <- parse_args()
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Reading ",
      args[["ratings-tsv"]], "\n", sep = "")
  ratings <- read_tsv(args[["ratings-tsv"]], comment = "#",
                      show_col_types = FALSE)

  subjects <- ratings %>% distinct(participant_id, group)
  sample_info <- list(
    n_total = nrow(subjects),
    n_lonely = sum(subjects$group == "Lonely"),
    n_nonlonely = sum(subjects$group == "Non-Lonely")
  )
  cat("Sample: total=", sample_info$n_total,
      " lonely=", sample_info$n_lonely,
      " non-lonely=", sample_info$n_nonlonely, "\n", sep = "")

  per_dim <- list()
  for (dim_name in DIMENSIONS) {
    cat("[", format(Sys.time(), "%H:%M:%S"), "] Dimension: ", dim_name, "\n", sep = "")
    d_dim <- ratings %>% filter(dimension == dim_name)
    keep_ids <- complete_subjects(d_dim)
    d <- d_dim %>%
      filter(participant_id %in% keep_ids) %>%
      mutate(
        emotion = factor(emotion, levels = EMOTIONS),
        group = factor(group, levels = c("Lonely", "Non-Lonely")),
        participant_id = factor(participant_id)
      )
    anova_tbl <- fit_mixed_anova_2way(d)
    tests <- within_emotion_tests(d)
    desc_tbl <- d %>%
      group_by(group, emotion) %>%
      summarise(n = n(), mean = mean(rating_mean),
                se = sd(rating_mean) / sqrt(n()), .groups = "drop") %>%
      arrange(group, emotion)
    per_dim[[length(per_dim) + 1]] <- list(
      name = dim_name, n = length(keep_ids),
      anova = anova_tbl, tests = tests, desc = desc_tbl
    )
  }

  out_report <- args[["out-report"]]
  out_prose <- args[["out-prose-tex"]]
  out_table <- args[["out-table-tex"]]
  dir.create(dirname(out_report), recursive = TRUE, showWarnings = FALSE)

  render_markdown(per_dim, sample_info, out_report)
  cat("Wrote ", out_report, "\n", sep = "")
  render_table_tex(per_dim, out_table)
  cat("Wrote ", out_table, "\n", sep = "")
  render_prose_tex(per_dim, sample_info, out_prose)
  cat("Wrote ", out_prose, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
