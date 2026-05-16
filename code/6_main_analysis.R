#!/usr/bin/env Rscript
# Main confirmatory analysis: 3-way mixed ANOVA (emotion x repetition x group)
# per spatiotemporal cluster, with post-hoc Welch t-tests conditioned on the
# significant ANOVA effects to *interpret* (not just multiply) the findings:
#   - 3-way interaction significant     -> 4 between-group t-tests, one per
#                                          emotion x repetition cell
#   - 2-way interaction with group sig. -> 2 between-group t-tests, one per
#                                          level of the relevant within factor
#   - group main effect only sig.       -> 1 overall between-group t-test on
#                                          subject-mean amplitudes
#   - no group effect significant       -> no post-hoc tests reported
# Bonferroni alpha is 0.05 / k, where k is the number of simple-effect tests
# run for that cluster. Cluster-level Bonferroni alpha is 0.02 across the 3
# ANOVA families.
#
# ANOVA: afex::aov_ez (Type III; classical multi-stratum error terms).

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

ALPHA_CLUSTER <- 0.02
ALPHA_POSTHOC <- 0.05 / 4

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
  required <- c("amplitudes-tsv", "out-report", "out-prose-tex", "out-table-tex")
  missing <- setdiff(required, names(out))
  if (length(missing)) stop("Missing required args: ", paste(missing, collapse = ", "))
  out
}

# ---------------------------------------------------------------------------
# Formatting helpers (APA-style)
# ---------------------------------------------------------------------------
fmt_p <- function(p, digits = 3) {
  if (!is.finite(p)) return("n/a")
  if (p < 0.001) return("< .001")
  sub("0\\.", ".", sprintf(paste0("%.", digits, "f"), p))
}

fmt_p_apa <- function(p) {
  if (!is.finite(p)) return("n/a")
  if (p < 0.001) return("\\textit{p} < .001")
  paste0("\\textit{p} = ", sub("0\\.", ".", sprintf("%.3f", p)))
}

fmt_p_tex <- function(p) {
  if (!is.finite(p)) return("n/a")
  if (p < 0.001) return("$<$ .001")
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

fit_mixed_anova <- function(d) {
  # 3-way mixed ANOVA via afex::aov_ez (Type III, classical multi-stratum
  # error terms; partial eta-squared as effect size).
  fit <- afex::aov_ez(
    id = "participant_id", dv = "amplitude", data = d,
    within = c("emotion", "repetition"), between = "group",
    type = 3, anova_table = list(es = "pes")
  )
  tbl <- as.data.frame(fit$anova_table)
  tbl$Source <- rownames(tbl); rownames(tbl) <- NULL
  rename_map <- c(
    "group" = "group",
    "emotion" = "emotion",
    "repetition" = "repetition",
    "group:emotion" = "emotion x group",
    "group:repetition" = "repetition x group",
    "emotion:repetition" = "emotion x repetition",
    "group:emotion:repetition" = "emotion x repetition x group"
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
  desired <- c("group", "emotion", "repetition",
               "emotion x group", "repetition x group",
               "emotion x repetition", "emotion x repetition x group")
  tbl <- tbl[match(desired, tbl$Source), ]
  tbl$p_bonf <- pmin(tbl$p_unc * 3, 1.0)
  tbl$significant_at_alpha_0.02 <- tbl$p_unc < ALPHA_CLUSTER
  tbl
}

complete_subjects <- function(d_cluster) {
  d_cluster %>%
    filter(!is.na(amplitude)) %>%
    group_by(participant_id) %>%
    summarise(n_cells = n(), .groups = "drop") %>%
    filter(n_cells == 4) %>%
    pull(participant_id)
}

# Build one between-group Welch t-test row, given the per-subject amplitude
# averages within the cell. Returns a tibble with a single row.
welch_row <- function(d_sub, label) {
  a <- d_sub$amplitude[d_sub$group == "Lonely"]
  b <- d_sub$amplitude[d_sub$group == "Non-Lonely"]
  r <- welch_d(a, b)
  tibble(
    comparison = label,
    n_lonely = r$n_a, n_nonlonely = r$n_b,
    mean_lonely = r$mean_a, se_lonely = r$se_a,
    mean_nonlonely = r$mean_b, se_nonlonely = r$se_b,
    t = r$t, df = r$df, p = r$p, d = r$d
  )
}

# Decompose the cluster's significant ANOVA effects into between-group simple
# effects, per the user-confirmed convention:
#   - 3-way sig.            : 4 cells (emotion x repetition)
#   - 2-way w/ group sig.   : 2 simple effects per interaction
#                             (between-group at each level of the within factor)
#   - group main effect only: 1 overall comparison on subject-mean amplitudes
#   - nothing involving grp : NULL
# Bonferroni-correct the simple-effect family within the cluster (k tests).
decompose_interactions <- function(d, anova_tbl) {
  sig <- anova_tbl$Source[anova_tbl$significant_at_alpha_0.02]
  three_way <- "emotion x repetition x group" %in% sig
  rep_x_grp <- "repetition x group" %in% sig
  emo_x_grp <- "emotion x group" %in% sig
  grp_main  <- "group" %in% sig

  rows <- list()
  decomposition <- character()

  if (three_way) {
    decomposition <- c(decomposition, "3-way (emotion x repetition x group)")
    for (em in c("angry", "happy")) {
      for (rep in c("1", "5")) {
        cell <- d %>% filter(emotion == em, repetition == rep) %>%
          select(participant_id, group, amplitude)
        rows[[length(rows) + 1]] <- welch_row(
          cell, sprintf("Lonely vs Non-Lonely at %s, rep %s", em, rep))
      }
    }
  } else {
    if (rep_x_grp) {
      decomposition <- c(decomposition, "repetition x group")
      for (rep in c("1", "5")) {
        cell <- d %>% filter(repetition == rep) %>%
          group_by(participant_id, group) %>%
          summarise(amplitude = mean(amplitude), .groups = "drop")
        rows[[length(rows) + 1]] <- welch_row(
          cell, sprintf("Lonely vs Non-Lonely at rep %s", rep))
      }
    }
    if (emo_x_grp) {
      decomposition <- c(decomposition, "emotion x group")
      for (em in c("angry", "happy")) {
        cell <- d %>% filter(emotion == em) %>%
          group_by(participant_id, group) %>%
          summarise(amplitude = mean(amplitude), .groups = "drop")
        rows[[length(rows) + 1]] <- welch_row(
          cell, sprintf("Lonely vs Non-Lonely at %s", em))
      }
    }
    if (length(rows) == 0 && grp_main) {
      decomposition <- c(decomposition, "group main effect (overall)")
      cell <- d %>% group_by(participant_id, group) %>%
        summarise(amplitude = mean(amplitude), .groups = "drop")
      rows[[length(rows) + 1]] <- welch_row(
        cell, "Lonely vs Non-Lonely (overall)")
    }
  }

  if (length(rows) == 0) {
    return(list(rationale = NULL, tests = NULL, alpha = NA_real_))
  }
  tests <- bind_rows(rows)
  k <- nrow(tests)
  alpha <- 0.05 / k
  tests$alpha_bonf <- alpha
  tests$p_bonferroni <- pmin(tests$p * k, 1.0)
  tests$significant <- !is.na(tests$p) & tests$p < alpha
  list(rationale = decomposition, tests = tests, alpha = alpha)
}

# ---------------------------------------------------------------------------
# Report builders
# ---------------------------------------------------------------------------
render_markdown <- function(per_cluster, sample_info, out_path) {
  lines <- c(
    "# Main Analysis Report", "",
    "## Sample", "",
    sprintf("- Analytic sample (post-QC): **%d** participants (Lonely n = %d; Non-Lonely n = %d).",
            sample_info$n_total, sample_info$n_lonely, sample_info$n_nonlonely),
    ""
  )

  for (cl in per_cluster) {
    lines <- c(lines, sprintf("## %s", cl$name), "",
               sprintf("Effective n: %d", cl$n), "",
               "### Descriptives (mean amplitude, uV)", "",
               "| Group | Emotion | Rep | n | Mean | SE |",
               "|---|---|---|---|---|---|")
    for (i in seq_len(nrow(cl$desc))) {
      r <- cl$desc[i, ]
      lines <- c(lines, sprintf("| %s | %s | %s | %d | %s | %s |",
                                r$group, r$emotion, r$repetition, r$n,
                                fmt_num(r$mean, 3), fmt_num(r$se, 3)))
    }
    lines <- c(lines, "",
               "### Mixed ANOVA (3-way, afex::aov_ez, Type III SS)", "",
               "| Source | df1 | df2 | F | p | partial eta^2 | sig. (alpha<0.02) |",
               "|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(cl$anova))) {
      r <- cl$anova[i, ]
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                                r$Source, fmt_num(r$df1, 0), fmt_num(r$df2, 0),
                                fmt_num(r$F, 2), fmt_p(r$p_unc, 3),
                                fmt_eta2(r$pes),
                                if (isTRUE(r$significant_at_alpha_0.02)) "**yes**" else "no"))
    }
    if (!is.null(cl$posthoc$tests)) {
      lines <- c(lines, "",
                 "### Post-hoc simple effects (Welch t-tests, Lonely vs Non-Lonely)", "",
                 sprintf("Decomposing: %s. Bonferroni alpha (k=%d %s) = %.4f.",
                         paste(cl$posthoc$rationale, collapse = "; "),
                         nrow(cl$posthoc$tests),
                         if (nrow(cl$posthoc$tests) == 1) "test" else "tests",
                         cl$posthoc$alpha),
                 "",
                 "| Comparison | Lonely mean (SE) | Non-Lonely mean (SE) | t | df | p | p Bonf. | Cohen d | sig. |",
                 "|---|---|---|---|---|---|---|---|---|")
      for (i in seq_len(nrow(cl$posthoc$tests))) {
        r <- cl$posthoc$tests[i, ]
        lines <- c(lines, sprintf("| %s | %s (%s) | %s (%s) | %s | %s | %s | %s | %s | %s |",
                                  r$comparison,
                                  fmt_num(r$mean_lonely, 2), fmt_num(r$se_lonely, 2),
                                  fmt_num(r$mean_nonlonely, 2), fmt_num(r$se_nonlonely, 2),
                                  fmt_num(r$t, 2), fmt_num(r$df, 1),
                                  fmt_p(r$p, 3), fmt_p(r$p_bonferroni, 3),
                                  fmt_num(r$d, 2),
                                  if (isTRUE(r$significant)) "**yes**" else "no"))
      }
    } else {
      lines <- c(lines, "",
                 "### Post-hoc simple effects",
                 "",
                 "_No ANOVA effect involving group was significant at alpha < 0.02; no post-hoc tests reported._")
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "## Methods", "",
             "- Filter: passed QC (>= 50 epochs per angry/happy x rep 1/5; <= 4 bad channels; `exclude != TRUE`; valid group).",
             "- Per-cluster mean ERP amplitudes were extracted in Python (`5_extract_amplitudes.py`) and supplied as a long-format TSV.",
             "- 3-way mixed ANOVA via `afex::aov_ez` (Type III SS; classical multi-stratum error terms; partial eta-squared as effect size).",
             "- Post-hoc Welch t-tests are reported only when they help interpret a significant ANOVA effect: a significant 3-way interaction is decomposed into 4 cell-level Lonely vs Non-Lonely comparisons; a significant 2-way interaction with group is decomposed into between-group comparisons at each level of the relevant within factor; a significant group main effect (in the absence of any group-involving interaction) is reported as one overall Lonely vs Non-Lonely comparison on subject-mean amplitudes; if no group-involving effect is significant, no post-hoc tests are reported.",
             sprintf("- Bonferroni correction: alpha < %.2f across the 3 clusters (ANOVA effects); within each cluster, alpha = 0.05 / k where k is the number of simple-effect tests for that cluster.",
                     ALPHA_CLUSTER),
             "")
  writeLines(lines, out_path)
}

render_table_tex <- function(per_cluster, out_path) {
  lines <- c(
    "% Requires: booktabs, xltabular, array.",
    "\\begingroup", "\\scriptsize",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{1.1}",
    paste0("\\begin{xltabular}{\\textwidth}{@{}",
           ">{\\raggedright\\arraybackslash}p{4.0cm}",
           ">{\\centering\\arraybackslash}p{0.8cm}",
           ">{\\centering\\arraybackslash}p{0.8cm}",
           ">{\\centering\\arraybackslash}p{1.2cm}",
           ">{\\centering\\arraybackslash}p{1.4cm}",
           ">{\\centering\\arraybackslash}p{1.4cm}",
           ">{\\centering\\arraybackslash}X@{}}"),
    "\\caption{3-way mixed ANOVA (emotion $\\times$ repetition $\\times$ group) per spatiotemporal cluster, with post-hoc Welch $t$-tests reported only for clusters in which an ANOVA effect involving group was significant at $\\alpha<0.02$ (Bonferroni-corrected across the three clusters). Each post-hoc family is Bonferroni-corrected within the cluster at $\\alpha=0.05/k$, where $k$ is the number of simple-effect comparisons used to decompose the significant interaction (or 1, for an isolated group main effect).} \\\\",
    "\\label{tab:main_analysis} \\\\",
    "\\toprule",
    "\\textbf{Source} & \\textbf{df1} & \\textbf{df2} & \\textbf{F} & \\textbf{p} & \\textbf{$\\eta^{2}_{p}$} & \\textbf{Sig.} \\\\",
    "\\midrule \\endfirsthead",
    "\\toprule",
    "\\textbf{Source} & \\textbf{df1} & \\textbf{df2} & \\textbf{F} & \\textbf{p} & \\textbf{$\\eta^{2}_{p}$} & \\textbf{Sig.} \\\\",
    "\\midrule \\endhead",
    "\\bottomrule \\endlastfoot"
  )

  for (cl in per_cluster) {
    lines <- c(lines, sprintf("\\multicolumn{7}{@{}l}{\\textbf{%s} (effective $n=%d$)} \\\\",
                              cl$name, cl$n))
    for (i in seq_len(nrow(cl$anova))) {
      r <- cl$anova[i, ]
      sig <- if (isTRUE(r$significant_at_alpha_0.02)) "\\textbf{*}" else ""
      lines <- c(lines, sprintf("\\hspace{1em}%s & %s & %s & %s & %s & %s & %s \\\\",
                                r$Source, fmt_num(r$df1, 0), fmt_num(r$df2, 0),
                                fmt_num(r$F, 2), fmt_p_tex(r$p_unc),
                                fmt_eta2(r$pes), sig))
    }
    if (!is.null(cl$posthoc$tests)) {
      lines <- c(lines, "\\addlinespace",
                 sprintf("\\multicolumn{7}{@{}l}{\\textit{Post-hoc simple effects (Welch $t$, Bonf. $\\alpha=%.4f$): %s}} \\\\",
                         cl$posthoc$alpha,
                         paste(cl$posthoc$rationale, collapse = "; ")),
                 "\\hspace{1em}\\textbf{Comparison} & \\multicolumn{2}{c}{\\textbf{t (df)}} & \\textbf{p} & \\textbf{p Bonf.} & \\textbf{d} & \\textbf{Sig.} \\\\")
      for (i in seq_len(nrow(cl$posthoc$tests))) {
        r <- cl$posthoc$tests[i, ]
        sig <- if (isTRUE(r$significant)) "\\textbf{*}" else ""
        lines <- c(lines, sprintf("\\hspace{1em}%s & \\multicolumn{2}{c}{%s (%s)} & %s & %s & %s & %s \\\\",
                                  r$comparison,
                                  fmt_num(r$t, 2), fmt_num(r$df, 1),
                                  fmt_p_tex(r$p), fmt_p_tex(r$p_bonferroni),
                                  fmt_num(r$d, 2), sig))
      }
    }
    lines <- c(lines, "\\addlinespace")
  }

  lines <- c(lines, "\\end{xltabular}", "\\endgroup")
  writeLines(lines, out_path)
}

render_prose_tex <- function(per_cluster, sample_info, out_path) {
  lines <- c(sprintf(
    paste("Mean event-related potential (ERP) amplitudes were averaged",
          "over the electrodes and time window of each pre-registered",
          "spatiotemporal cluster and submitted to a $2 \\times 2 \\times 2$",
          "mixed analysis of variance (ANOVA) with the within-subject factors",
          "emotion (angry, happy) and repetition (1st, 5th) and the",
          "between-subject factor group (lonely, non-lonely). Significance",
          "was evaluated at a Bonferroni-corrected threshold of",
          "$\\alpha < %.2f$ across the three clusters. The analytic sample",
          "comprised %d participants (lonely $n=%d$; non-lonely $n=%d$).",
          sep = " "),
    ALPHA_CLUSTER, sample_info$n_total,
    sample_info$n_lonely, sample_info$n_nonlonely))

  describe_anova <- function(cl) {
    sig_terms <- cl$anova[cl$anova$significant_at_alpha_0.02, ]
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
        "did not reveal any effect that survived Bonferroni correction at $\\alpha < ",
        sprintf("%.2f", ALPHA_CLUSTER), "$"
      )
    }

    grp_row <- cl$anova[cl$anova$Source == "group", ]
    grp_clause <- if (nrow(grp_row) == 1 && !isTRUE(grp_row$significant_at_alpha_0.02)) {
      sprintf("; the group main effect was %s",
              apa_f(grp_row$F, grp_row$df1, grp_row$df2,
                    grp_row$p_unc, grp_row$pes))
    } else ""

    ph <- cl$posthoc
    if (is.null(ph$tests)) {
      ph_sentence <- ""  # no group-involving ANOVA effect to decompose
    } else {
      sig_ph <- ph$tests[!is.na(ph$tests$significant) & ph$tests$significant, ]
      if (nrow(sig_ph) > 0) {
        parts <- character()
        for (i in seq_len(nrow(sig_ph))) {
          r <- sig_ph[i, ]
          direction <- if (r$mean_lonely < r$mean_nonlonely)
            "more negative" else "more positive"
          parts <- c(parts,
                     sprintf("%s (%s; lonely %s than non-lonely)",
                             r$comparison, apa_t(r$t, r$df, r$p, r$d),
                             direction))
        }
        ph_sentence <- paste0(
          " Decomposing this with Welch $t$-tests (Bonferroni $\\alpha=",
          sprintf("%.4f", ph$alpha), "$), groups differed for ",
          paste(parts, collapse = "; "), "."
        )
      } else {
        nearest <- ph$tests[which.min(ph$tests$p), ]
        nearest_dir <- if (nearest$mean_lonely < nearest$mean_nonlonely)
          "more negative" else "more positive"
        if (nrow(ph$tests) == 1) {
          # Single overall comparison: just report it.
          ph_sentence <- paste0(
            " The corresponding Welch $t$-test was ",
            apa_t(nearest$t, nearest$df, nearest$p, nearest$d),
            " (lonely ", nearest_dir, " than non-lonely)."
          )
        } else {
          ph_sentence <- paste0(
            " Decomposing this with Welch $t$-tests (Bonferroni $\\alpha=",
            sprintf("%.4f", ph$alpha),
            "$) did not isolate a specific level driving the effect; the ",
            "largest difference was at ",
            sprintf("%s (%s; lonely %s than non-lonely)", nearest$comparison,
                    apa_t(nearest$t, nearest$df, nearest$p, nearest$d),
                    nearest_dir), "."
          )
        }
      }
    }

    paste0("In the ", cl$name, " cluster (effective $n=", cl$n, "$), the mixed ANOVA ",
           effect_sentence, grp_clause, ".", ph_sentence)
  }

  for (cl in per_cluster) {
    lines <- c(lines, "", describe_anova(cl))
  }
  writeLines(lines, out_path)
}

# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------
main <- function() {
  args <- parse_args()
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Reading ",
      args[["amplitudes-tsv"]], "\n", sep = "")
  amps <- read_tsv(args[["amplitudes-tsv"]], comment = "#",
                   show_col_types = FALSE)
  amps$repetition <- as.character(amps$repetition)

  subjects <- amps %>% distinct(participant_id, group)
  sample_info <- list(
    n_total = nrow(subjects),
    n_lonely = sum(subjects$group == "Lonely"),
    n_nonlonely = sum(subjects$group == "Non-Lonely")
  )
  cat("Sample: total=", sample_info$n_total,
      " lonely=", sample_info$n_lonely,
      " non-lonely=", sample_info$n_nonlonely, "\n", sep = "")

  cluster_names <- unique(amps$cluster)
  per_cluster <- list()
  for (cn in cluster_names) {
    cat("[", format(Sys.time(), "%H:%M:%S"), "] Cluster: ", cn, "\n", sep = "")
    d_cluster <- amps %>% filter(cluster == cn)
    keep_ids <- complete_subjects(d_cluster)
    d <- d_cluster %>%
      filter(participant_id %in% keep_ids) %>%
      mutate(
        emotion = factor(emotion, levels = c("angry", "happy")),
        repetition = factor(repetition, levels = c("1", "5")),
        group = factor(group, levels = c("Lonely", "Non-Lonely")),
        participant_id = factor(participant_id)
      )
    anova_tbl <- fit_mixed_anova(d)
    ph <- decompose_interactions(d, anova_tbl)
    desc_tbl <- d %>%
      group_by(group, emotion, repetition) %>%
      summarise(n = n(), mean = mean(amplitude),
                se = sd(amplitude) / sqrt(n()), .groups = "drop") %>%
      arrange(group, emotion, repetition)
    per_cluster[[length(per_cluster) + 1]] <- list(
      name = cn, n = length(keep_ids),
      anova = anova_tbl, posthoc = ph, desc = desc_tbl
    )
  }

  out_report <- args[["out-report"]]
  out_prose <- args[["out-prose-tex"]]
  out_table <- args[["out-table-tex"]]
  dir.create(dirname(out_report), recursive = TRUE, showWarnings = FALSE)

  render_markdown(per_cluster, sample_info, out_report)
  cat("Wrote ", out_report, "\n", sep = "")
  render_table_tex(per_cluster, out_table)
  cat("Wrote ", out_table, "\n", sep = "")
  render_prose_tex(per_cluster, sample_info, out_prose)
  cat("Wrote ", out_prose, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
