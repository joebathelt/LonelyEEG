#!/usr/bin/env Rscript
# Main confirmatory analysis: 3-way mixed ANOVA (emotion x repetition x group)
# per spatiotemporal cluster, with post-hoc Welch t-tests conditioned on the
# significant ANOVA effects to *interpret* (not just multiply) the findings:
#   - 3-way interaction significant     -> 2 pre-registered between-group
#                                          t-tests at angry x rep 1 (H1) and
#                                          angry x rep 5 (H2)
#   - 2-way interaction with group sig. -> 2 between-group t-tests, one per
#                                          level of the relevant within factor
#                                          (exploratory; not part of H1/H2)
#   - group main effect only sig.       -> 1 overall between-group t-test on
#                                          subject-mean amplitudes
#                                          (exploratory; not part of H1/H2)
#   - no group effect significant       -> no post-hoc tests reported
# Significance criteria: ANOVA terms are evaluated at p < 0.02 directly (no
# further correction across clusters); the within-cluster post-hoc family uses
# the same baseline of 0.02 with Bonferroni applied on top, giving 0.02 / k
# where k is the number of simple-effect tests for that cluster.
#
# Hypothesis verdicts. H1 (first presentation of angry faces) and H2 (fifth
# presentation of angry faces) are *confirmed* in a cluster when all three
# criteria hold:
#   (1) the 3-way emotion x repetition x group interaction is significant at
#       the cluster-corrected threshold;
#   (2) the corresponding pre-registered Welch t-test (angry, rep 1 for H1;
#       angry, rep 5 for H2) is significant at the within-cluster Bonferroni
#       threshold; and
#   (3) the mean amplitude is higher (more positive, signed) in the lonely
#       than in the non-lonely group.
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

ALPHA_CLUSTER <- 0.02  # ANOVA threshold (no further correction across clusters)
ALPHA_POSTHOC_BASE <- 0.02  # baseline for within-cluster post-hoc family
ALPHA_POSTHOC_THREEWAY <- ALPHA_POSTHOC_BASE / 2  # k=2 pre-registered tests (angry rep 1, rep 5)

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
# For p < .001 we report the exact p in scientific notation rather than the
# usual "< .001" placeholder.
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
  tbl$significant_at_cluster_alpha <- tbl$p_unc < ALPHA_CLUSTER
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
# effects, per the pre-registered convention:
#   - 3-way sig.            : 2 cells (angry rep 1 = H1, angry rep 5 = H2)
#   - 2-way w/ group sig.   : 2 simple effects per interaction
#                             (between-group at each level of the within
#                             factor; exploratory, not part of H1/H2)
#   - group main effect only: 1 overall comparison on subject-mean amplitudes
#                             (exploratory, not part of H1/H2)
#   - nothing involving grp : NULL
# Bonferroni-correct the simple-effect family within the cluster (k tests).
decompose_interactions <- function(d, anova_tbl) {
  sig <- anova_tbl$Source[anova_tbl$significant_at_cluster_alpha]
  three_way <- "emotion x repetition x group" %in% sig
  rep_x_grp <- "repetition x group" %in% sig
  emo_x_grp <- "emotion x group" %in% sig
  grp_main  <- "group" %in% sig

  rows <- list()
  decomposition <- character()

  if (three_way) {
    decomposition <- c(decomposition,
                       "3-way (emotion x repetition x group); pre-registered tests at angry rep 1 (H1) and angry rep 5 (H2)")
    for (rep in c("1", "5")) {
      cell <- d %>% filter(emotion == "angry", repetition == rep) %>%
        select(participant_id, group, amplitude)
      rows[[length(rows) + 1]] <- welch_row(
        cell, sprintf("Lonely vs Non-Lonely at angry, rep %s", rep))
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
  alpha <- ALPHA_POSTHOC_BASE / k
  tests$alpha_bonf <- alpha
  tests$p_bonferroni <- pmin(tests$p * k, 1.0)
  tests$significant <- !is.na(tests$p) & tests$p < alpha
  list(rationale = decomposition, tests = tests, alpha = alpha)
}

# Evaluate the pre-registered H1 and H2 confirmation criteria for a cluster.
# A hypothesis is *confirmed* when (a) the 3-way emotion x repetition x group
# interaction is significant at the cluster-corrected threshold, (b) the
# pre-registered between-group Welch t-test for the relevant cell (angry x rep 1
# for H1; angry x rep 5 for H2) is significant at the within-cluster Bonferroni
# threshold, and (c) the mean amplitude is higher (more positive, signed) in
# the lonely than in the non-lonely group. Returns a list with one element per
# hypothesis; each element records the three criteria and the overall verdict.
evaluate_hypotheses <- function(anova_tbl, posthoc) {
  three_way_row <- anova_tbl[anova_tbl$Source == "emotion x repetition x group", ]
  three_way_sig <- nrow(three_way_row) == 1 &&
    isTRUE(three_way_row$significant_at_cluster_alpha)

  hypos <- list(
    list(name = "H1", repetition = "1",
         description = "first presentation of angry faces"),
    list(name = "H2", repetition = "5",
         description = "fifth presentation of angry faces")
  )

  out <- list()
  for (h in hypos) {
    label <- sprintf("Lonely vs Non-Lonely at angry, rep %s", h$repetition)
    t_row <- NULL
    if (!is.null(posthoc$tests)) {
      cand <- posthoc$tests[posthoc$tests$comparison == label, ]
      if (nrow(cand) == 1) t_row <- cand
    }
    posthoc_sig <- !is.null(t_row) && isTRUE(t_row$significant)
    direction_ok <- !is.null(t_row) &&
      !is.na(t_row$mean_lonely) && !is.na(t_row$mean_nonlonely) &&
      t_row$mean_lonely > t_row$mean_nonlonely
    confirmed <- three_way_sig && posthoc_sig && direction_ok
    out[[length(out) + 1]] <- list(
      name = h$name, description = h$description, repetition = h$repetition,
      three_way_sig = three_way_sig,
      three_way_row = if (nrow(three_way_row) == 1) three_way_row else NULL,
      t_row = t_row,
      posthoc_alpha = posthoc$alpha,
      posthoc_sig = posthoc_sig,
      direction_ok = direction_ok,
      confirmed = confirmed
    )
  }
  out
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
               sprintf("| Source | df1 | df2 | F | p | partial eta^2 | sig. (alpha<%.2f) |",
                       ALPHA_CLUSTER),
               "|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(cl$anova))) {
      r <- cl$anova[i, ]
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                                r$Source, fmt_num(r$df1, 0), fmt_num(r$df2, 0),
                                fmt_num(r$F, 2), fmt_p(r$p_unc, 3),
                                fmt_eta2(r$pes),
                                if (isTRUE(r$significant_at_cluster_alpha)) "**yes**" else "no"))
    }
    if (!is.null(cl$posthoc$tests)) {
      lines <- c(lines, "",
                 "### Post-hoc simple effects (Welch t-tests, Lonely vs Non-Lonely)", "",
                 sprintf("Decomposing: %s. Bonferroni alpha (k=%d %s, baseline %.2f) = %.4f.",
                         paste(cl$posthoc$rationale, collapse = "; "),
                         nrow(cl$posthoc$tests),
                         if (nrow(cl$posthoc$tests) == 1) "test" else "tests",
                         ALPHA_POSTHOC_BASE,
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
                 sprintf("_No ANOVA effect involving group was significant at alpha < %.2f; no post-hoc tests reported._",
                         ALPHA_CLUSTER))
    }

    # Hypothesis evaluation (H1, H2): conjunction of the 3-way interaction,
    # the pre-registered cell t-test, and the directional prediction
    # (mean amplitude higher in lonely than non-lonely). The Bonferroni alpha
    # quoted here is the fixed H1/H2 family size (k=2), not the cluster's
    # actual posthoc family size — which can differ when a non-3-way effect
    # drove the decomposition.
    lines <- c(lines, "",
               "### Hypothesis evaluation", "",
               sprintf("Each hypothesis is confirmed only if (1) the 3-way emotion x repetition x group interaction is significant (alpha < %.2f), (2) the pre-registered Welch t-test for the relevant angry-face cell is significant (Bonferroni alpha = %.4f, baseline %.2f, k = 2 for the H1/H2 family), and (3) the mean amplitude is higher in the Lonely group than the Non-Lonely group.",
                       ALPHA_CLUSTER,
                       ALPHA_POSTHOC_THREEWAY, ALPHA_POSTHOC_BASE),
               "",
               "| Hypothesis | Description | 3-way (emotion x repetition x group) | Pre-registered t-test | Lonely > Non-Lonely | **Confirmed** |",
               "|---|---|---|---|---|---|")
    for (h in cl$hypotheses) {
      tw <- h$three_way_row
      three_way_str <- if (!is.null(tw)) {
        sprintf("%s (F(%s,%s) = %s, p = %s)",
                if (isTRUE(h$three_way_sig)) "**yes**" else "no",
                fmt_num(tw$df1, 0), fmt_num(tw$df2, 0),
                fmt_num(tw$F, 2), fmt_p(tw$p_unc, 3))
      } else {
        "n/a"
      }
      tr <- h$t_row
      posthoc_str <- if (!is.null(tr)) {
        sprintf("%s (t(%s) = %s, p = %s, p Bonf. = %s)",
                if (isTRUE(h$posthoc_sig)) "**yes**" else "no",
                fmt_num(tr$df, 1), fmt_num(tr$t, 2),
                fmt_p(tr$p, 3), fmt_p(tr$p_bonferroni, 3))
      } else {
        "_not run (3-way not significant)_"
      }
      direction_str <- if (!is.null(tr)) {
        sprintf("%s (M_L = %s, M_NL = %s)",
                if (isTRUE(h$direction_ok)) "**yes**" else "no",
                fmt_num(tr$mean_lonely, 2),
                fmt_num(tr$mean_nonlonely, 2))
      } else {
        "n/a"
      }
      confirmed_str <- if (isTRUE(h$confirmed)) "**yes**" else "**no**"
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s |",
                                h$name, h$description,
                                three_way_str, posthoc_str,
                                direction_str, confirmed_str))
    }

    lines <- c(lines, "")
  }

  lines <- c(lines, "## Methods", "",
             "- Filter: passed QC (>= 50 epochs per angry/happy x rep 1/5; <= 4 bad channels; `exclude != TRUE`; valid group).",
             "- Per-cluster mean ERP amplitudes were extracted in Python (`5_extract_amplitudes.py`) and supplied as a long-format TSV.",
             "- 3-way mixed ANOVA via `afex::aov_ez` (Type III SS; classical multi-stratum error terms; partial eta-squared as effect size).",
             "- Pre-registered post-hoc Welch t-tests: a significant 3-way interaction is decomposed into the two pre-registered Lonely vs Non-Lonely comparisons at angry x rep 1 (H1) and angry x rep 5 (H2). When the 3-way is not significant, exploratory follow-ups may still be reported for a significant 2-way interaction with group (one between-group test per level of the within factor) or a significant group main effect (one overall comparison on subject-mean amplitudes); these are not part of the H1/H2 confirmation criteria.",
             sprintf("- Significance thresholds: ANOVA effects are evaluated at alpha < %.2f (per term, no further correction across clusters). Within each cluster, the post-hoc simple-effect family uses a baseline criterion of p < %.2f with Bonferroni applied on top, i.e. alpha = %.2f / k where k is the number of simple-effect tests for that cluster.",
                     ALPHA_CLUSTER, ALPHA_POSTHOC_BASE, ALPHA_POSTHOC_BASE),
             sprintf("- Hypothesis verdicts: H1 (first presentation of angry faces) and H2 (fifth presentation of angry faces) are reported as *confirmed* in a cluster when (1) the 3-way emotion x repetition x group interaction is significant at alpha < %.2f, (2) the pre-registered Welch t-test for the relevant cell is significant at the within-cluster Bonferroni threshold (alpha = %.4f for the H1/H2 family of k = 2 tests), and (3) the mean amplitude is higher (more positive, signed) in the Lonely group than in the Non-Lonely group.",
                     ALPHA_CLUSTER, ALPHA_POSTHOC_THREEWAY),
             "")
  writeLines(lines, out_path)
}

render_table_tex <- function(per_cluster, out_path) {
  # APA-style ANOVA table: italicised statistic letters in the header, a single
  # combined `df` column (df_num, df_den), no separate Sig. column (asterisks
  # mark significant rows), and a Note. below the table explaining the markers
  # and conventions.
  note_text <- sprintf(paste0(
    "\\textit{Note.} ANOVA fitted with \\texttt{afex::aov\\_ez} ",
    "(Type III sums of squares; classical multi-stratum error terms; ",
    "partial $\\eta^{2}$ as effect size). Asterisks mark effects ",
    "significant at $\\alpha<%.2f$. \\textit{p}-values below $10^{-3}$ ",
    "are reported in scientific notation."),
    ALPHA_CLUSTER)

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
    sprintf("\\caption{3-way mixed ANOVA (emotion $\\times$ repetition $\\times$ group) per spatiotemporal cluster. Effects are evaluated at $\\alpha<%.2f$ per term.} \\\\",
            ALPHA_CLUSTER),
    "\\label{tab:main_analysis} \\\\",
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

  for (cl in per_cluster) {
    lines <- c(lines, sprintf("\\multicolumn{5}{@{}l}{\\textbf{%s} (effective $n=%d$)} \\\\",
                              cl$name, cl$n))
    for (i in seq_len(nrow(cl$anova))) {
      r <- cl$anova[i, ]
      F_str <- fmt_num(r$F, 2)
      if (isTRUE(r$significant_at_cluster_alpha)) F_str <- paste0(F_str, "*")
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

render_prose_tex <- function(per_cluster, sample_info, out_path) {
  lines <- c(sprintf(
    paste("Mean event-related potential (ERP) amplitudes were averaged",
          "over the electrodes and time window of each pre-registered",
          "spatiotemporal cluster, yielding one amplitude per participant",
          "per emotion (angry, happy) $\\times$ repetition (1st, 5th) cell.",
          "These amplitudes were submitted to a $2 \\times 2 \\times 2$",
          "mixed analysis of variance (ANOVA) with emotion and repetition",
          "as within-subject factors and group (lonely, non-lonely) as the",
          "between-subject factor, fitted with the \\texttt{aov\\_ez}",
          "function of the \\texttt{afex} package in \\texttt{R} using",
          "Type III sums of squares and classical multi-stratum error",
          "terms. Partial $\\eta^{2}$ is reported as the effect size for",
          "each ANOVA term. Only participants with non-missing amplitudes",
          "in all four within-subject cells were included in a cluster's",
          "analysis. Significance of ANOVA effects was evaluated at",
          "$\\alpha<%.2f$ per term. When the",
          "three-way emotion $\\times$ repetition $\\times$ group",
          "interaction was significant, it was decomposed with the two",
          "pre-registered between-group Welch $t$-tests on the angry-face",
          "cells at repetition 1 (corresponding to hypothesis H1) and",
          "repetition 5 (hypothesis H2). For clusters in which the",
          "three-way interaction was not significant but another effect",
          "involving group was, exploratory follow-ups were reported (a",
          "two-way interaction with group decomposed at each level of the",
          "within factor; a group main effect reported as one overall",
          "comparison on subject-mean amplitudes); these exploratory",
          "follow-ups are not part of the H1 or H2 confirmation criteria.",
          "Where no effect involving group was significant, no post-hoc",
          "tests were performed. Welch $t$-tests used the Satterthwaite",
          "degrees of freedom, and Cohen's $d$ was computed from the",
          "pooled within-group standard deviation. Within each cluster,",
          "the simple-effect family was evaluated at a baseline criterion",
          "of $p<%.2f$ with Bonferroni applied on top, yielding",
          "$\\alpha=%.2f/k$, where $k$ is the number of simple-effect",
          "tests for that cluster. A hypothesis (H1 or H2) was considered",
          "confirmed in a cluster when (i) the three-way interaction was",
          "significant at the cluster threshold, (ii) the corresponding",
          "pre-registered Welch $t$-test was significant at the",
          "within-cluster Bonferroni threshold, and (iii) the mean",
          "amplitude was higher (more positive, signed) in the lonely than",
          "in the non-lonely group. \\textit{p}-values below $10^{-3}$ are",
          "reported in scientific notation. The analytic sample comprised",
          "%d participants (lonely $n=%d$; non-lonely $n=%d$).",
          sep = " "),
    ALPHA_CLUSTER,
    ALPHA_POSTHOC_BASE, ALPHA_POSTHOC_BASE,
    sample_info$n_total,
    sample_info$n_lonely, sample_info$n_nonlonely))

  describe_anova <- function(cl) {
    sig_terms <- cl$anova[cl$anova$significant_at_cluster_alpha, ]
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
        sprintf("%.2f", ALPHA_CLUSTER), "$"
      )
    }

    grp_row <- cl$anova[cl$anova$Source == "group", ]
    grp_clause <- if (nrow(grp_row) == 1 && !isTRUE(grp_row$significant_at_cluster_alpha)) {
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

  describe_hypotheses <- function(cl) {
    parts <- character()
    for (h in cl$hypotheses) {
      tr <- h$t_row
      if (isTRUE(h$confirmed)) {
        part <- sprintf(
          "%s (%s) was confirmed: the three-way interaction was significant (see above), the pre-registered Welch $t$-test was significant (%s), and the mean amplitude was higher in the lonely ($M=%s$) than the non-lonely ($M=%s$) group",
          h$name, h$description,
          apa_t(tr$t, tr$df, tr$p, tr$d),
          fmt_num(tr$mean_lonely, 2), fmt_num(tr$mean_nonlonely, 2)
        )
      } else if (!isTRUE(h$three_way_sig)) {
        part <- sprintf(
          "%s (%s) was not confirmed because the three-way emotion $\\times$ repetition $\\times$ group interaction did not reach significance (see above)",
          h$name, h$description
        )
      } else if (is.null(tr)) {
        part <- sprintf(
          "%s (%s) was not confirmed because the pre-registered Welch $t$-test was not available",
          h$name, h$description
        )
      } else {
        reasons <- character()
        if (!isTRUE(h$posthoc_sig)) {
          reasons <- c(reasons,
                       sprintf("the Welch $t$-test did not reach the within-cluster Bonferroni threshold (%s, $p_{\\text{Bonf.}}=%s$)",
                               apa_t(tr$t, tr$df, tr$p, tr$d),
                               fmt_p(tr$p_bonferroni, 3)))
        }
        if (!isTRUE(h$direction_ok)) {
          reasons <- c(reasons,
                       sprintf("the directional prediction was not met ($M_{\\text{lonely}}=%s$ vs.\\ $M_{\\text{non-lonely}}=%s$)",
                               fmt_num(tr$mean_lonely, 2),
                               fmt_num(tr$mean_nonlonely, 2)))
        }
        part <- sprintf("%s (%s) was not confirmed: %s",
                        h$name, h$description,
                        paste(reasons, collapse = "; and "))
      }
      parts <- c(parts, part)
    }
    if (length(parts) == 0) return("")
    paste0(" ", paste(parts, collapse = ". "), ".")
  }

  for (cl in per_cluster) {
    lines <- c(lines, "",
               paste0(describe_anova(cl), describe_hypotheses(cl)))
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
    hypotheses <- evaluate_hypotheses(anova_tbl, ph)
    desc_tbl <- d %>%
      group_by(group, emotion, repetition) %>%
      summarise(n = n(), mean = mean(amplitude),
                se = sd(amplitude) / sqrt(n()), .groups = "drop") %>%
      arrange(group, emotion, repetition)
    per_cluster[[length(per_cluster) + 1]] <- list(
      name = cn, n = length(keep_ids),
      anova = anova_tbl, posthoc = ph, hypotheses = hypotheses,
      desc = desc_tbl
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
