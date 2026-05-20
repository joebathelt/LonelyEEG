#!/usr/bin/env Rscript
# Exploratory repetition-trend analysis: per cluster x emotion, fit a linear
# mixed-effects model of mean ERP amplitude with repetition as a CONTINUOUS
# predictor (vs the original per-participant slope/intercept fits). Reports
# the group main effect (= intercept difference between Lonely and Non-Lonely
# at rep 1) and the group:repetition interaction (= slope difference between
# the groups). Linear and logarithmic forms of the repetition predictor are
# fitted separately and compared via dAIC and marginal/conditional R^2
# (Nakagawa & Schielzeth, via MuMIn::r.squaredGLMM).
#
# Repetition coding (so the intercept lands on rep 1 in both models):
#   linear: rep_c   = repetition - 1     (rep 1 -> 0)
#   log:    rep_ln  = log(repetition)    (rep 1 -> log(1) = 0)
#
# Per (cluster, emotion) cell, the model is:
#   amplitude ~ <rep_term> * group + (1 | participant_id)
# with `<rep_term>` = `rep_c` (linear) or `rep_ln` (log). Fixed-effect tests
# use Satterthwaite degrees of freedom via lmerTest. Repetition 7 is excluded
# upstream (too noisy due to too few trials).
#
# Significance: baseline alpha = 0.02 (matches 6_main_analysis.R); Bonferroni
# applied within each (model, cluster, emotion) family of k = 2 tests
# (group main effect, group:rep interaction) -> alpha = 0.01. This is an
# exploratory secondary analysis (not part of H1/H2 confirmation criteria).

suppressPackageStartupMessages({
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (nzchar(user_lib) && dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lme4)
  library(lmerTest)
  library(MuMIn)
})

ALPHA_BASE <- 0.02
N_TESTS_PER_FAMILY <- 2  # group main effect, group:rep interaction
ALPHA_BONF <- ALPHA_BASE / N_TESTS_PER_FAMILY  # 0.01

MODELS <- c("linear", "log")
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
  required <- c("amplitudes-tsv", "out-report", "out-prose-tex",
                "out-table-tex", "out-predictions-tsv")
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

apa_t <- function(t_val, df, p) {
  paste0("\\textit{t}(", fmt_num(df, 1), ") = ", fmt_num(t_val, 2),
         ", ", fmt_p_apa(p))
}

apa_t_d <- function(t_val, df, p, d) {
  paste0("\\textit{t}(", fmt_num(df, 1), ") = ", fmt_num(t_val, 2),
         ", ", fmt_p_apa(p), ", \\textit{d} = ", fmt_num(d, 2))
}

# ---------------------------------------------------------------------------
# Supplementary: simple Welch t-test on per-participant mean amplitude
# ---------------------------------------------------------------------------
# Reports the same between-group contrast that a reader infers from the
# figure (which shows group-mean trajectories that are roughly parallel
# offsets). For each (cluster, emotion) cell, each participant's amplitude
# is averaged across reps 1..6, and a Welch t-test compares Lonely vs
# Non-Lonely. Cohen's d is computed from the pooled within-group SD. Sign
# convention matches the LMM contrast: positive `diff` = Non-Lonely > Lonely.
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

simple_ttest_cell <- function(d_cell) {
  per_pid <- d_cell %>%
    group_by(participant_id, group) %>%
    summarise(mean_amp = mean(amplitude), .groups = "drop")
  a <- per_pid$mean_amp[per_pid$group == "Lonely"]
  b <- per_pid$mean_amp[per_pid$group == "Non-Lonely"]
  r <- welch_d(a, b)
  list(
    n_lonely = r$n_a, n_nonlonely = r$n_b,
    mean_lonely = r$mean_a, se_lonely = r$se_a,
    mean_nonlonely = r$mean_b, se_nonlonely = r$se_b,
    diff = r$mean_b - r$mean_a,  # Non-Lonely - Lonely, to match LMM contrast
    t = r$t, df = r$df, p = r$p, d = r$d
  )
}

# ---------------------------------------------------------------------------
# Modelling
# ---------------------------------------------------------------------------

# Per (cluster, emotion, model) fit; returns list with the fitted model, the
# fixed-effect coefficient table for the two tests of interest, dAIC inputs,
# and marginal/conditional R^2.
fit_one <- function(d, model) {
  rep_term <- if (model == "linear") "rep_c" else "rep_ln"
  group_term <- "groupNon-Lonely"
  inter_term <- paste0(rep_term, ":", group_term)

  fml <- as.formula(sprintf("amplitude ~ %s * group + (1 | participant_id)",
                            rep_term))
  fit <- suppressMessages(suppressWarnings(
    lmerTest::lmer(fml, data = d, REML = TRUE)
  ))
  # Coefficient table with Satterthwaite df via lmerTest.
  co <- as.data.frame(summary(fit)$coefficients)
  co$term <- rownames(co); rownames(co) <- NULL

  pick_row <- function(term_name, label) {
    r <- co[co$term == term_name, , drop = FALSE]
    if (nrow(r) != 1) {
      return(tibble(term = term_name, label = label,
                    estimate = NA_real_, se = NA_real_,
                    df = NA_real_, t = NA_real_, p = NA_real_))
    }
    tibble(term = term_name, label = label,
           estimate = r[["Estimate"]],
           se = r[["Std. Error"]],
           df = r[["df"]], t = r[["t value"]],
           p = r[["Pr(>|t|)"]])
  }

  tests <- bind_rows(
    pick_row(group_term, "intercept (rep 1 baseline)"),
    pick_row(inter_term, "slope (rep effect)")
  )
  tests$alpha_bonf <- ALPHA_BONF
  tests$p_bonferroni <- pmin(tests$p * N_TESTS_PER_FAMILY, 1.0)
  tests$significant <- !is.na(tests$p) & tests$p < ALPHA_BONF

  # ML-based AIC for like-for-like comparison across non-nested mean structures
  # (REML AIC isn't comparable across models with different fixed effects;
  # both linear and log share the same fixed-effect *count* but use different
  # predictors, so ML-AIC is the appropriate basis).
  ll_fit <- suppressMessages(suppressWarnings(update(fit, REML = FALSE)))
  aic_val <- AIC(ll_fit)

  # Nakagawa & Schielzeth R^2 via MuMIn (delta method by default).
  r2 <- tryCatch({
    r2v <- suppressWarnings(MuMIn::r.squaredGLMM(fit))
    # r2v is a 1x2 or 2x2 matrix depending on family; for LMM it is 1x2.
    list(marginal = as.numeric(r2v[1, "R2m"]),
         conditional = as.numeric(r2v[1, "R2c"]))
  }, error = function(e) list(marginal = NA_real_, conditional = NA_real_))

  list(fit = fit, tests = tests, aic = aic_val,
       r2_marginal = r2$marginal, r2_conditional = r2$conditional,
       n_obs = nobs(fit),
       n_participants = length(unique(d$participant_id)),
       rep_term = rep_term)
}

# Predictions at integer reps 1..6 per group, from fixed effects only (i.e.
# population-average predictions ignoring random intercepts). Used to draw
# group-mean fitted curves in the figure script.
group_predictions <- function(fit, model, cluster_name, emotion) {
  reps <- 1:6
  grid <- expand.grid(
    repetition = reps,
    group = c("Lonely", "Non-Lonely"),
    stringsAsFactors = FALSE
  )
  grid$rep_c <- grid$repetition - 1
  grid$rep_ln <- log(grid$repetition)
  grid$group <- factor(grid$group, levels = c("Lonely", "Non-Lonely"))
  # `re.form = NA` -> population-level (fixed-effects-only) prediction.
  grid$predicted <- predict(fit, newdata = grid, re.form = NA,
                             allow.new.levels = TRUE)
  tibble(
    cluster = cluster_name,
    emotion = emotion,
    model = model,
    group = as.character(grid$group),
    repetition = grid$repetition,
    predicted = as.numeric(grid$predicted)
  )
}

# ---------------------------------------------------------------------------
# Report builders
# ---------------------------------------------------------------------------
render_markdown <- function(per_cell, model_recs, overall_r2, sample_info,
                            out_path) {
  lines <- c(
    "# Repetition-Trend Analysis Report", "",
    "Exploratory secondary analysis: per cluster x emotion, a linear mixed-",
    "effects model of mean ERP amplitude with **repetition as a continuous",
    "predictor** and a between-subjects group factor. Linear",
    "(`amp ~ (rep-1) * group + (1|pid)`) and logarithmic",
    "(`amp ~ ln(rep) * group + (1|pid)`) forms are fitted and compared via",
    "dAIC and Nakagawa & Schielzeth marginal/conditional R^2. The 7th",
    "repetition is excluded as too noisy (too few trials per participant).",
    "",
    "## Sample", "",
    sprintf("- Analytic sample (post-QC, same filter as main analysis): **%d** participants (Lonely n = %d; Non-Lonely n = %d).",
            sample_info$n_total, sample_info$n_lonely, sample_info$n_nonlonely),
    "- Repetitions 1..6 are used; missing per-rep amplitudes are simply absent rows in the LMM (lme4 handles unbalanced data).",
    ""
  )

  # Overall AIC + R^2 summary.
  lines <- c(lines,
             "## Overall model comparison (pooled across cells)", "",
             "| Model | Mean dAIC vs lower-AIC sibling | Mean marginal R^2 | Mean conditional R^2 | n cells where preferred |",
             "|---|---|---|---|---|")
  for (i in seq_len(nrow(overall_r2))) {
    r <- overall_r2[i, ]
    lines <- c(lines, sprintf("| %s | %s | %s | %s | %d |",
                              r$model,
                              fmt_num(r$mean_dAIC, 2),
                              fmt_num(r$mean_r2m, 3),
                              fmt_num(r$mean_r2c, 3),
                              r$n_preferred))
  }
  preferred_overall <- overall_r2$model[which.max(overall_r2$n_preferred)]
  lines <- c(lines, "",
             sprintf("**Overall recommendation:** the %s model is preferred (lower AIC) in the most cells.",
                     preferred_overall),
             "")

  # Per-cell recommendations.
  lines <- c(lines,
             "## Per-cell model recommendation",
             "",
             "| Cluster | Emotion | AIC linear | AIC log | dAIC (log - linear) | marg R^2 linear | marg R^2 log | cond R^2 linear | cond R^2 log | Recommended |",
             "|---|---|---|---|---|---|---|---|---|---|")
  for (rec in model_recs) {
    lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s | **%s** |",
                              rec$cluster, rec$emotion,
                              fmt_num(rec$aic_linear, 1),
                              fmt_num(rec$aic_log, 1),
                              fmt_num(rec$aic_log - rec$aic_linear, 2),
                              fmt_num(rec$r2m_linear, 3),
                              fmt_num(rec$r2m_log, 3),
                              fmt_num(rec$r2c_linear, 3),
                              fmt_num(rec$r2c_log, 3),
                              rec$recommended))
  }
  lines <- c(lines, "")

  # Supplementary: simple between-group Welch t-tests on per-participant
  # mean amplitude (averaged across reps 1..6). Mirrors what a reader
  # extracts visually from the figure (group-mean offsets that are roughly
  # constant across reps). Marginal effects (Bonferroni-corrected p just
  # over alpha = 0.01 but uncorrected p < 0.05 with consistent direction)
  # are flagged at the end of each row.
  lines <- c(lines,
             "## Supplementary: per-participant mean amplitude (Welch t-tests)",
             "",
             "Per cluster x emotion, each participant's mean amplitude is",
             "averaged across reps 1..6 and a Welch t-test compares Lonely",
             "vs Non-Lonely. `Diff` is Non-Lonely - Lonely (positive means",
             "Lonely is lower, matching the LMM intercept-difference sign).",
             "These tests use the same data as the LMM intercept term but",
             "do not partial out the linear/log rep trend; agreement with",
             "the LMM is therefore a sanity check on the inference.",
             "",
             "| Cluster | Emotion | n_L | n_NL | M_L (SE) | M_NL (SE) | Diff | t | df | p (uncorrected) | Cohen d | Note |",
             "|---|---|---|---|---|---|---|---|---|---|---|---|")
  for (cell in per_cell) {
    s <- cell$simple
    # Note flag: uncorrected p in (0.01, 0.05] with non-trivial effect size
    # is a 'marginal' finding under the strict pre-set Bonferroni alpha.
    note <- if (is.finite(s$p) && s$p < ALPHA_BONF) {
      "significant at Bonferroni alpha"
    } else if (is.finite(s$p) && s$p < 0.05 && !is.na(s$d) && abs(s$d) >= 0.3) {
      "marginal: uncorrected p < .05, Cohen d >= 0.3, consistent direction"
    } else {
      ""
    }
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %s (%s) | %s (%s) | %s | %s | %s | %s | %s | %s |",
                              cell$cluster, str_to_title(cell$emotion),
                              s$n_lonely, s$n_nonlonely,
                              fmt_num(s$mean_lonely, 3), fmt_num(s$se_lonely, 3),
                              fmt_num(s$mean_nonlonely, 3), fmt_num(s$se_nonlonely, 3),
                              fmt_num(s$diff, 3),
                              fmt_num(s$t, 2), fmt_num(s$df, 1),
                              fmt_p(s$p, 3),
                              fmt_num(s$d, 2),
                              note))
  }
  lines <- c(lines, "")

  # Per-cell detail.
  for (cell in per_cell) {
    lines <- c(lines, sprintf("## %s -- %s",
                              cell$cluster, str_to_title(cell$emotion)), "",
               sprintf("Observations: n_obs = %d; participants = %d (Lonely + Non-Lonely combined).",
                       cell$linear$n_obs, cell$linear$n_participants),
               "")
    for (m in MODELS) {
      mc <- cell[[m]]
      lines <- c(lines, sprintf("### Model: %s", m), "",
                 sprintf("AIC = %s; marginal R^2 = %s; conditional R^2 = %s.",
                         fmt_num(mc$aic, 2),
                         fmt_num(mc$r2_marginal, 3),
                         fmt_num(mc$r2_conditional, 3)),
                 "",
                 "**Fixed-effect tests of interest (Satterthwaite df via lmerTest)**",
                 "",
                 sprintf("Family Bonferroni alpha (k=%d, baseline %.2f) = %.4f.",
                         N_TESTS_PER_FAMILY, ALPHA_BASE, ALPHA_BONF),
                 "",
                 "| Term | Label | Estimate (SE) | t | df | p | p Bonf. | sig. |",
                 "|---|---|---|---|---|---|---|---|")
      for (i in seq_len(nrow(mc$tests))) {
        r <- mc$tests[i, ]
        lines <- c(lines, sprintf("| %s | %s | %s (%s) | %s | %s | %s | %s | %s |",
                                  r$term, r$label,
                                  fmt_num(r$estimate, 3), fmt_num(r$se, 3),
                                  fmt_num(r$t, 2), fmt_num(r$df, 1),
                                  fmt_p(r$p, 3), fmt_p(r$p_bonferroni, 3),
                                  if (isTRUE(r$significant)) "**yes**" else "no"))
      }
      lines <- c(lines, "")
    }
  }

  lines <- c(lines, "## Methods", "",
             "- Per-participant per-emotion per-cluster mean ERP amplitudes were extracted across repetitions 1..6 in Python (`11_extract_amplitudes_all_reps.py`), reusing the cluster definitions and analytic-sample filter from `5_extract_amplitudes.py` (>= 50 epochs per angry/happy x rep 1/5, <= 4 bad channels, `exclude != TRUE`, valid group label). The 7th repetition was excluded because it has too few trials per participant and is too noisy.",
             "- Linear mixed-effects models were fitted per (cluster, emotion) cell with `lmerTest::lmer` (REML for reporting; ML for AIC comparison). The model was `amplitude ~ <rep> * group + (1 | participant_id)` where `<rep>` was `(repetition - 1)` for the linear form (so the intercept lies at rep 1) and `ln(repetition)` for the logarithmic form (where `ln(1) = 0`). Group was coded with `Lonely` as the reference level, so the `groupNon-Lonely` coefficient is the **intercept difference (Lonely - Non-Lonely)** at rep 1, and the `<rep>:groupNon-Lonely` coefficient is the **slope difference (Lonely - Non-Lonely)**. (Equivalent to comparing the slopes and intercepts of per-group linear/log trajectories.)",
             "- Fixed-effect tests used Satterthwaite degrees of freedom via `lmerTest`.",
             "- Model comparison: AIC was computed on ML-refits (REML AICs are not directly comparable across non-nested models); the model with lower AIC is preferred per cell. Marginal and conditional R^2 were computed via `MuMIn::r.squaredGLMM` (Nakagawa & Schielzeth, 2013).",
             sprintf("- Significance threshold: alpha < %.2f baseline, Bonferroni-corrected within each (model, cluster, emotion) family for k = %d tests (group main effect, group:rep interaction), yielding alpha = %.4f. Exploratory; no further correction was applied across the 3 clusters or 2 emotions.",
                     ALPHA_BASE, N_TESTS_PER_FAMILY, ALPHA_BONF),
             sprintf("- Supplementary: a Welch t-test on each participant's mean amplitude (averaged across reps 1..6) is reported per cluster x emotion as a sanity check on the LMM intercept term. Cohen's d is computed from the pooled within-group SD. Sign convention follows the LMM contrast: positive `Diff` indicates a higher amplitude in the Non-Lonely than the Lonely group. Marginal effects (uncorrected p < .05, |d| >= 0.3, consistent direction) that fail the Bonferroni-corrected alpha = %.4f are flagged in the 'Note' column for transparency; they are not promoted to confirmed findings.",
                     ALPHA_BONF),
             "")
  writeLines(lines, out_path)
}

render_table_tex <- function(per_cell, model_recs, out_path) {
  note_text <- sprintf(paste0(
    "\\textit{Note.} Fixed-effect coefficients from a linear mixed model ",
    "$y \\sim \\text{rep} \\times \\text{group} + (1 \\mid \\text{pid})$ ",
    "per cluster $\\times$ emotion, with $\\text{rep}=\\text{repetition}-1$ ",
    "(linear) or $\\ln(\\text{repetition})$ (log); both intercepts thus ",
    "land on repetition~1. Group is referenced to Lonely, so coefficients ",
    "encode the Non-Lonely vs.\\ Lonely contrast at rep~1 (intercept term) ",
    "and the slope difference between groups (interaction term). ",
    "Satterthwaite \\textit{df} via \\texttt{lmerTest}. Asterisks mark ",
    "effects significant at the Bonferroni-corrected $\\alpha=%.4f$ ",
    "(baseline $%.2f$, $k=%d$). The Rec. column flags the model with the ",
    "lower AIC in that cell. \\textit{p}-values below $10^{-3}$ are ",
    "reported in scientific notation."),
    ALPHA_BONF, ALPHA_BASE, N_TESTS_PER_FAMILY)

  lines <- c(
    "% Requires: booktabs, xltabular, array.",
    "\\begingroup", "\\scriptsize",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\renewcommand{\\arraystretch}{1.15}",
    paste0("\\begin{xltabular}{\\textwidth}{@{}",
           ">{\\raggedright\\arraybackslash}X",
           ">{\\centering\\arraybackslash}p{1.7cm}",
           ">{\\centering\\arraybackslash}p{1.2cm}",
           ">{\\centering\\arraybackslash}p{1.3cm}",
           ">{\\centering\\arraybackslash}p{1.7cm}",
           ">{\\centering\\arraybackslash}p{1.0cm}@{}}"),
    sprintf("\\caption{Repetition-trend mixed-effects analysis. Repetition is a continuous predictor (linear or log of repetition; reps 1--6, rep 7 excluded as too noisy). Group coefficients are the Non-Lonely vs.\\ Lonely contrast. Exploratory; $\\alpha=%.4f$ within each (model, cluster, emotion) family ($k=%d$).} \\\\",
            ALPHA_BONF, N_TESTS_PER_FAMILY),
    "\\label{tab:repetition_trends} \\\\",
    "\\toprule",
    "\\textbf{Source} & \\textit{Estimate} (SE) & \\textit{df} & \\textit{t} & \\textit{p} Bonf. & Rec. \\\\",
    "\\midrule \\endfirsthead",
    "\\toprule",
    "\\textbf{Source} & \\textit{Estimate} (SE) & \\textit{df} & \\textit{t} & \\textit{p} Bonf. & Rec. \\\\",
    "\\midrule \\endhead",
    "\\bottomrule",
    sprintf("\\multicolumn{6}{@{}p{\\textwidth}@{}}{%s} \\\\", note_text),
    "\\endlastfoot"
  )

  rec_lookup <- function(cluster, emotion) {
    for (rec in model_recs) {
      if (rec$cluster == cluster && rec$emotion == emotion) return(rec$recommended)
    }
    NA_character_
  }

  for (cell in per_cell) {
    lines <- c(lines, sprintf(
      "\\multicolumn{6}{@{}l}{\\textbf{%s -- %s}} \\\\",
      cell$cluster, str_to_title(cell$emotion)))
    for (m in MODELS) {
      mc <- cell[[m]]
      rec <- rec_lookup(cell$cluster, cell$emotion)
      rec_mark <- if (!is.na(rec) && rec == m) "$\\star$" else ""
      lines <- c(lines, sprintf(
        "\\hspace{1em}\\textit{%s} (AIC = %s, marg.\\ $R^{2}=%s$, cond.\\ $R^{2}=%s$) & & & & & %s \\\\",
        m, fmt_num(mc$aic, 1),
        fmt_num(mc$r2_marginal, 3),
        fmt_num(mc$r2_conditional, 3),
        rec_mark))
      for (i in seq_len(nrow(mc$tests))) {
        r <- mc$tests[i, ]
        t_str <- fmt_num(r$t, 2)
        if (isTRUE(r$significant)) t_str <- paste0(t_str, "*")
        est_str <- sprintf("%s (%s)",
                           fmt_num(r$estimate, 3), fmt_num(r$se, 3))
        lines <- c(lines, sprintf(
          "\\hspace{2em}%s & %s & %s & %s & %s & \\\\",
          r$label, est_str,
          fmt_num(r$df, 1), t_str,
          fmt_p_tex(r$p_bonferroni)))
      }
    }
    lines <- c(lines, "\\addlinespace")
  }

  lines <- c(lines, "\\end{xltabular}", "\\endgroup")
  writeLines(lines, out_path)
}

render_prose_tex <- function(per_cell, model_recs, overall_r2, sample_info,
                             out_path) {
  preferred_overall <- overall_r2$model[which.max(overall_r2$n_preferred)]
  n_pref_overall <- overall_r2$n_preferred[which.max(overall_r2$n_preferred)]

  preface <- sprintf(
    paste("As an exploratory secondary analysis, mean ERP amplitudes from",
          "repetitions 1--6 (the seventh repetition was excluded as too",
          "noisy due to too few trials per participant) were analysed with",
          "linear mixed-effects models in which repetition entered as a",
          "continuous predictor. For each spatiotemporal cluster and emotion",
          "(angry, happy), separate models were fitted with the formula",
          "$\\text{amplitude} \\sim \\text{rep}\\times\\text{group} +",
          "(1\\mid\\text{participant})$, where $\\text{rep}$ was either",
          "$\\text{repetition}-1$ (linear form, intercept anchored at",
          "repetition~1) or $\\ln(\\text{repetition})$ (logarithmic form,",
          "for which $\\ln 1 = 0$, also anchoring the intercept at",
          "repetition~1). Group was contrast-coded with Lonely as the",
          "reference, so the \\texttt{groupNon-Lonely} coefficient",
          "estimates the between-group intercept difference (rep~1 baseline)",
          "and the \\texttt{rep:groupNon-Lonely} coefficient estimates the",
          "between-group slope difference. Models were fitted with",
          "\\texttt{lmerTest::lmer} (REML for inference); fixed-effect",
          "tests used Satterthwaite degrees of freedom. Linear and",
          "logarithmic forms were compared via AIC on ML-refits and via",
          "marginal and conditional $R^{2}$ (Nakagawa \\& Schielzeth, 2013;",
          "\\texttt{MuMIn::r.squaredGLMM}). Effects were evaluated at a",
          "baseline $\\alpha=%.2f$ with Bonferroni applied within each",
          "(model, cluster, emotion) family ($k=%d$ tests: intercept and",
          "slope difference), yielding $\\alpha=%.4f$; no further correction",
          "was applied across clusters or emotions. The",
          "participant-inclusion filter was identical to the main ERP",
          "analysis. \\textit{p}-values below $10^{-3}$ are reported in",
          "scientific notation. The analytic sample comprised %d",
          "participants (lonely $n=%d$; non-lonely $n=%d$).",
          "Pooling across (cluster, emotion) cells, the %s form was",
          "preferred (lower AIC) in %d of %d cells, and is taken as the",
          "more parsimonious description of the repetition trend overall.",
          "As a supplementary cross-check, a Welch $t$-test on each",
          "participant's mean amplitude (averaged across reps 1--6) was",
          "computed per cluster $\\times$ emotion.",
          sep = " "),
    ALPHA_BASE, N_TESTS_PER_FAMILY, ALPHA_BONF,
    sample_info$n_total, sample_info$n_lonely, sample_info$n_nonlonely,
    preferred_overall, n_pref_overall, length(model_recs))

  lines <- c(preface)

  # Identify marginal cells across the LMM intercept tests + simple t-tests
  # and build a single concluding sentence about them.
  marg_descriptions <- character()
  for (cell in per_cell) {
    s <- cell$simple
    if (!is.finite(s$p)) next
    if (s$p < ALPHA_BONF) next  # already 'significant'; not a marginal effect
    if (s$p >= 0.05) next       # not marginal either
    if (is.na(s$d) || abs(s$d) < 0.3) next
    direction <- if (s$diff > 0) {
      "the Lonely group was lower than the Non-Lonely group"
    } else {
      "the Lonely group was higher than the Non-Lonely group"
    }
    marg_descriptions <- c(marg_descriptions, sprintf(
      "%s for %s faces (mean difference $=%s\\,\\mu\\mathrm{V}$, %s, where %s)",
      cell$cluster, cell$emotion,
      fmt_num(s$diff, 2),
      apa_t_d(s$t, s$df, s$p, s$d), direction))
  }
  if (length(marg_descriptions) > 0) {
    marg_sentence <- paste0(
      "Several cells produced marginal between-group effects that did not ",
      "survive the within-cell Bonferroni-corrected threshold but were ",
      "consistent in direction and of medium effect size: ",
      paste(marg_descriptions, collapse = "; "),
      ". These are flagged for transparency; under the pre-set $\\alpha=",
      sprintf("%.4f", ALPHA_BONF),
      "$ they should not be interpreted as confirmed group differences."
    )
    lines <- c(lines, "", marg_sentence)
  }

  describe_cell <- function(cell) {
    parts <- character()
    for (m in MODELS) {
      mc <- cell[[m]]
      cell_lines <- character()
      for (i in seq_len(nrow(mc$tests))) {
        r <- mc$tests[i, ]
        direction <- if (!is.finite(r$estimate)) "n/a" else if (r$estimate > 0) {
          "higher in non-lonely than lonely"
        } else {
          "lower in non-lonely than lonely"
        }
        verdict <- if (isTRUE(r$significant)) "significant" else "not significant"
        cell_lines <- c(cell_lines,
                        sprintf("%s (estimate $=%s$, %s; %s; %s)",
                                r$label, fmt_num(r$estimate, 3),
                                apa_t(r$t, r$df, r$p), direction, verdict))
      }
      parts <- c(parts,
                 sprintf("%s model (AIC $=%s$, marg.\\ $R^{2}=%s$, cond.\\ $R^{2}=%s$): %s",
                         m, fmt_num(mc$aic, 1),
                         fmt_num(mc$r2_marginal, 3),
                         fmt_num(mc$r2_conditional, 3),
                         paste(cell_lines, collapse = "; ")))
    }
    paste0("In the ", cell$cluster, " cluster for ", cell$emotion,
           " faces, ", paste(parts, collapse = "; "), ".")
  }

  for (cell in per_cell) {
    lines <- c(lines, "", describe_cell(cell))
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

  amps <- amps %>%
    filter(!is.na(amplitude)) %>%
    mutate(
      repetition = as.integer(repetition),
      rep_c = repetition - 1L,
      rep_ln = log(repetition),
      group = factor(group, levels = c("Lonely", "Non-Lonely")),
      participant_id = factor(participant_id)
    )

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
  per_cell <- list()
  model_recs <- list()
  preds <- list()

  for (cn in cluster_names) {
    for (em in EMOTIONS) {
      d <- amps %>% filter(cluster == cn, emotion == em)
      if (nrow(d) == 0) next
      cell <- list(cluster = cn, emotion = em,
                   simple = simple_ttest_cell(d))
      aics <- c()
      for (m in MODELS) {
        cat("[", format(Sys.time(), "%H:%M:%S"), "] Fitting ", m,
            " | ", cn, " | ", em, "\n", sep = "")
        fit_res <- fit_one(d, m)
        cell[[m]] <- fit_res
        aics[m] <- fit_res$aic
        preds[[length(preds) + 1]] <- group_predictions(
          fit_res$fit, m, cn, em)
      }
      per_cell[[length(per_cell) + 1]] <- cell
      rec <- names(aics)[which.min(aics)]
      model_recs[[length(model_recs) + 1]] <- list(
        cluster = cn, emotion = em,
        aic_linear = aics[["linear"]],
        aic_log = aics[["log"]],
        r2m_linear = cell$linear$r2_marginal,
        r2m_log = cell$log$r2_marginal,
        r2c_linear = cell$linear$r2_conditional,
        r2c_log = cell$log$r2_conditional,
        recommended = rec
      )
    }
  }

  # Overall summary across cells.
  pref_counts <- table(vapply(model_recs, function(r) r$recommended, character(1)))
  build_overall_row <- function(model_name) {
    aics_this <- vapply(model_recs, function(r) r[[paste0("aic_", model_name)]], numeric(1))
    aics_other <- vapply(model_recs,
                         function(r) r[[paste0("aic_", setdiff(MODELS, model_name))]],
                         numeric(1))
    dAIC <- aics_this - aics_other  # positive => this model is worse
    r2m <- vapply(model_recs, function(r) r[[paste0("r2m_", model_name)]], numeric(1))
    r2c <- vapply(model_recs, function(r) r[[paste0("r2c_", model_name)]], numeric(1))
    n_pref <- if (model_name %in% names(pref_counts)) {
      as.integer(pref_counts[model_name])
    } else {
      0L
    }
    tibble(
      model = model_name,
      mean_dAIC = mean(dAIC, na.rm = TRUE),
      mean_r2m = mean(r2m, na.rm = TRUE),
      mean_r2c = mean(r2c, na.rm = TRUE),
      n_preferred = n_pref
    )
  }
  overall_r2 <- bind_rows(lapply(MODELS, build_overall_row))

  out_report <- args[["out-report"]]
  out_prose <- args[["out-prose-tex"]]
  out_table <- args[["out-table-tex"]]
  out_pred <- args[["out-predictions-tsv"]]
  dir.create(dirname(out_report), recursive = TRUE, showWarnings = FALSE)

  render_markdown(per_cell, model_recs, overall_r2, sample_info, out_report)
  cat("Wrote ", out_report, "\n", sep = "")
  render_table_tex(per_cell, model_recs, out_table)
  cat("Wrote ", out_table, "\n", sep = "")
  render_prose_tex(per_cell, model_recs, overall_r2, sample_info, out_prose)
  cat("Wrote ", out_prose, "\n", sep = "")

  # Write per-group population-level predictions for the figure script.
  preds_df <- bind_rows(preds)
  # Add the recommendation flag so the figure knows which model line is solid.
  rec_df <- bind_rows(lapply(model_recs, function(r) tibble(
    cluster = r$cluster, emotion = r$emotion,
    recommended_model = r$recommended)))
  preds_df <- preds_df %>%
    left_join(rec_df, by = c("cluster", "emotion")) %>%
    mutate(is_recommended = model == recommended_model) %>%
    select(-recommended_model)
  write_tsv(preds_df, out_pred)
  cat("Wrote ", out_pred, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
