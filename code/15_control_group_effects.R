#!/usr/bin/env Rscript
# Control analysis (positive control / manipulation check): does the COMPARISON
# (Non-Lonely) group show the canonical ERP effects that the paradigm is meant
# to elicit? The main analysis (6_main_analysis.R) tests whether the Lonely
# group shows *enhanced* threat/repetition effects relative to Non-Lonely; that
# contrast is only interpretable if the comparison group shows the basic effects
# in the first place. Here, within the Non-Lonely group only, we run three
# within-subject (paired) t-tests on the pre-extracted cluster amplitudes:
#
#   Cluster                          Contrast                  Expected (one-tailed)
#   -------------------------------  ------------------------  ---------------------
#   Hypersensitivity 1 (120-170 ms)  angry rep1 vs happy rep1  angry > happy
#   Hypersensitivity 2 (360-470 ms)  angry rep1 vs happy rep1  angry > happy
#   Hyperalertness     (480-600 ms)  angry rep1 vs angry rep5  rep1  > rep5
#
# Directions match the project's own difference-wave contrast definitions in
# 5b_extract_difference_waves.py (emotion: hi=angry, lo=happy; repetition:
# hi=rep1, lo=rep5). Tests are one-tailed in the pre-specified direction.
#
# Each test reports a paired t-test (t, df, one-tailed p), Cohen's dz
# (mean(diff)/sd(diff)), and a directional Bayes Factor via
# BayesFactor::ttestBF(x = diff, mu = 0, nullInterval = c(0, Inf)) -- matching
# the BF reporting in 6_main_analysis.R. BF10 = evidence for the (directional)
# alternative; BF01 = 1/BF10 = evidence for the null.
#
# Significance: baseline alpha = 0.02 (matches 6_main_analysis.R). Each cluster
# contributes a single test, so there is no within-cluster family to Bonferroni-
# correct; results are reported uncorrected per cluster at alpha = 0.02,
# consistent with how the main analysis treats the three clusters. This is a
# secondary control analysis, not part of the H1/H2 confirmation criteria.

suppressPackageStartupMessages({
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (nzchar(user_lib) && dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(BayesFactor)
})

ALPHA <- 0.02            # matches 6_main_analysis.R baseline
CONTROL_GROUP <- "Non-Lonely"

# Pre-specified contrasts. `key` is matched against the start of the cluster
# name (which carries a time-window suffix, e.g. "Hypersensitivity 1
# (120-170 ms)"). `hi`/`lo` are (emotion, repetition) cells; the one-tailed
# alternative is mean(hi) > mean(lo).
CONTRASTS <- list(
  list(key = "Hypersensitivity 1", contrast = "emotion",
       hi = list(emotion = "angry", repetition = 1L),
       lo = list(emotion = "happy", repetition = 1L),
       direction = "angry > happy",
       expectation = "a larger response to angry than happy faces at first presentation"),
  list(key = "Hypersensitivity 2", contrast = "emotion",
       hi = list(emotion = "angry", repetition = 1L),
       lo = list(emotion = "happy", repetition = 1L),
       direction = "angry > happy",
       expectation = "a larger response to angry than happy faces at first presentation"),
  list(key = "Hyperalertness", contrast = "repetition",
       hi = list(emotion = "angry", repetition = 1L),
       lo = list(emotion = "angry", repetition = 5L),
       direction = "rep 1 > rep 5",
       expectation = "repetition suppression (a larger response to the first than the fifth presentation) for angry faces")
)

# ---------------------------------------------------------------------------
# CLI parsing (same idiom as 13_repetition_trends_analysis.R)
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
# Formatting helpers (APA-style; copied from 6_main_analysis.R / 13)
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

# Bayes Factor formatters (copied from 6_main_analysis.R).
fmt_bf <- function(bf, digits = 2) {
  if (!is.finite(bf)) return("n/a")
  if (bf <= 0) return("n/a")
  if (bf < 0.01 || bf > 1000) {
    parts <- strsplit(formatC(bf, format = "e", digits = digits), "e")[[1]]
    return(sprintf("%s x 10^%d", parts[1], as.integer(parts[2])))
  }
  formatC(bf, format = "fg", digits = digits + 1, flag = "#")
}

fmt_bf_tex <- function(bf, digits = 2) {
  if (!is.finite(bf)) return("n/a")
  if (bf <= 0) return("n/a")
  if (bf < 0.01 || bf > 1000) {
    parts <- strsplit(formatC(bf, format = "e", digits = digits), "e")[[1]]
    return(sprintf("$%s \\times 10^{%d}$", parts[1], as.integer(parts[2])))
  }
  formatC(bf, format = "fg", digits = digits + 1, flag = "#")
}

apa_t_d <- function(t_val, df, p, d, bf10 = NULL, bf01 = NULL) {
  base <- paste0("\\textit{t}(", fmt_num(df, 1), ") = ", fmt_num(t_val, 2),
                 ", ", fmt_p_apa(p), ", \\textit{d}\\textsubscript{z} = ",
                 fmt_num(d, 2))
  if (!is.null(bf10) && !is.null(bf01)) {
    base <- paste0(base, ", $\\mathrm{BF}_{10}$ = ", fmt_bf_tex(bf10),
                   ", $\\mathrm{BF}_{01}$ = ", fmt_bf_tex(bf01))
  }
  base
}

# ---------------------------------------------------------------------------
# Statistics
# ---------------------------------------------------------------------------
# Directional Bayes Factor on paired-difference scores. Mirrors bf_ttest in
# 6_main_analysis.R but operates on the within-subject difference (one-sample
# vs mu = 0) and restricts the alternative to the positive interval to match
# the one-tailed frequentist test. extractBF()$bf[1] is the BF for the
# restricted alternative (0 < delta < Inf) vs the point null.
bf_ttest_dir <- function(diff) {
  diff <- diff[!is.na(diff)]
  if (length(diff) < 2) {
    return(list(bf10 = NA_real_, bf01 = NA_real_, error_pct = NA_real_))
  }
  bf <- tryCatch(
    suppressMessages(BayesFactor::ttestBF(x = diff, mu = 0,
                                          nullInterval = c(0, Inf),
                                          rscale = "medium")),
    error = function(e) NULL
  )
  if (is.null(bf)) {
    return(list(bf10 = NA_real_, bf01 = NA_real_, error_pct = NA_real_))
  }
  ext <- BayesFactor::extractBF(bf, logbf = FALSE)
  bf10 <- ext$bf[1]
  list(bf10 = bf10,
       bf01 = if (is.finite(bf10) && bf10 > 0) 1 / bf10 else NA_real_,
       error_pct = ext$error[1])
}

# Run one paired contrast within the comparison group. `d_cluster` is the
# comparison-group amplitude rows for a single cluster.
run_contrast <- function(cluster_name, d_cluster, spec) {
  cell <- function(side) {
    s <- spec[[side]]
    d_cluster %>%
      filter(emotion == s$emotion, repetition == s$repetition) %>%
      group_by(participant_id) %>%
      summarise(amp = mean(amplitude), .groups = "drop")
  }
  hi <- cell("hi"); lo <- cell("lo")
  paired <- inner_join(hi, lo, by = "participant_id",
                       suffix = c("_hi", "_lo"))
  paired <- paired %>% filter(!is.na(amp_hi), !is.na(amp_lo))
  n <- nrow(paired)

  base <- list(
    cluster = cluster_name, contrast = spec$contrast,
    direction = spec$direction, expectation = spec$expectation,
    hi_label = sprintf("%s rep %d", spec$hi$emotion, spec$hi$repetition),
    lo_label = sprintf("%s rep %d", spec$lo$emotion, spec$lo$repetition),
    n = n
  )
  if (n < 2) {
    return(c(base, list(
      mean_hi = NA_real_, sd_hi = NA_real_, se_hi = NA_real_,
      mean_lo = NA_real_, sd_lo = NA_real_, se_lo = NA_real_,
      mean_diff = NA_real_, sd_diff = NA_real_, se_diff = NA_real_,
      t = NA_real_, df = NA_real_, p = NA_real_, dz = NA_real_,
      bf10 = NA_real_, bf01 = NA_real_, significant = FALSE)))
  }

  diff <- paired$amp_hi - paired$amp_lo
  tt <- t.test(paired$amp_hi, paired$amp_lo, paired = TRUE,
               alternative = "greater")
  dz <- if (sd(diff) == 0) NA_real_ else mean(diff) / sd(diff)
  bf <- bf_ttest_dir(diff)

  c(base, list(
    mean_hi = mean(paired$amp_hi), sd_hi = sd(paired$amp_hi),
    se_hi = sd(paired$amp_hi) / sqrt(n),
    mean_lo = mean(paired$amp_lo), sd_lo = sd(paired$amp_lo),
    se_lo = sd(paired$amp_lo) / sqrt(n),
    mean_diff = mean(diff), sd_diff = sd(diff), se_diff = sd(diff) / sqrt(n),
    t = unname(tt$statistic), df = unname(tt$parameter),
    p = unname(tt$p.value), dz = dz,
    bf10 = bf$bf10, bf01 = bf$bf01,
    significant = is.finite(tt$p.value) && tt$p.value < ALPHA
  ))
}

# ---------------------------------------------------------------------------
# Report builders
# ---------------------------------------------------------------------------
render_markdown <- function(results, n_control, out_path) {
  lines <- c(
    "# Control Analysis: Canonical Effects in the Comparison Group", "",
    "Positive-control / manipulation check. The main analysis tests whether the",
    "**Lonely** group shows *enhanced* threat and repetition effects relative to",
    "the **Non-Lonely** comparison group; that contrast is only interpretable if",
    "the comparison group shows the basic effects in the first place. Within the",
    sprintf("**%s** group only, three within-subject (paired) t-tests on the", CONTROL_GROUP),
    "pre-extracted cluster amplitudes test the canonical effects:", "",
    "- **Hypersensitivity 1 / 2 (emotion):** angry vs happy at first presentation (expected angry > happy).",
    "- **Hyperalertness (repetition):** first vs fifth presentation for angry faces (expected rep 1 > rep 5, i.e. repetition suppression).",
    "",
    "Directions follow the project's own difference-wave definitions",
    "(`5b_extract_difference_waves.py`). Tests are **one-tailed** in the",
    "pre-specified direction. Bayes Factors use a directional alternative",
    "(`BayesFactor::ttestBF(x = diff, mu = 0, nullInterval = c(0, Inf))`).", "",
    "## Sample", "",
    sprintf("- Comparison group (%s), post-QC analytic sample: **%d** participants.",
            CONTROL_GROUP, n_control),
    sprintf("- Significance threshold: alpha = %.2f (baseline, matching the main analysis). One test per cluster, so no within-cluster Bonferroni correction is applied. This is a secondary control analysis, not part of the H1/H2 confirmation criteria.",
            ALPHA),
    "",
    "## Results", "",
    "| Cluster | Contrast (hi vs lo) | n | M(hi) (SE) | M(lo) (SE) | M(diff) (SE) | t | df | p (1-tailed) | d_z | BF10 | BF01 | Effect present |",
    "|---|---|---|---|---|---|---|---|---|---|---|---|---|"
  )
  for (r in results) {
    verdict <- if (isTRUE(r$significant)) "**yes**" else "no"
    lines <- c(lines, sprintf(
      "| %s | %s vs %s | %d | %s (%s) | %s (%s) | %s (%s) | %s | %s | %s | %s | %s | %s | %s |",
      r$cluster, r$hi_label, r$lo_label, r$n,
      fmt_num(r$mean_hi, 3), fmt_num(r$se_hi, 3),
      fmt_num(r$mean_lo, 3), fmt_num(r$se_lo, 3),
      fmt_num(r$mean_diff, 3), fmt_num(r$se_diff, 3),
      fmt_num(r$t, 2), fmt_num(r$df, 1), fmt_p(r$p, 3),
      fmt_num(r$dz, 2), fmt_bf(r$bf10), fmt_bf(r$bf01), verdict))
  }
  lines <- c(lines, "")

  # Per-cluster prose verdicts.
  for (r in results) {
    present <- if (isTRUE(r$significant)) {
      sprintf("The comparison group **showed** the expected %s effect (%s).",
              r$contrast, r$direction)
    } else {
      sprintf("The comparison group **did not show** a significant %s effect (%s) at alpha = %.2f.",
              r$contrast, r$direction, ALPHA)
    }
    lines <- c(lines, sprintf("### %s", r$cluster), "",
               sprintf("Expected: %s.", r$expectation),
               sprintf("Mean difference (%s - %s) = %s uV (SE = %s); t(%s) = %s, p = %s (one-tailed), d_z = %s; BF10 = %s, BF01 = %s (n = %d).",
                       r$hi_label, r$lo_label,
                       fmt_num(r$mean_diff, 3), fmt_num(r$se_diff, 3),
                       fmt_num(r$df, 1), fmt_num(r$t, 2), fmt_p(r$p, 3),
                       fmt_num(r$dz, 2), fmt_bf(r$bf10), fmt_bf(r$bf01), r$n),
               present, "")
  }

  lines <- c(lines, "## Methods", "",
             sprintf("- Within the comparison (%s) group only, per-participant mean ERP amplitudes were taken from `results/cluster_amplitudes.tsv` (extracted by `5_extract_amplitudes.py` for the three pre-registered spatiotemporal clusters; analytic-sample filter: >= 50 epochs per angry/happy x rep 1/5, <= 4 bad channels, `exclude != TRUE`, valid group label).",
                     CONTROL_GROUP),
             "- For the Hypersensitivity 1 and Hypersensitivity 2 clusters, a paired-sample t-test compared mean amplitude to angry vs happy faces at the first presentation (repetition 1). For the Hyperalertness cluster, a paired-sample t-test compared the first vs the fifth presentation (repetitions 1 vs 5) for angry faces.",
             "- Tests were one-tailed in the pre-specified direction (angry > happy; first > fifth presentation), matching the difference-wave contrast directions in `5b_extract_difference_waves.py`. The paired effect size is Cohen's d_z = mean(difference) / SD(difference).",
             "- Bayes Factors were computed with `BayesFactor::ttestBF(x = difference, mu = 0, nullInterval = c(0, Inf), rscale = \"medium\")`, a directional one-sample test on the within-subject difference scores with the default JZS (Cauchy, r = sqrt(2)/2) prior. BF10 quantifies evidence for the (directional) alternative; BF01 = 1/BF10 quantifies evidence for the null. By Jeffreys' conventions, BF > 3 is substantial, BF > 10 strong, and BF > 30 very strong evidence.",
             sprintf("- Significance threshold: alpha = %.2f (baseline, matching `6_main_analysis.R`). Each cluster contributes a single test, so no within-cluster Bonferroni correction was applied. This is a secondary control analysis and is not part of the H1/H2 confirmation criteria.",
                     ALPHA),
             "")
  writeLines(lines, out_path)
}

render_table_tex <- function(results, n_control, out_path) {
  note_text <- sprintf(paste0(
    "\\textit{Note.} Within the comparison (Non-Lonely) group ($n=%d$), ",
    "within-subject paired \\textit{t}-tests on mean cluster amplitudes. For ",
    "the Hypersensitivity clusters the contrast is angry vs.\\ happy at the ",
    "first presentation; for Hyperalertness it is the first vs.\\ fifth ",
    "presentation of angry faces. Tests are one-tailed in the pre-specified ",
    "direction (angry $>$ happy; first $>$ fifth presentation). ",
    "$M_{\\text{diff}}$ is the mean within-subject difference (hi $-$ lo) in ",
    "$\\mu$V; $d_{z}=M_{\\text{diff}}/\\mathit{SD}_{\\text{diff}}$. Bayes ",
    "Factors use a directional alternative (\\texttt{BayesFactor::ttestBF}, ",
    "$\\text{mu}=0$, $\\text{nullInterval}=(0,\\infty)$, $r=\\sqrt{2}/2$); ",
    "$\\mathrm{BF}_{10}$ favours the alternative, $\\mathrm{BF}_{01}=1/",
    "\\mathrm{BF}_{10}$ the null. Asterisks mark effects significant at ",
    "$\\alpha=%.2f$. This is a secondary control analysis. \\textit{p}-values ",
    "below $10^{-3}$ are reported in scientific notation."),
    n_control, ALPHA)

  lines <- c(
    "% Requires: booktabs, xltabular, array.",
    "\\begingroup", "\\scriptsize",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\renewcommand{\\arraystretch}{1.15}",
    paste0("\\begin{xltabular}{\\textwidth}{@{}",
           ">{\\raggedright\\arraybackslash}X",      # cluster
           ">{\\centering\\arraybackslash}p{0.7cm}",  # n
           ">{\\centering\\arraybackslash}p{1.6cm}",  # M diff (SE)
           ">{\\centering\\arraybackslash}p{1.1cm}",  # t
           ">{\\centering\\arraybackslash}p{0.9cm}",  # df
           ">{\\centering\\arraybackslash}p{1.3cm}",  # p
           ">{\\centering\\arraybackslash}p{0.9cm}",  # dz
           ">{\\centering\\arraybackslash}p{1.2cm}",  # BF10
           ">{\\centering\\arraybackslash}p{1.2cm}@{}}"),  # BF01
    sprintf("\\caption{Control analysis: canonical within-subject ERP effects in the comparison (Non-Lonely) group ($n=%d$). One-tailed paired \\textit{t}-tests; secondary control analysis, $\\alpha=%.2f$.} \\\\",
            n_control, ALPHA),
    "\\label{tab:control_group_effects} \\\\",
    "\\toprule",
    "\\textbf{Cluster (contrast)} & $n$ & $M_{\\text{diff}}$ (SE) & \\textit{t} & \\textit{df} & \\textit{p} & $d_{z}$ & $\\mathrm{BF}_{10}$ & $\\mathrm{BF}_{01}$ \\\\",
    "\\midrule \\endfirsthead",
    "\\toprule",
    "\\textbf{Cluster (contrast)} & $n$ & $M_{\\text{diff}}$ (SE) & \\textit{t} & \\textit{df} & \\textit{p} & $d_{z}$ & $\\mathrm{BF}_{10}$ & $\\mathrm{BF}_{01}$ \\\\",
    "\\midrule \\endhead",
    "\\bottomrule",
    sprintf("\\multicolumn{9}{@{}p{\\textwidth}@{}}{%s} \\\\", note_text),
    "\\endlastfoot"
  )

  for (r in results) {
    t_str <- fmt_num(r$t, 2)
    if (isTRUE(r$significant)) t_str <- paste0(t_str, "*")
    contrast_label <- sprintf("%s \\textit{vs.} %s", r$hi_label, r$lo_label)
    lines <- c(lines, sprintf(
      "%s \\newline \\footnotesize(%s) & %d & %s (%s) & %s & %s & %s & %s & %s & %s \\\\",
      r$cluster, contrast_label, r$n,
      fmt_num(r$mean_diff, 3), fmt_num(r$se_diff, 3),
      t_str, fmt_num(r$df, 1), fmt_p_tex(r$p),
      fmt_num(r$dz, 2), fmt_bf_tex(r$bf10), fmt_bf_tex(r$bf01)))
    lines <- c(lines, "\\addlinespace")
  }

  lines <- c(lines, "\\end{xltabular}", "\\endgroup")
  writeLines(lines, out_path)
}

render_prose_tex <- function(results, n_control, out_path) {
  preface <- sprintf(paste(
    "As a control analysis, we verified that the canonical ERP effects were",
    "present in the comparison (non-lonely) group, since the hypothesised",
    "group differences are only interpretable against a backdrop of intact",
    "basic effects. Within the comparison group ($n=%d$), within-subject",
    "paired \\textit{t}-tests were computed on the mean cluster amplitudes",
    "(extracted as in the main analysis). For the two hypersensitivity",
    "clusters, amplitudes to angry and happy faces at the first presentation",
    "were compared; for the hyperalertness cluster, amplitudes to the first",
    "and fifth presentation of angry faces were compared. Tests were",
    "one-tailed in the pre-specified direction (angry $>$ happy; first $>$",
    "fifth presentation), and the paired effect size was Cohen's",
    "$d_{z}=M_{\\text{diff}}/\\mathit{SD}_{\\text{diff}}$. Directional Bayes",
    "Factors were computed with \\texttt{BayesFactor::ttestBF} on the",
    "within-subject difference scores (one-sample test against zero, positive",
    "interval, default Cauchy prior $r=\\sqrt{2}/2$); $\\mathrm{BF}_{10}$",
    "quantifies evidence for the alternative and $\\mathrm{BF}_{01}$ for the",
    "null. Effects were evaluated at $\\alpha=%.2f$ (the baseline threshold of",
    "the main analysis); as each cluster contributed a single test, no",
    "within-cluster correction was applied. \\textit{p}-values below $10^{-3}$",
    "are reported in scientific notation.",
    sep = " "), n_control, ALPHA)

  lines <- c(preface)

  for (r in results) {
    verb <- if (isTRUE(r$significant)) "showed" else "did not show"
    sentence <- sprintf(paste(
      "In the %s cluster, the comparison group %s the expected %s effect",
      "(%s): the mean within-subject difference was $%s\\,\\mu\\mathrm{V}$",
      "(\\textit{SE} $=%s$), %s."),
      r$cluster, verb, r$contrast, r$direction,
      fmt_num(r$mean_diff, 2), fmt_num(r$se_diff, 2),
      apa_t_d(r$t, r$df, r$p, r$dz, r$bf10, r$bf01))
    lines <- c(lines, "", sentence)
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
    filter(!is.na(amplitude), group == CONTROL_GROUP) %>%
    mutate(
      repetition = as.integer(repetition),
      participant_id = factor(participant_id)
    )

  n_control <- length(unique(amps$participant_id))
  cat("Comparison group (", CONTROL_GROUP, "): n = ", n_control, "\n", sep = "")
  if (n_control < 2) stop("Too few comparison-group participants for paired t-tests.")

  cluster_names <- unique(amps$cluster)
  results <- list()
  for (spec in CONTRASTS) {
    match_idx <- which(startsWith(cluster_names, spec$key))
    if (length(match_idx) != 1) {
      stop("Expected exactly one cluster starting with '", spec$key,
           "'; found ", length(match_idx), " in: ",
           paste(cluster_names, collapse = ", "))
    }
    cn <- cluster_names[match_idx]
    d_cluster <- amps %>% filter(cluster == cn)
    cat("[", format(Sys.time(), "%H:%M:%S"), "] ", spec$contrast,
        " contrast | ", cn, "\n", sep = "")
    results[[length(results) + 1]] <- run_contrast(cn, d_cluster, spec)
  }

  out_report <- args[["out-report"]]
  out_prose <- args[["out-prose-tex"]]
  out_table <- args[["out-table-tex"]]
  dir.create(dirname(out_report), recursive = TRUE, showWarnings = FALSE)

  render_markdown(results, n_control, out_report)
  cat("Wrote ", out_report, "\n", sep = "")
  render_table_tex(results, n_control, out_table)
  cat("Wrote ", out_table, "\n", sep = "")
  render_prose_tex(results, n_control, out_prose)
  cat("Wrote ", out_prose, "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
