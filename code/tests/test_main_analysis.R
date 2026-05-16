#!/usr/bin/env Rscript
# Unit tests for code/6_main_analysis.R using simulated data.
#
# Run from the repo root with:
#     Rscript code/tests/test_main_analysis.R
#     Rscript code/tests/test_main_analysis.R --report-tsv /tmp/r.tsv
#
# Uses a tiny base-R assertion harness (no testthat dependency). Exits with
# status 1 if any test fails so CI can fail loudly. The optional --report-tsv
# flag emits a machine-readable TSV with columns name, status, duration_ms,
# message; used by the Snakemake `tests` rule.
#
# Sources 6_main_analysis.R; the `if (sys.nframe() == 0L) main()` guard there
# prevents main() from running on source().

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) sub("^--file=", "", m[[1]]) else NA_character_
}
SCRIPT_DIR <- {
  p <- script_path()
  if (!is.na(p)) dirname(normalizePath(p, mustWork = FALSE)) else getwd()
}
ANALYSIS_R <- file.path(SCRIPT_DIR, "..", "6_main_analysis.R")
if (!file.exists(ANALYSIS_R)) {
  ANALYSIS_R <- file.path("code", "6_main_analysis.R")
}
source(ANALYSIS_R, local = FALSE)

# ---------------------------------------------------------------------------
# CLI parsing (only --report-tsv is consumed here; everything else is silent)
# ---------------------------------------------------------------------------
parse_test_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list()
  i <- 1L
  while (i <= length(args)) {
    if (i + 1L <= length(args)) {
      out[[sub("^--", "", args[[i]])]] <- args[[i + 1L]]
      i <- i + 2L
    } else {
      i <- i + 1L
    }
  }
  out
}
TEST_ARGS <- parse_test_args()

# ---------------------------------------------------------------------------
# Tiny assertion harness
# ---------------------------------------------------------------------------
.n_pass <- 0L; .n_fail <- 0L; .failures <- character()
.records <- list()

test_that <- function(desc, body) {
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(body(), error = function(e) e)
  dt_ms <- (proc.time()[["elapsed"]] - t0) * 1000
  if (inherits(res, "error")) {
    .n_fail <<- .n_fail + 1L
    msg <- conditionMessage(res)
    .failures <<- c(.failures, sprintf("%s :: %s", desc, msg))
    .records[[length(.records) + 1L]] <<- list(name = desc, status = "fail",
                                                duration_ms = dt_ms,
                                                message = msg)
    cat(sprintf("FAIL  %s\n        %s\n", desc, msg))
  } else {
    .n_pass <<- .n_pass + 1L
    .records[[length(.records) + 1L]] <<- list(name = desc, status = "pass",
                                                duration_ms = dt_ms,
                                                message = "")
    cat(sprintf("ok    %s\n", desc))
  }
}

expect_identical_ <- function(actual, expected) {
  if (!identical(actual, expected)) {
    stop(sprintf("expected %s, got %s",
                 paste(deparse(expected), collapse = " "),
                 paste(deparse(actual), collapse = " ")))
  }
}
expect_equal_ <- function(actual, expected, tol = 1e-6) {
  if (length(actual) != length(expected))
    stop(sprintf("length mismatch: %d vs %d", length(actual), length(expected)))
  diff <- abs(as.numeric(actual) - as.numeric(expected))
  if (any(diff > tol & !is.na(diff)))
    stop(sprintf("values differ beyond tol=%g: %s vs %s", tol,
                 toString(actual), toString(expected)))
}
expect_true_ <- function(x) {
  if (!isTRUE(x)) stop(sprintf("expected TRUE, got %s",
                                paste(deparse(x), collapse = " ")))
}
expect_match_ <- function(s, pattern, fixed = FALSE) {
  if (!any(grepl(pattern, s, fixed = fixed)))
    stop(sprintf("no match for /%s/ in:\n  %s", pattern, paste(s, collapse = "\n  ")))
}
expect_length_ <- function(x, n) {
  if (length(x) != n) stop(sprintf("length %d != %d", length(x), n))
}

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------
test_that("fmt_p formats small p as '< .001'", function() {
  expect_identical_(fmt_p(0.0001), "< .001")
  expect_identical_(fmt_p(0.0005), "< .001")
})

test_that("fmt_p drops leading zero", function() {
  expect_identical_(fmt_p(0.025), ".025")
  expect_identical_(fmt_p(0.5),   ".500")
})

test_that("fmt_p returns 'n/a' for non-finite", function() {
  expect_identical_(fmt_p(NA_real_), "n/a")
  expect_identical_(fmt_p(NaN), "n/a")
  expect_identical_(fmt_p(Inf), "n/a")
})

test_that("fmt_p_apa wraps with italic p and uses '< .001' threshold", function() {
  expect_identical_(fmt_p_apa(0.0001), "\\textit{p} < .001")
  expect_identical_(fmt_p_apa(0.04),   "\\textit{p} = .040")
})

test_that("fmt_p_tex emits LaTeX-friendly small-p", function() {
  expect_identical_(fmt_p_tex(0.0001), "$<$ .001")
  expect_identical_(fmt_p_tex(0.04),   ".040")
})

test_that("fmt_num rounds to specified digits", function() {
  expect_identical_(fmt_num(1.236, 2), "1.24")
  expect_identical_(fmt_num(0.0,   1), "0.0")
  expect_identical_(fmt_num(NA_real_),  "n/a")
})

test_that("fmt_eta2 drops leading zero", function() {
  expect_identical_(fmt_eta2(0.123), ".123")
  expect_identical_(fmt_eta2(NA_real_), "n/a")
})

test_that("apa_f assembles the canonical APA F-string", function() {
  s <- apa_f(F_val = 5.23, df1 = 1, df2 = 38, p = 0.027, eta2 = 0.121)
  expect_match_(s, "F\\}\\(1, 38\\)")
  expect_match_(s, "5\\.23")
  expect_match_(s, "\\.027")
  expect_match_(s, "\\.121")
})

test_that("apa_t assembles the canonical APA t-string", function() {
  s <- apa_t(t_val = 2.10, df = 37.4, p = 0.043, d = 0.65)
  expect_match_(s, "t\\}\\(37\\.4\\)")
  expect_match_(s, "2\\.10")
  expect_match_(s, "\\.043")
  expect_match_(s, "0\\.65")
})

# ---------------------------------------------------------------------------
# welch_d
# ---------------------------------------------------------------------------
test_that("welch_d matches base t.test on a known sample", function() {
  set.seed(1)
  a <- rnorm(20, mean = 0,   sd = 1)
  b <- rnorm(25, mean = 0.7, sd = 1.2)
  r <- welch_d(a, b)
  baseline <- t.test(a, b, var.equal = FALSE)
  expect_equal_(r$t, unname(baseline$statistic))
  expect_equal_(r$df, unname(baseline$parameter))
  expect_equal_(r$p, unname(baseline$p.value))
  expect_equal_(r$n_a, 20)
  expect_equal_(r$n_b, 25)
})

test_that("welch_d returns NA fields when a group has < 2 observations", function() {
  r <- welch_d(c(1.0), c(2.0, 3.0, 4.0))
  expect_true_(is.na(r$t)); expect_true_(is.na(r$p)); expect_true_(is.na(r$d))
  expect_equal_(r$n_a, 1)
  expect_equal_(r$n_b, 3)
  expect_equal_(r$mean_a, 1.0)  # mean still reported for the single obs
})

test_that("welch_d drops NAs before counting", function() {
  r <- welch_d(c(1, NA, 2, 3), c(4, 5, NA, 6))
  expect_equal_(r$n_a, 3); expect_equal_(r$n_b, 3)
})

test_that("welch_d computes Cohen's d with pooled SD", function() {
  a <- c(1, 2, 3, 4, 5)
  b <- c(3, 4, 5, 6, 7)
  r <- welch_d(a, b)
  # Mean diff = -2; pooled SD = sqrt(((4*var(a)) + (4*var(b))) / 8) = sd of each = sqrt(2.5)
  expect_equal_(r$d, (mean(a) - mean(b)) / sqrt(2.5))
})

# ---------------------------------------------------------------------------
# complete_subjects
# ---------------------------------------------------------------------------
test_that("complete_subjects keeps participants with all 4 non-NA cells", function() {
  d <- tibble(
    participant_id = rep(c("s1", "s2", "s3", "s4"), each = 4),
    emotion = rep(c("angry", "angry", "happy", "happy"), 4),
    repetition = rep(c("1", "5", "1", "5"), 4),
    amplitude = c( 1, 2, 3, 4,        # s1: complete
                   1, NA, 3, 4,       # s2: NA -> drop
                   1, 2, 3, 4,        # s3: complete
                   1, 2, 3, NA)       # s4: NA -> drop
  )
  kept <- complete_subjects(d)
  expect_identical_(sort(kept), c("s1", "s3"))
})

test_that("complete_subjects drops participants missing cells entirely", function() {
  d <- tibble(
    participant_id = c("s1", "s1", "s1", "s1", "s2", "s2", "s2"),
    emotion       = c("angry","angry","happy","happy","angry","angry","happy"),
    repetition    = c("1","5","1","5","1","5","1"),
    amplitude     = c(1, 2, 3, 4, 1, 2, 3)  # s2 missing happy/5
  )
  kept <- complete_subjects(d)
  expect_identical_(kept, "s1")
})

# ---------------------------------------------------------------------------
# welch_row
# ---------------------------------------------------------------------------
test_that("welch_row returns a 1-row tibble with the expected columns", function() {
  d <- tibble(
    participant_id = paste0("s", 1:8),
    group = rep(c("Lonely", "Non-Lonely"), each = 4),
    amplitude = c(1, 2, 3, 4, 5, 6, 7, 8)
  )
  row <- welch_row(d, "test cell")
  expected_cols <- c("comparison", "n_lonely", "n_nonlonely",
                     "mean_lonely", "se_lonely",
                     "mean_nonlonely", "se_nonlonely",
                     "t", "df", "p", "d")
  expect_identical_(colnames(row), expected_cols)
  expect_equal_(nrow(row), 1)
  expect_identical_(row$comparison, "test cell")
  expect_equal_(row$n_lonely, 4); expect_equal_(row$n_nonlonely, 4)
  expect_equal_(row$mean_lonely, 2.5)
  expect_equal_(row$mean_nonlonely, 6.5)
})

# ---------------------------------------------------------------------------
# fit_mixed_anova
# ---------------------------------------------------------------------------
# Simulate long-format amplitude data with user-specified effect sizes.
# Each factor is sum-coded to +/-0.5, so an `_effect` argument equals the full
# difference between levels (or the difference of differences, for interactions).
# Subject-level random intercepts give realistic within-subject correlation.
simulate_long <- function(n_per_group = 12,
                          group_effect = 0,
                          emotion_effect = 0,
                          repetition_effect = 0,
                          emotion_x_group = 0,
                          repetition_x_group = 0,
                          emotion_x_repetition = 0,
                          three_way = 0,
                          sd_subject = 0.4,
                          sd_residual = 0.5,
                          seed = 42) {
  set.seed(seed)
  subjects <- paste0("s", sprintf("%03d", seq_len(2 * n_per_group)))
  group_lookup <- rep(c("Lonely", "Non-Lonely"), each = n_per_group)
  grid <- expand.grid(participant_id = subjects,
                      emotion = c("angry", "happy"),
                      repetition = c("1", "5"),
                      stringsAsFactors = FALSE)
  grid$group <- group_lookup[match(grid$participant_id, subjects)]

  g <- ifelse(grid$group      == "Lonely",    -0.5, 0.5)
  e <- ifelse(grid$emotion    == "angry",     -0.5, 0.5)
  r <- ifelse(grid$repetition == "1",         -0.5, 0.5)

  subj_re <- setNames(rnorm(length(subjects), sd = sd_subject), subjects)
  grid$amplitude <-
    subj_re[grid$participant_id] +
    rnorm(nrow(grid), sd = sd_residual) +
    group_effect          * g +
    emotion_effect        * e +
    repetition_effect     * r +
    emotion_x_group       * e * g +
    repetition_x_group    * r * g +
    emotion_x_repetition  * e * r +
    three_way             * e * r * g

  grid %>% mutate(
    emotion        = factor(emotion,        levels = c("angry", "happy")),
    repetition     = factor(repetition,     levels = c("1", "5")),
    group          = factor(group,          levels = c("Lonely", "Non-Lonely")),
    participant_id = factor(participant_id)
  )
}

# Convenience: pull the (uncorrected) p-value for a single ANOVA Source row.
p_for <- function(tbl, source) tbl$p_unc[tbl$Source == source]

test_that("fit_mixed_anova returns the expected schema and row order", function() {
  d <- simulate_long(n_per_group = 12, group_effect = 0)
  tbl <- fit_mixed_anova(d)
  expect_identical_(colnames(tbl),
                    c("Source", "df1", "df2", "F", "p_unc", "pes",
                      "p_bonf", "significant_at_alpha_0.02"))
  expect_identical_(tbl$Source,
                    c("group", "emotion", "repetition",
                      "emotion x group", "repetition x group",
                      "emotion x repetition", "emotion x repetition x group"))
  expect_equal_(nrow(tbl), 7)
})

test_that("fit_mixed_anova detects an injected between-group effect", function() {
  d <- simulate_long(n_per_group = 20, group_effect = 4.0, seed = 7)
  tbl <- fit_mixed_anova(d)
  grp <- tbl[tbl$Source == "group", ]
  expect_true_(grp$F > 5)
  expect_true_(grp$p_unc < 0.01)
  expect_true_(grp$significant_at_alpha_0.02)
})

test_that("fit_mixed_anova applies Bonferroni *3 to p_bonf", function() {
  d <- simulate_long(n_per_group = 12, group_effect = 0, seed = 11)
  tbl <- fit_mixed_anova(d)
  # p_bonf == min(p_unc * 3, 1)
  expect_equal_(tbl$p_bonf, pmin(tbl$p_unc * 3, 1.0))
})

# Effect-recovery tests: inject one effect at a time and check that the ANOVA
# detects it. Effect sizes are large (>= 1.5) so the tests are robust to seed
# choice; each test uses a fixed seed for reproducibility. Within-subject
# effects are tested at p < 0.001 (high power once subject variance is
# partialled out); between-subject effects at p < 0.01.

test_that("fit_mixed_anova detects emotion main effect", function() {
  d <- simulate_long(n_per_group = 20, emotion_effect = 1.5, seed = 101)
  tbl <- fit_mixed_anova(d)
  expect_true_(p_for(tbl, "emotion") < 0.001)
  expect_true_(tbl$significant_at_alpha_0.02[tbl$Source == "emotion"])
})

test_that("fit_mixed_anova detects repetition main effect", function() {
  d <- simulate_long(n_per_group = 20, repetition_effect = 1.5, seed = 102)
  tbl <- fit_mixed_anova(d)
  expect_true_(p_for(tbl, "repetition") < 0.001)
  expect_true_(tbl$significant_at_alpha_0.02[tbl$Source == "repetition"])
})

test_that("fit_mixed_anova detects group main effect", function() {
  d <- simulate_long(n_per_group = 20, group_effect = 2.0, seed = 103)
  tbl <- fit_mixed_anova(d)
  expect_true_(p_for(tbl, "group") < 0.01)
  expect_true_(tbl$significant_at_alpha_0.02[tbl$Source == "group"])
})

test_that("fit_mixed_anova detects emotion x group interaction", function() {
  d <- simulate_long(n_per_group = 20, emotion_x_group = 2.0, seed = 104)
  tbl <- fit_mixed_anova(d)
  expect_true_(p_for(tbl, "emotion x group") < 0.001)
  expect_true_(tbl$significant_at_alpha_0.02[tbl$Source == "emotion x group"])
})

test_that("fit_mixed_anova detects repetition x group interaction", function() {
  d <- simulate_long(n_per_group = 20, repetition_x_group = 2.0, seed = 105)
  tbl <- fit_mixed_anova(d)
  expect_true_(p_for(tbl, "repetition x group") < 0.001)
  expect_true_(tbl$significant_at_alpha_0.02[tbl$Source == "repetition x group"])
})

test_that("fit_mixed_anova detects emotion x repetition interaction", function() {
  d <- simulate_long(n_per_group = 20, emotion_x_repetition = 1.5, seed = 106)
  tbl <- fit_mixed_anova(d)
  expect_true_(p_for(tbl, "emotion x repetition") < 0.001)
  expect_true_(tbl$significant_at_alpha_0.02[tbl$Source == "emotion x repetition"])
})

test_that("fit_mixed_anova detects 3-way emotion x repetition x group", function() {
  d <- simulate_long(n_per_group = 20, three_way = 3.0, seed = 107)
  tbl <- fit_mixed_anova(d)
  expect_true_(p_for(tbl, "emotion x repetition x group") < 0.01)
  expect_true_(tbl$significant_at_alpha_0.02[
    tbl$Source == "emotion x repetition x group"])
})

test_that("fit_mixed_anova does not flag spurious effects under a single injection", function() {
  # Inject only emotion. The other six terms should all sit at p > 0.05 with
  # this seed. This guards against the simulator accidentally introducing
  # correlated effects.
  d <- simulate_long(n_per_group = 20, emotion_effect = 1.5, seed = 201)
  tbl <- fit_mixed_anova(d)
  for (src in c("group", "repetition",
                "emotion x group", "repetition x group",
                "emotion x repetition", "emotion x repetition x group")) {
    if (!(p_for(tbl, src) > 0.05))
      stop(sprintf("spurious effect at %s: p = %.4f", src, p_for(tbl, src)))
  }
})

# ---------------------------------------------------------------------------
# decompose_interactions
# ---------------------------------------------------------------------------
make_fake_anova_tbl <- function(sig_terms) {
  sources <- c("group", "emotion", "repetition",
               "emotion x group", "repetition x group",
               "emotion x repetition", "emotion x repetition x group")
  tibble(
    Source = sources,
    df1 = rep(1, 7), df2 = rep(20, 7),
    F = rep(1, 7), p_unc = rep(0.5, 7), pes = rep(0.01, 7),
    p_bonf = rep(1.0, 7),
    significant_at_alpha_0.02 = sources %in% sig_terms
  )
}

make_fake_long <- function(n_per_group = 4) {
  subjects <- paste0("s", sprintf("%03d", seq_len(2 * n_per_group)))
  group <- rep(c("Lonely", "Non-Lonely"), each = n_per_group)
  grid <- expand.grid(participant_id = subjects,
                      emotion = c("angry", "happy"),
                      repetition = c("1", "5"),
                      stringsAsFactors = FALSE)
  grid$group <- group[match(grid$participant_id, subjects)]
  set.seed(99)
  grid$amplitude <- rnorm(nrow(grid))
  grid
}

test_that("decompose_interactions: 3-way sig -> 4 between-group tests, alpha = .0125", function() {
  d <- make_fake_long()
  anova_tbl <- make_fake_anova_tbl("emotion x repetition x group")
  res <- decompose_interactions(d, anova_tbl)
  expect_equal_(nrow(res$tests), 4)
  expect_equal_(res$alpha, 0.05 / 4)
  # All 4 emotion x repetition cells covered.
  expect_match_(res$tests$comparison, "angry, rep 1")
  expect_match_(res$tests$comparison, "angry, rep 5")
  expect_match_(res$tests$comparison, "happy, rep 1")
  expect_match_(res$tests$comparison, "happy, rep 5")
  expect_identical_(res$rationale, "3-way (emotion x repetition x group)")
})

test_that("decompose_interactions: repetition x group sig -> 2 tests, one per repetition", function() {
  d <- make_fake_long()
  anova_tbl <- make_fake_anova_tbl("repetition x group")
  res <- decompose_interactions(d, anova_tbl)
  expect_equal_(nrow(res$tests), 2)
  expect_equal_(res$alpha, 0.05 / 2)
  expect_match_(res$tests$comparison, "at rep 1")
  expect_match_(res$tests$comparison, "at rep 5")
})

test_that("decompose_interactions: emotion x group sig -> 2 tests, one per emotion", function() {
  d <- make_fake_long()
  anova_tbl <- make_fake_anova_tbl("emotion x group")
  res <- decompose_interactions(d, anova_tbl)
  expect_equal_(nrow(res$tests), 2)
  expect_equal_(res$alpha, 0.05 / 2)
  expect_match_(res$tests$comparison, "at angry")
  expect_match_(res$tests$comparison, "at happy")
})

test_that("decompose_interactions: group main only -> 1 overall test, alpha = .05", function() {
  d <- make_fake_long()
  anova_tbl <- make_fake_anova_tbl("group")
  res <- decompose_interactions(d, anova_tbl)
  expect_equal_(nrow(res$tests), 1)
  expect_equal_(res$alpha, 0.05)
  expect_match_(res$tests$comparison, "overall")
  expect_identical_(res$rationale, "group main effect (overall)")
})

test_that("decompose_interactions: no group-involving sig -> NULL tests", function() {
  d <- make_fake_long()
  anova_tbl <- make_fake_anova_tbl(c("emotion", "repetition",
                                     "emotion x repetition"))
  res <- decompose_interactions(d, anova_tbl)
  expect_true_(is.null(res$tests))
  expect_true_(is.null(res$rationale))
})

test_that("decompose_interactions: 3-way trumps lower-order group interactions", function() {
  # When both 3-way and a 2-way-with-group are significant, only the 4-cell
  # decomposition should run (3-way branch is taken; 2-ways are not duplicated).
  d <- make_fake_long()
  anova_tbl <- make_fake_anova_tbl(c("emotion x repetition x group",
                                     "repetition x group"))
  res <- decompose_interactions(d, anova_tbl)
  expect_equal_(nrow(res$tests), 4)
  expect_equal_(res$alpha, 0.05 / 4)
})

test_that("decompose_interactions: Bonferroni p column is p * k clipped at 1", function() {
  d <- make_fake_long()
  anova_tbl <- make_fake_anova_tbl("emotion x group")
  res <- decompose_interactions(d, anova_tbl)
  k <- nrow(res$tests)
  expect_equal_(res$tests$p_bonferroni, pmin(res$tests$p * k, 1.0))
})

# ---------------------------------------------------------------------------
# Renderers (smoke tests — content checked at a coarse level)
# ---------------------------------------------------------------------------
fake_per_cluster <- function() {
  anova_tbl <- make_fake_anova_tbl("group")
  anova_tbl$F[anova_tbl$Source == "group"] <- 6.50
  anova_tbl$p_unc[anova_tbl$Source == "group"] <- 0.015
  anova_tbl$pes[anova_tbl$Source == "group"] <- 0.18

  posthoc_tests <- tibble(
    comparison = "Lonely vs Non-Lonely (overall)",
    n_lonely = 20, n_nonlonely = 22,
    mean_lonely = -1.10, se_lonely = 0.18,
    mean_nonlonely = -0.40, se_nonlonely = 0.16,
    t = -2.85, df = 39.2, p = 0.007, d = -0.85,
    alpha_bonf = 0.05, p_bonferroni = 0.007, significant = TRUE
  )

  desc_tbl <- expand.grid(
    group = c("Lonely", "Non-Lonely"),
    emotion = c("angry", "happy"),
    repetition = c("1", "5"),
    stringsAsFactors = FALSE
  )
  desc_tbl$n <- 20
  desc_tbl$mean <- c(-1.1, -0.4, -1.0, -0.3, -1.05, -0.42, -0.95, -0.38)
  desc_tbl$se <- 0.15

  list(list(
    name = "frontal-N100",
    n = 42,
    anova = anova_tbl,
    posthoc = list(rationale = "group main effect (overall)",
                   tests = posthoc_tests, alpha = 0.05),
    desc = desc_tbl
  ))
}

test_that("render_markdown writes a report mentioning the cluster and ANOVA", function() {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  per_cluster <- fake_per_cluster()
  sample_info <- list(n_total = 42, n_lonely = 20, n_nonlonely = 22)
  render_markdown(per_cluster, sample_info, tmp)
  body <- paste(readLines(tmp), collapse = "\n")
  expect_match_(body, "Main Analysis Report")
  expect_match_(body, "frontal-N100")
  expect_match_(body, "Mixed ANOVA")
  expect_match_(body, "Lonely vs Non-Lonely")
})

test_that("render_table_tex writes a LaTeX xltabular with the cluster header", function() {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)
  per_cluster <- fake_per_cluster()
  render_table_tex(per_cluster, tmp)
  body <- paste(readLines(tmp), collapse = "\n")
  expect_match_(body, "\\\\begin\\{xltabular\\}")
  expect_match_(body, "frontal-N100")
})

test_that("render_prose_tex writes a sample-size sentence and a per-cluster sentence", function() {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)
  per_cluster <- fake_per_cluster()
  sample_info <- list(n_total = 42, n_lonely = 20, n_nonlonely = 22)
  render_prose_tex(per_cluster, sample_info, tmp)
  body <- paste(readLines(tmp), collapse = "\n")
  expect_match_(body, "analytic sample")
  expect_match_(body, "frontal-N100 cluster")
  expect_match_(body, "mixed ANOVA")
})

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat(sprintf("\n%d passed, %d failed.\n", .n_pass, .n_fail))

# Optional TSV report (consumed by code/tests/render_report.py).
if (!is.null(TEST_ARGS[["report-tsv"]])) {
  path <- TEST_ARGS[["report-tsv"]]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- do.call(rbind, lapply(.records, function(r) data.frame(
    name = r$name,
    status = r$status,
    duration_ms = sprintf("%.2f", r$duration_ms),
    message = gsub("[\t\n]", " ", r$message),
    stringsAsFactors = FALSE)))
  write.table(df, path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
}

if (.n_fail > 0L) {
  cat("\nFailures:\n")
  for (f in .failures) cat("  -", f, "\n")
  quit(status = 1L, save = "no")
}
