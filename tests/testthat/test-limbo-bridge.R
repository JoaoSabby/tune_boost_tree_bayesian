library(testthat)
library(TuneBoostTreeBayesian)

TuneBoostTree_TestLimboBounds <- function() {

  TuneBoostTreeSearchSpace(
    learn_rate = c(0.01, 0.2),
    tree_depth = c(2L, 4L),
    min_n = c(1L, 5L),
    loss_reduction = c(0, 2),
    sample_size = c(0.7, 1),
    mtry = NULL
  )
}
####
## Fim
#

TuneBoostTree_WriteLimboScript <- function(lines) {

  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("sh")), "A POSIX shell is required for fake Limbo tests.")
  fakeLimbo <- tempfile("fake-limbo-")
  writeLines(c("#!/bin/sh", lines), fakeLimbo)
  Sys.chmod(fakeLimbo, mode = "0755")
  fakeLimbo
}
####
## Fim
#

test_that("Limbo ask/tell candidate is validated, clamped, and normalized", {
  bounds <- TuneBoostTree_TestLimboBounds()
  history <- data.frame(
    learn_rate = 0.05,
    tree_depth = 3L,
    min_n = 2L,
    sample_size = 0.8,
    loss_reduction = 0.5,
    Value = 0.7
  )
  fakeLimbo <- TuneBoostTree_WriteLimboScript(c(
    "candidate=\"$4\"",
    "cat > \"$candidate\" <<'CSV'",
    "loss_reduction,sample_size,min_n,tree_depth,learn_rate",
    "9,0.1,-5,99,1.5",
    "CSV"
  ))

  candidate <- TuneBoostTreeBayesian:::TuneBoostTree_RequestLimboCandidate(
    fakeLimbo,
    bounds,
    history,
    seed = 123L,
    iteration = 2L
  )

  expect_equal(names(candidate), names(bounds))
  expect_equal(candidate$learn_rate, 0.2)
  expect_equal(candidate$tree_depth, 4L)
  expect_equal(candidate$min_n, 1L)
  expect_equal(candidate$sample_size, 0.7)
  expect_equal(candidate$loss_reduction, 2)
})

test_that("Limbo ask/tell rejects malformed candidate files", {
  bounds <- TuneBoostTree_TestLimboBounds()
  history <- data.frame(
    learn_rate = 0.05,
    tree_depth = 3L,
    min_n = 2L,
    sample_size = 0.8,
    loss_reduction = 0.5,
    Value = 0.7
  )
  fakeLimbo <- TuneBoostTree_WriteLimboScript(c(
    "candidate=\"$4\"",
    "cat > \"$candidate\" <<'CSV'",
    "learn_rate,tree_depth,min_n,sample_size",
    "0.05,3,2,0.8",
    "CSV"
  ))

  expect_error(
    TuneBoostTreeBayesian:::TuneBoostTree_RequestLimboCandidate(fakeLimbo, bounds, history),
    "missing required column"
  )
})

test_that("Limbo installer supports syntax check and dry-run smoke test", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("bash")), "bash is required for install_limbo.sh tests.")
  script <- file.path(getwd(), "inst", "scripts", "install_limbo.sh")
  skip_if(!file.exists(script), "install_limbo.sh is not available.")
  prefix <- tempfile("tbtb-limbo-")

  syntaxStatus <- system2("bash", c("-n", script))
  expect_equal(syntaxStatus, 0L)

  dryRunOutput <- system2(
    "bash",
    c(script, "--dry-run", "--no-system-deps", "--no-profile", "--no-renviron", "--prefix", prefix, "--timeout", "30"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(dryRunOutput, "status"))
  expect_true(any(grepl("TBTB_LIMBO_COMMAND", dryRunOutput, fixed = TRUE)))
  expect_true(any(grepl("dry-run", dryRunOutput, fixed = TRUE)))
})
