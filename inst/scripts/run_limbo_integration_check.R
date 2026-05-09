#!/usr/bin/env Rscript

# Executa uma tunagem pequena de ponta a ponta usando o executável ask/tell configurado em TBTB_LIMBO_COMMAND

command <- Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_)
if(is.na(command) || !nzchar(command) || file.access(command, mode = 1L) != 0L){
  stop("TBTB_LIMBO_COMMAND must point to an executable ask/tell adapter.", call. = FALSE)
}

suppressPackageStartupMessages(library(TuneBoostTreeBayesian))

set.seed(2026L)
n <- 72L
x1 <- rnorm(n)
x2 <- runif(n)
linear <- x1 + 0.75 * x2 + rnorm(n, sd = 0.35)
y <- factor(ifelse(linear > stats::median(linear), "pos", "neg"), levels = c("neg", "pos"))
trainingData <- data.frame(y = y, x1 = x1, x2 = x2)

result <- TuneBoostTree(
  y ~ x1 + x2,
  data = trainingData,
  initial = 2L,
  nIter = 2L,
  engine = "xgboost",
  boost = TuneBoostTreeBoostParams(trees = 8L, stop_iter = 2L, mtry = 1),
  searchSpace = TuneBoostTreeSearchSpace(
    learn_rate = c(0.05, 0.2),
    tree_depth = c(2L, 3L),
    min_n = c(1L, 4L),
    loss_reduction = c(0, 1),
    sample_size = c(0.8, 1),
    mtry = NULL
  ),
  cv = TuneBoostTreeCv(folds = 2L),
  optimizer = TuneBoostTreeOptimizerLimbo(command = command, fallback = FALSE),
  performance = TuneBoostTreePerformance(metric = "pr_auc", backend = "r"),
  control = TuneBoostTreeControl(parallel = FALSE, verbose = FALSE)
)

if(!is.list(result) || !is.finite(result$bestScore)){
  stop("Limbo integration returned a non-finite bestScore.", call. = FALSE)
}
if(!identical(result$config$optimizer$type, "limbo")){
  stop("Optimizer type is not limbo.", call. = FALSE)
}
if(is.null(result$evaluationLog) || nrow(result$evaluationLog) < 2L){
  stop("Evaluation log is unexpectedly empty.", call. = FALSE)
}
if(!is.list(result$bestThreshold) || !all(c("threshold", "metric", "score") %in% names(result$bestThreshold))){
  stop("Threshold summary is incomplete.", call. = FALSE)
}

cat("TBTB_LIMBO_REAL_INTEGRATION_OK\n")
cat("command=", command, "\n", sep = "")
cat("bestScore=", format(result$bestScore, digits = 8L), "\n", sep = "")
cat("evaluations=", nrow(result$evaluationLog), "\n", sep = "")
cat("threshold=", format(result$bestThreshold$threshold, digits = 8L), "\n", sep = "")
