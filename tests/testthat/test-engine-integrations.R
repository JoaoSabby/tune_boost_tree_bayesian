library(testthat)
library(TuneBoostTreeBayesian)

TuneBoostTree_TestBinaryData <- function() {

  skip_if_not_installed("modeldata")
  skip_if_not_installed("rsample")
  skip_if_not_installed("tibble")
  data("two_class_dat", package = "modeldata", envir = environment())
  split <- rsample::initial_split(tibble::as_tibble(two_class_dat), prop = 0.75, strata = "Class")
  list(train = rsample::training(split), test = rsample::testing(split))
}
####
## Fim
#

TuneBoostTree_TestFakeLimbo <- function() {

  fakeLimbo <- tempfile("fake-limbo-")
  writeLines(c(
    "#!/bin/sh",
    "candidate=\"$4\"",
    "printf 'learn_rate,tree_depth,min_n,loss_reduction,sample_size\\n0.05,4,8,0,0.8\\n' > \"$candidate\""
  ), fakeLimbo)
  Sys.chmod(fakeLimbo, mode = "0755")
  fakeLimbo
}
####
## Fim
#

TuneBoostTree_TestOptimizer <- function(backend) {

  if(identical(backend, "limbo")) return(TuneBoostTreeOptimizerLimbo(command = TuneBoostTree_TestFakeLimbo(), fallback = FALSE))
  skip_if_not_installed("rBayesianOptimization")
  TuneBoostTreeOptimizerRBayesianOptimization(acquisition = "ucb", kappa = 2.576, eps = 0)
}
####
## Fim
#

TuneBoostTree_TestMetrics <- function(model, testData) {

  skip_if_not_installed("yardstick")
  predictions <- PredictBoostTreeModel(model, testData)
  truthName <- model$targetName
  metricData <- data.frame(
    truth = factor(testData[[truthName]], levels = model$targetLevels),
    estimate = factor(predictions$predictedClass, levels = model$targetLevels),
    probability = predictions$probabilitySecondClass
  )
  list(
    pr_auc = yardstick::pr_auc(metricData, truth, probability, event_level = "second"),
    roc_auc = yardstick::roc_auc(metricData, truth, probability, event_level = "second"),
    sens = yardstick::sens(metricData, truth, estimate, event_level = "second"),
    spec = yardstick::spec(metricData, truth, estimate, event_level = "second"),
    accuracy = yardstick::accuracy(metricData, truth, estimate),
    bal_accuracy = yardstick::bal_accuracy(metricData, truth, estimate, event_level = "second")
  )
}
####
## Fim
#

TuneBoostTree_RunEngineIntegration <- function(engineName, backendName) {

  if(identical(engineName, "xgboost")) skip_if_not_installed("xgboost")
  if(identical(engineName, "lightgbm")) skip_if_not_installed("lightgbm")
  dataSplit <- TuneBoostTree_TestBinaryData()
  optimizer <- TuneBoostTree_TestOptimizer(backendName)
  tuned <- TuneBoostTree(
    Class ~ A + B,
    data = dataSplit$train,
    initial = 2L,
    nIter = 1L,
    engine = engineName,
    boost = TuneBoostTreeBoostParams(trees = 12L, stop_iter = 3L, mtry = 1, max_bin = 64L),
    searchSpace = TuneBoostTreeSearchSpace(
      learn_rate = c(0.03, 0.12),
      tree_depth = c(2L, 4L),
      min_n = c(1L, 12L),
      loss_reduction = c(0, 2),
      sample_size = c(0.7, 1)
    ),
    cv = TuneBoostTreeCv(folds = 2L),
    optimizer = optimizer,
    control = TuneBoostTreeControl(parallel = FALSE, verbose = FALSE)
  )
  model <- FitBoostTreeModel(
    Class ~ A + B,
    dataTrain = dataSplit$train,
    hyperparameters = tuned$bestHyperparameters,
    nThreads = 1L,
    engine_boost_tree = engineName
  )
  predictions <- PredictBoostTreeModel(model, dataSplit$test)
  metrics <- TuneBoostTree_TestMetrics(model, dataSplit$test)
  expect_s3_class(dataSplit$train, "tbl_df")
  expect_true(is.list(tuned$bestHyperparameters))
  expect_true(nrow(predictions) == nrow(dataSplit$test))
  expect_true(all(c("predictedClass", "probabilityFirstClass", "probabilitySecondClass") %in% names(predictions)))
  expect_true(all(vapply(metrics, function(x) is.data.frame(x) && is.finite(x$.estimate[[1L]]), logical(1L))))
}
####
## Fim
#

test_that("pipeline completo lightgbm principal + Limbo fake calcula métricas yardstick", {
  TuneBoostTree_RunEngineIntegration("lightgbm", "limbo")
})

test_that("pipeline completo lightgbm principal + rBayesianOptimization calcula métricas yardstick", {
  TuneBoostTree_RunEngineIntegration("lightgbm", "rBayesianOptimization")
})

test_that("pipeline completo xgboost alternativo + Limbo fake calcula métricas yardstick", {
  TuneBoostTree_RunEngineIntegration("xgboost", "limbo")
})

test_that("pipeline completo xgboost alternativo + rBayesianOptimization calcula métricas yardstick", {
  TuneBoostTree_RunEngineIntegration("xgboost", "rBayesianOptimization")
})
