\name{TuneBoostTreeBayesian}
\alias{TuneBoostTreeBayesian}
\title{Tune Bayesian Gradient Boosted Tree Hyperparameters}
\usage{
TuneBoostTreeBayesian(
  formula,
  dataTrain,
  nFolds = 10L,
  initGridDt = NULL,
  initPoints = 10L,
  nIter = 30L,
  nRoundsTuning = 500L,
  nRoundsFinal = 100L,
  earlyStoppingRounds = 20L,
  seed = 42L,
  nThreads = 8L,
  nWorkersFolds = 1L,
  bounds = NULL,
  featureTypes = NULL,
  evalMetric = "aucpr",
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE,
  scalePosWeight = NULL,
  balanceFn = NULL,
  engine_boost_tree = "xgboost",
  prAucBackend = "auto"
)
}
\arguments{
\item{formula}{A two-sided formula with one binary outcome and numeric predictors.}
\item{dataTrain}{A data frame containing the training rows used during tuning.}
\item{nFolds}{Integer number of stratified folds for cross-validation.}
\item{initGridDt}{Optional data frame returned by a previous call for warm-starting Bayesian optimization.}
\item{initPoints}{Integer number of random initial points when \code{initGridDt} is not enough.}
\item{nIter}{Integer number of Bayesian optimization iterations after initialization.}
\item{nRoundsTuning}{Integer boosting-round ceiling used only inside cross-validation tuning.}
\item{nRoundsFinal}{Integer fallback boosting-round count for the final model when CV cannot infer one.}
\item{earlyStoppingRounds}{Integer patience for aggressive early stopping during tuning.}
\item{seed}{Integer random seed forwarded to fold splitting and model engines.}
\item{nThreads}{Integer maximum threads available to a single sequential model fit.}
\item{nWorkersFolds}{Integer number of fold workers for manual CV.}
\item{bounds}{Named list of Bayesian bounds.}
\item{featureTypes}{Optional XGBoost feature type vector aligned to predictors.}
\item{evalMetric}{Metric name for XGBoost; LightGBM uses \code{average_precision}.}
\item{acq}{Acquisition function passed to \pkg{rBayesianOptimization}.}
\item{kappa}{Exploration parameter passed to Bayesian optimization.}
\item{eps}{Improvement parameter passed to Bayesian optimization.}
\item{verbose}{Logical or integer controlling optimizer and engine verbosity.}
\item{scalePosWeight}{Optional class-imbalance weight for unbalanced CV.}
\item{balanceFn}{Optional function with signature \code{function(data, formula)} applied once per fold.}
\item{engine_boost_tree}{Boosting engine, either \code{"xgboost"} or \code{"lightgbm"}.}
\item{prAucBackend}{PR-AUC implementation, one of \code{"auto"}, \code{"c"}, \code{"fortran"}, \code{"rfast"}, or \code{"r"}.}
}
\value{A list with \code{bestHyperparameters}, \code{bestScore}, \code{initGridDt}, and \code{evaluationLog}.}
\description{Tunes XGBoost or LightGBM binary boosted tree hyperparameters using Bayesian optimization and cached stratified cross-validation.}
