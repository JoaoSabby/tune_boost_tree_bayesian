\name{TuneBoostTreeBayesian}
\alias{TuneBoostTreeBayesian}
\title{Tune Bayesian Gradient Boosted Tree Hyperparameters}
\usage{
TuneBoostTreeBayesian(
  formula,
  data,
  initial = 10L,
  nIter = 30L,
  engine = "xgboost",
  boost = TuneBoostTreeBoostParams(),
  search_space = TuneBoostTreeSearchSpace(),
  cv = TuneBoostTreeCv(),
  optimizer = TuneBoostTreeLimbo(),
  imbalance = TuneBoostTreeImbalance(),
  performance = TuneBoostTreePerformance(),
  control = TuneBoostTreeControl()
)
}
\arguments{
\item{formula}{A two-sided formula with one binary outcome and numeric predictors.}
\item{data}{A data.frame, tibble, or data.table containing training rows.}
\item{initial}{Either \code{NULL}, an integer number of random initial points, or a tabular warm-start grid containing parameter columns and \code{Value}.}
\item{nIter}{Integer number of optimization iterations after initialization.}
\item{engine}{Either \code{"xgboost"}, \code{"lightgbm"}, or an engine configuration from \code{TuneBoostTreeXgboost()} or \code{TuneBoostTreeLightgbm()}.}
\item{boost}{Boosting defaults from \code{TuneBoostTreeBoostParams()}.}
\item{search_space}{Bayesian search bounds from \code{TuneBoostTreeSearchSpace()}.}
\item{cv}{Cross-validation settings from \code{TuneBoostTreeCv()}.}
\item{optimizer}{Optimizer settings from \code{TuneBoostTreeLimbo()} or \code{TuneBoostTreeInternalOptimizer()}.}
\item{imbalance}{Class imbalance settings from \code{TuneBoostTreeImbalance()}.}
\item{performance}{Performance/scoring settings from \code{TuneBoostTreePerformance()}.}
\item{control}{Runtime controls from \code{TuneBoostTreeControl()}.}
}
\value{A list with \code{bestHyperparameters}, \code{bestScore}, \code{initial}, \code{evaluationLog}, and \code{config}.}
\description{Tunes XGBoost or LightGBM binary boosted tree hyperparameters using parsnip-style boost-tree parameter names, cached stratified cross-validation, warm starts, Limbo ask/tell optimization, and configurable PR-AUC scoring.}
