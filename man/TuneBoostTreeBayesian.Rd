\name{TuneBoostTreeBayesian}
\alias{TuneBoostTreeBayesian}
\title{Tune Bayesian Gradient Boosted Tree Hyperparameters}
\usage{
TuneBoostTreeBayesian(
  formula,
  data,
  initial = 10L,
  nIter = 30L,
  engine = "lightgbm",
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
\item{formula}{A two-sided formula with one binary factor outcome and numeric predictors.}
\item{data}{A data.frame, tibble, or data.table containing training rows. The target must be a factor with exactly two levels.}
\item{initial}{Either \code{NULL}, an integer number of random initial points, or a tabular warm-start grid containing parameter columns and \code{Value}.}
\item{nIter}{Integer number of optimization iterations after initialization.}
\item{engine}{\code{"lightgbm"} by default, \code{"xgboost"} as an alternative, or an engine configuration from \code{TuneBoostTreeXgboost()} or \code{TuneBoostTreeLightgbm()}.}
\item{boost}{Boosting defaults from \code{TuneBoostTreeBoostParams()}.}
\item{search_space}{Bayesian search bounds from \code{TuneBoostTreeSearchSpace()}.}
\item{cv}{Cross-validation settings from \code{TuneBoostTreeCv()}.}
\item{optimizer}{Optimizer settings from \code{TuneBoostTreeLimbo()}, \code{TuneBoostTreeRBayesianOptimization()}, or \code{TuneBoostTreeInternalOptimizer()}.}
\item{imbalance}{Class imbalance settings from \code{TuneBoostTreeImbalance()}.}
\item{performance}{Performance/scoring settings from \code{TuneBoostTreePerformance()}.}
\item{control}{Runtime controls from \code{TuneBoostTreeControl()}.}
}
\value{A list with \code{bestHyperparameters}, \code{bestScore}, \code{bestThreshold}, \code{initial}, \code{evaluationLog}, and \code{config}. \code{bestHyperparameters$threshold} stores the F1-optimized out-of-fold decision threshold.}
\description{Tunes LightGBM or XGBoost binary boosted tree hyperparameters using parsnip-style boost-tree parameter names, cached stratified cross-validation, warm starts, Limbo/internal optimization, and configurable PR-AUC scoring.}
\details{
The positive class is selected deterministically from the target factor: the rarer level is positive; if both levels have the same count, the first level in \code{levels(target)} is positive. The class ordering stored in the result is \code{c(negative, positive)}.

The tuner validates cross-validation class counts before fitting. If the minority class has fewer observations than the requested fold count, the tuner emits a warning and reduces the fold count to the minority-class count so that every validation fold contains both classes and PR-AUC is not silently averaged over invalid folds.

After selecting hyperparameters by PR-AUC, the tuner refits fold models at the selected number of trees and computes out-of-fold probabilities to choose an F1-optimized probability threshold. \code{PredictBoostTreeModel()} uses the stored threshold by default when the model was fitted with tuned hyperparameters.
}
