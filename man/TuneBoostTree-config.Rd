\name{TuneBoostTreeBoostParams}
\alias{TuneBoostTreeBoostParams}
\alias{TuneBoostTreeSearchSpace}
\alias{TuneBoostTreeCv}
\alias{TuneBoostTreeOptimizerLimbo}
\alias{TuneBoostTreeLimbo}
\alias{TuneBoostTreeInternalOptimizer}
\alias{TuneBoostTreeOptimizerRBayesianOptimization}
\alias{TuneBoostTreeRBayesianOptimization}
\alias{TuneBoostTreeImbalance}
\alias{TuneBoostTreePerformance}
\alias{TuneBoostTreeControl}
\alias{TuneBoostTreeParallel}
\alias{TuneBoostTreeXgboost}
\alias{TuneBoostTreeLightgbm}
\alias{TuneBoostTreeUltraConfig}
\alias{TuneBoostTreeBayesianUltra}
\title{Configuration Helpers for TuneBoostTreeBayesian}
\usage{
TuneBoostTreeBoostParams(trees = 500L, stop_iter = 20L, learn_rate = NULL,
  tree_depth = NULL, min_n = NULL, loss_reduction = NULL,
  sample_size = NULL, mtry = "default", max_bin = NULL)
TuneBoostTreeSearchSpace(learn_rate = c(0.01, 0.2), tree_depth = c(2L, 12L),
  min_n = c(1, 80), loss_reduction = c(0, 8), sample_size = c(0.55, 1),
  mtry = NULL, max_bin = NULL, lambda = NULL, alpha = NULL,
  max_delta_step = NULL, colsample_bytree = NULL, colsample_bylevel = NULL,
  num_leaves = NULL, min_data_in_leaf = NULL, scale_pos_weight = NULL)
TuneBoostTreeCv(folds = 10L, stratified = TRUE)
TuneBoostTreeOptimizerLimbo(command = NULL, fallback = TRUE, acquisition = c("ucb", "ei", "poi"), kappa = 2.576,
  eps = 0)
TuneBoostTreeLimbo(command = NULL, fallback = TRUE, acquisition = c("ucb", "ei", "poi"), kappa = 2.576,
  eps = 0)
TuneBoostTreeInternalOptimizer()
TuneBoostTreeOptimizerRBayesianOptimization(acquisition = c("ucb", "ei", "poi"), kappa = 2.576, eps = 0,
  fallback = TRUE)
TuneBoostTreeRBayesianOptimization(acquisition = c("ucb", "ei", "poi"), kappa = 2.576, eps = 0,
  fallback = TRUE)
TuneBoostTreeImbalance(balanceFn = NULL, scale_pos_weight = "auto", ...)
TuneBoostTreePerformance(metric = "pr_auc", backend = "auto")
TuneBoostTreeControl(seed = 42L, parallel = "auto", verbose = TRUE,
  fallback_trees = 100L)
TuneBoostTreeParallel(workers = "auto", threads_per_worker = "auto",
  strategy = c("auto", "folds", "engine", "sequential"))
TuneBoostTreeXgboost(eval_metric = "aucpr", tree_method = "hist", feature_types = NULL)
TuneBoostTreeLightgbm(metric = "average_precision")
TuneBoostTreeUltraConfig(command = NULL, strict_limbo = TRUE)
TuneBoostTreeBayesianUltra(formula, data, initial = 20L, nIter = 60L,
  engine = "lightgbm", command = NULL, strict_limbo = TRUE)
}
\arguments{
\item{trees, stop_iter, learn_rate, tree_depth, min_n, loss_reduction, sample_size, mtry}{Boost-tree controls using parsnip naming. Set a boost parameter to a scalar to fix it rather than tune it. For \code{mtry}, \code{"default"} fixes the predictor-sampling fraction at 0.8 (about 80 percent of features), a numeric value fixes another fraction in (0, 1], and \code{NULL} leaves it tunable only when present in \code{TuneBoostTreeSearchSpace()}.}
\item{max_bin}{Optional fixed histogram-bin value in \code{TuneBoostTreeBoostParams()} or optional bounds in \code{TuneBoostTreeSearchSpace()}. The default dedicated profile fixes it internally at 256 unless supplied explicitly.}
\item{lambda}{L2 regularization bounds. Recommended to tune for both engines; maps to \code{lambda} in XGBoost and \code{lambda_l2} in LightGBM.}
\item{alpha}{L1 regularization bounds. Recommended to tune when predictors may be redundant; maps to \code{alpha} in XGBoost and \code{lambda_l1} in LightGBM.}
\item{max_delta_step}{Optional XGBoost logistic-step stabilization bounds. Tune only for severe imbalance or unstable probabilities.}
\item{colsample_bytree}{Per-tree feature-sampling bounds. Recommended to tune when many predictors are available.}
\item{colsample_bylevel}{Optional XGBoost per-level feature-sampling bounds. Tune for additional regularization in wide data.}
\item{num_leaves}{Optional LightGBM leaf-count bounds. Recommended to tune when using LightGBM because it controls model complexity.}
\item{min_data_in_leaf}{Optional LightGBM minimum leaf-count bounds. Recommended to tune when using LightGBM to control overfitting.}
\item{scale_pos_weight}{\code{"auto"}, \code{NULL}, a positive numeric scalar, or optional search bounds in \code{TuneBoostTreeSearchSpace()}. By default, automatic imbalance handling computes negatives / positives for the selected positive class; tune it only when validation data is large enough.}
\item{folds, stratified}{Cross-validation configuration. If a requested fold count exceeds the minority-class count, the tuner warns and reduces folds so every validation fold contains both classes.}
\item{command, fallback, acquisition, kappa, eps}{Limbo or rBayesianOptimization optimizer configuration. \code{acquisition} is one of \code{"ucb"} (upper confidence bound, balances predicted score and uncertainty), \code{"ei"} (expected improvement), or \code{"poi"} (probability of improvement). \code{kappa} controls UCB exploration; \code{eps} controls EI/POI improvement margin.}
\item{balanceFn}{Optional function called as \code{balanceFn(data, formula, ...)} once per training fold. It must return a data.frame, tibble, or data.table containing the outcome and all predictor columns; extra arguments are supplied through \code{...}.}
\item{...}{Additional arguments forwarded only to \code{balanceFn}.}
\item{metric, backend}{Metric and PR-AUC backend configuration.}
\item{seed, parallel, verbose, fallback_trees}{Runtime controls.}
\item{workers, threads_per_worker, strategy}{Explicit parallel execution controls.}
\item{eval_metric, tree_method, feature_types}{XGBoost-specific controls.}
\item{formula, data, initial, nIter, engine}{Arguments forwarded to \code{TuneBoostTreeBayesianUltra()} for the ultra-optimized scenario. The target in \code{data} must be a factor with exactly two levels.}
\item{strict_limbo}{When \code{TRUE}, ultra configuration requires a working Limbo executable.}
}
\value{Each helper returns a validated list consumed by \code{TuneBoostTree()}. Tabular outputs returned by tuning and prediction helpers are tibbles.}
\description{Small configuration constructors document tuning scenarios while keeping the main tuning function compact and safe.}
\details{
The default dedicated HPC profile tunes only \code{learn_rate}, \code{tree_depth}, \code{min_n}, \code{loss_reduction}, and \code{sample_size}. Optional entries such as \code{mtry}, \code{max_bin}, \code{lambda}, \code{alpha}, \code{colsample_bytree}, engine-specific bounds, and \code{scale_pos_weight} remain available when explicitly supplied, but are no longer tuned by default to keep Bayesian search focused and fast on the target Xeon Platinum 8260 machine.
}
