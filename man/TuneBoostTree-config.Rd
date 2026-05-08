\name{TuneBoostTreeBoostParams}
\alias{TuneBoostTreeBoostParams}
\alias{TuneBoostTreeSearchSpace}
\alias{TuneBoostTreeCv}
\alias{TuneBoostTreeLimbo}
\alias{TuneBoostTreeInternalOptimizer}
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
  sample_size = NULL, mtry = NULL)
TuneBoostTreeSearchSpace(learn_rate = c(0.01, 0.15), tree_depth = c(2L, 10L),
  min_n = c(1, 50), loss_reduction = c(0, 5), sample_size = c(0.5, 1),
  mtry = c(0.3, 1), max_bin = c(32L, 256L))
TuneBoostTreeCv(folds = 10L, stratified = TRUE)
TuneBoostTreeLimbo(command = Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_),
  fallback = TRUE, acquisition = "ucb", kappa = 2.576, eps = 0)
TuneBoostTreeInternalOptimizer()
TuneBoostTreeImbalance(balance_fn = NULL, scale_pos_weight = "auto", ...)
TuneBoostTreePerformance(metric = "pr_auc", backend = "auto")
TuneBoostTreeControl(seed = 42L, parallel = "auto", verbose = TRUE,
  fallback_trees = 100L)
TuneBoostTreeParallel(workers = "auto", threads_per_worker = "auto",
  strategy = c("auto", "folds", "engine", "sequential"))
TuneBoostTreeXgboost(eval_metric = "aucpr", tree_method = "hist", feature_types = NULL)
TuneBoostTreeLightgbm(metric = "average_precision")
TuneBoostTreeUltraConfig(command = Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_),
  strict_limbo = TRUE)
TuneBoostTreeBayesianUltra(formula, data, initial = 20L, nIter = 60L,
  engine = "xgboost", command = Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_),
  strict_limbo = TRUE)
}
\arguments{
\item{trees, stop_iter, learn_rate, tree_depth, min_n, loss_reduction, sample_size, mtry}{Boost-tree controls using parsnip naming.}
\item{max_bin}{Native histogram-bin bounds for XGBoost/LightGBM.}
\item{folds, stratified}{Cross-validation configuration.}
\item{command, fallback, acquisition, kappa, eps}{Limbo ask/tell optimizer configuration.}
\item{balance_fn}{Optional function called as \code{balance_fn(data, formula, ...)} once per training fold.}
\item{scale_pos_weight}{\code{"auto"}, \code{NULL}, or a positive numeric scalar; auto is computed after balancing when a balancer is supplied.}
\item{...}{Additional arguments forwarded only to \code{balance_fn}.}
\item{metric, backend}{Metric and PR-AUC backend configuration.}
\item{seed, parallel, verbose, fallback_trees}{Runtime controls.}
\item{workers, threads_per_worker, strategy}{Explicit parallel execution controls.}
\item{eval_metric, tree_method, feature_types}{XGBoost-specific controls.}
\item{formula, data, initial, nIter, engine}{Arguments forwarded to \code{TuneBoostTreeBayesianUltra()} for the ultra-optimized scenario.}
\item{strict_limbo}{When \code{TRUE}, ultra configuration requires a working Limbo executable.}
}
\value{Each helper returns a validated list consumed by \code{TuneBoostTreeBayesian()}.}
\description{Small configuration constructors document tuning scenarios while keeping the main tuning function compact and safe.}
