#' Build Boosting Hyperparameter Defaults
#'
#' @param trees Integer maximum number of boosting iterations used inside tuning.
#' @param stop_iter Integer early-stopping patience, matching `parsnip::boost_tree()` naming.
#' @param learn_rate Optional fixed learning rate; `NULL` means tune it through `search_space`.
#' @param tree_depth Optional fixed maximum tree depth; `NULL` means tune it.
#' @param min_n Optional fixed minimum node size / child-weight proxy; `NULL` means tune it.
#' @param loss_reduction Optional fixed minimum loss reduction; `NULL` means tune it.
#' @param sample_size Optional fixed row-sampling fraction; `NULL` means tune it.
#' @param mtry Optional fixed predictor-sampling fraction in `(0, 1]`; `NULL` means tune it.
#'
#' @return A validated list of boost-tree defaults.
#' @export
TuneBoostTreeBoostParams <- function(trees = 500L, stop_iter = 20L, learn_rate = NULL, tree_depth = NULL, min_n = NULL, loss_reduction = NULL, sample_size = NULL, mtry = NULL) {
  out <- list(trees = as.integer(trees), stop_iter = as.integer(stop_iter), learn_rate = learn_rate, tree_depth = tree_depth, min_n = min_n, loss_reduction = loss_reduction, sample_size = sample_size, mtry = mtry)
  if (length(out$trees) != 1L || is.na(out$trees) || out$trees < 1L) cli::cli_abort("`trees` must be a positive integer.")
  if (length(out$stop_iter) != 1L || is.na(out$stop_iter) || out$stop_iter < 1L) cli::cli_abort("`stop_iter` must be a positive integer.")
  class(out) <- c("tbtb_boost_params", "list")
  out
}

#' Build Bayesian Search Space
#'
#' @param learn_rate Numeric lower/upper bounds for `boost_tree(learn_rate)`.
#' @param tree_depth Integer lower/upper bounds for `boost_tree(tree_depth)`.
#' @param min_n Numeric lower/upper bounds for `boost_tree(min_n)`.
#' @param loss_reduction Numeric lower/upper bounds for `boost_tree(loss_reduction)`.
#' @param sample_size Numeric lower/upper bounds for `boost_tree(sample_size)`.
#' @param mtry Numeric lower/upper bounds for predictor-sampling fraction.
#' @param max_bin Integer lower/upper bounds for histogram bins used by XGBoost/LightGBM.
#' @param lambda Numeric L2-regularization bounds. Tunable and recommended for most real data.
#' @param alpha Numeric L1-regularization bounds. Tunable and recommended when many predictors may be weak or redundant.
#' @param max_delta_step Numeric XGBoost logistic-step stabilization bounds; optional, most useful under severe imbalance.
#' @param colsample_bytree Numeric per-tree feature-sampling bounds in `(0, 1]`; tunable and recommended when many predictors exist.
#' @param colsample_bylevel Numeric per-level feature-sampling bounds in `(0, 1]`; optional XGBoost regularization.
#' @param num_leaves Integer LightGBM leaf-count bounds; tunable and recommended for LightGBM.
#' @param min_data_in_leaf Integer LightGBM minimum leaf-count bounds; tunable and recommended for LightGBM.
#' @param scale_pos_weight Optional positive-class weight bounds. Usually leave `NULL` so `TuneBoostTreeImbalance(scale_pos_weight = "auto")` computes negatives/positives; tune only when validation data is large enough.
#'
#' @return A named list of optimizer bounds.
#' @export
TuneBoostTreeSearchSpace <- function(learn_rate = c(0.01, 0.15), tree_depth = c(2L, 10L), min_n = c(1, 50), loss_reduction = c(0, 5), sample_size = c(0.5, 1), mtry = c(0.3, 1), max_bin = c(32L, 256L), lambda = c(0, 10), alpha = c(0, 5), max_delta_step = NULL, colsample_bytree = c(0.5, 1), colsample_bylevel = NULL, num_leaves = NULL, min_data_in_leaf = NULL, scale_pos_weight = NULL) {
  out <- list(learn_rate = learn_rate, tree_depth = tree_depth, min_n = min_n, sample_size = sample_size, mtry = mtry, loss_reduction = loss_reduction, max_bin = max_bin, lambda = lambda, alpha = alpha, max_delta_step = max_delta_step, colsample_bytree = colsample_bytree, colsample_bylevel = colsample_bylevel, num_leaves = num_leaves, min_data_in_leaf = min_data_in_leaf, scale_pos_weight = scale_pos_weight)
  out <- out[!vapply(out, is.null, logical(1L))]
  for (parameterName in names(out)) {
    value <- as.numeric(out[[parameterName]])
    if (length(value) != 2L || anyNA(value) || any(!is.finite(value)) || value[1L] >= value[2L]) cli::cli_abort("Search-space entry `{parameterName}` must contain finite increasing lower/upper bounds.")
    out[[parameterName]] <- value
  }
  for (fractionName in intersect(c("mtry", "sample_size", "colsample_bytree", "colsample_bylevel"), names(out))) {
    if (out[[fractionName]][1L] <= 0 || out[[fractionName]][2L] > 1) cli::cli_abort("`{fractionName}` search bounds must be fractions in `(0, 1]`.")
  }
  if ("scale_pos_weight" %in% names(out) && out$scale_pos_weight[1L] <= 0) cli::cli_abort("`scale_pos_weight` search bounds must be positive.")
  class(out) <- c("tbtb_search_space", "list")
  out
}

#' Build Cross-Validation Configuration
#'
#' @param folds Integer number of stratified folds.
#' @param stratified Logical; currently only stratified binary folds are supported.
#'
#' @return A validated cross-validation configuration list.
#' @export
TuneBoostTreeCv <- function(folds = 10L, stratified = TRUE) {
  out <- list(folds = as.integer(folds), stratified = isTRUE(stratified))
  if (length(out$folds) != 1L || is.na(out$folds) || out$folds < 2L) cli::cli_abort("`folds` must be an integer greater than or equal to 2.")
  if (!out$stratified) cli::cli_abort("Only stratified binary folds are currently supported; use `stratified = TRUE`.")
  class(out) <- c("tbtb_cv", "list")
  out
}

#' Build Limbo Optimizer Configuration
#'
#' @param command Path or PATH command for the Limbo ask/tell executable.
#' @param fallback Logical; when `TRUE`, use the internal safe optimizer if Limbo is not configured.
#' @param acquisition Acquisition label passed to the Limbo adapter metadata.
#' @param kappa Exploration parameter for UCB-style acquisition functions.
#' @param eps Improvement jitter for EI-style acquisition functions.
#'
#' @return A validated optimizer configuration list.
#' @export
TuneBoostTreeLimbo <- function(command = Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_), fallback = TRUE, acquisition = "ucb", kappa = 2.576, eps = 0) {
  out <- list(type = "limbo", command = command, fallback = isTRUE(fallback), acquisition = as.character(acquisition)[1L], kappa = as.numeric(kappa)[1L], eps = as.numeric(eps)[1L])
  if (!nzchar(out$acquisition) || is.na(out$acquisition)) cli::cli_abort("`acquisition` must be a non-empty string.")
  if (!is.finite(out$kappa) || !is.finite(out$eps)) cli::cli_abort("`kappa` and `eps` must be finite numerics.")
  class(out) <- c("tbtb_optimizer", "list")
  out
}

#' Build Internal Optimizer Configuration
#'
#' @return A dependency-free optimizer configuration list.
#' @export
TuneBoostTreeInternalOptimizer <- function() {
  out <- list(type = "internal", command = NA_character_, fallback = TRUE, acquisition = "internal", kappa = 0, eps = 0)
  class(out) <- c("tbtb_optimizer", "list")
  out
}

#' Build Class-Imbalance Configuration
#'
#' @param balance_fn Optional function called once per training fold as `balance_fn(data, formula, ...)`.
#' @param scale_pos_weight Either `"auto"`, `NULL`, or a positive numeric scalar.
#' @param ... Extra arguments forwarded only to `balance_fn`.
#'
#' @return A validated imbalance configuration list.
#' @export
TuneBoostTreeImbalance <- function(balance_fn = NULL, scale_pos_weight = "auto", ...) {
  if (!is.null(balance_fn) && !is.function(balance_fn)) cli::cli_abort("`balance_fn` must be a function or `NULL`.")
  if (is.character(scale_pos_weight)) {
    if (!identical(scale_pos_weight, "auto")) cli::cli_abort("`scale_pos_weight` must be `\"auto\"`, `NULL`, or a positive numeric scalar.")
  } else if (!is.null(scale_pos_weight)) {
    scale_pos_weight <- as.numeric(scale_pos_weight)[1L]
    if (!is.finite(scale_pos_weight) || scale_pos_weight <= 0) cli::cli_abort("Numeric `scale_pos_weight` must be positive and finite.")
  }
  out <- list(balance_fn = balance_fn, scale_pos_weight = scale_pos_weight, balance_args = list(...))
  class(out) <- c("tbtb_imbalance", "list")
  out
}

#' Build Performance Configuration
#'
#' @param metric Metric optimized during tuning; currently only `"pr_auc"` is supported.
#' @param backend PR-AUC implementation, one of `"auto"`, `"c"`, `"fortran"`, `"rfast"`, or `"r"`.
#'
#' @return A validated performance configuration list.
#' @export
TuneBoostTreePerformance <- function(metric = "pr_auc", backend = "auto") {
  metric <- match.arg(as.character(metric)[1L], c("pr_auc"))
  backend <- match.arg(as.character(backend)[1L], c("auto", "c", "fortran", "rfast", "r"))
  out <- list(metric = metric, backend = backend)
  class(out) <- c("tbtb_performance", "list")
  out
}

#' Build Runtime Control Configuration
#'
#' @param seed Integer random seed.
#' @param parallel Either `"auto"`, `FALSE`, or a list from `TuneBoostTreeParallel()`.
#' @param verbose Logical or integer controlling user-facing progress messages.
#' @param fallback_trees Integer final-training fallback when early stopping cannot infer rounds.
#'
#' @return A validated runtime control list.
#' @export
TuneBoostTreeControl <- function(seed = 42L, parallel = "auto", verbose = TRUE, fallback_trees = 100L) {
  out <- list(seed = as.integer(seed), parallel = parallel, verbose = verbose, fallback_trees = as.integer(fallback_trees))
  if (length(out$seed) != 1L || is.na(out$seed)) cli::cli_abort("`seed` must be a scalar integer.")
  if (length(out$fallback_trees) != 1L || is.na(out$fallback_trees) || out$fallback_trees < 1L) cli::cli_abort("`fallback_trees` must be a positive integer.")
  class(out) <- c("tbtb_control", "list")
  out
}

#' Build Explicit Parallel Configuration
#'
#' @param workers Number of fold workers, or `"auto"`.
#' @param threads_per_worker Number of engine threads per worker, or `"auto"`.
#' @param strategy One of `"auto"`, `"folds"`, `"engine"`, or `"sequential"`.
#'
#' @return A validated parallel configuration list.
#' @export
TuneBoostTreeParallel <- function(workers = "auto", threads_per_worker = "auto", strategy = c("auto", "folds", "engine", "sequential")) {
  out <- list(workers = workers, threads_per_worker = threads_per_worker, strategy = match.arg(strategy))
  class(out) <- c("tbtb_parallel", "list")
  out
}

#' Build XGBoost Engine Configuration
#'
#' @param eval_metric XGBoost evaluation metric, usually `"aucpr"` or `"auc"`.
#' @param tree_method XGBoost tree method; `"hist"` is the optimized default.
#' @param feature_types Optional XGBoost feature type vector.
#'
#' @return A validated engine configuration list.
#' @export
TuneBoostTreeXgboost <- function(eval_metric = "aucpr", tree_method = "hist", feature_types = NULL) {
  out <- list(name = "xgboost", eval_metric = as.character(eval_metric)[1L], tree_method = as.character(tree_method)[1L], feature_types = feature_types)
  if (!(out$eval_metric %in% c("aucpr", "auc"))) cli::cli_abort("`eval_metric` must be 'aucpr' or 'auc' for XGBoost.")
  class(out) <- c("tbtb_engine", "list")
  out
}

#' Build LightGBM Engine Configuration
#'
#' @param metric LightGBM metric; `"average_precision"` is the optimized PR-AUC default.
#'
#' @return A validated engine configuration list.
#' @export
TuneBoostTreeLightgbm <- function(metric = "average_precision") {
  out <- list(name = "lightgbm", metric = as.character(metric)[1L], feature_types = NULL)
  class(out) <- c("tbtb_engine", "list")
  out
}

#' Build Ultra-Optimized Configuration
#'
#' @param command Path or PATH command for the Limbo ask/tell executable.
#' @param strict_limbo Logical; when `TRUE`, Limbo must be configured and executable.
#'
#' @return A list of configuration blocks for high-performance tuning.
#' @export
TuneBoostTreeUltraConfig <- function(command = Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_), strict_limbo = TRUE) {
  list(boost = TuneBoostTreeBoostParams(trees = 1000L, stop_iter = 30L), search_space = TuneBoostTreeSearchSpace(learn_rate = c(0.005, 0.2), tree_depth = c(2L, 12L), min_n = c(1, 80), loss_reduction = c(0, 8), sample_size = c(0.55, 1), mtry = c(0.25, 1), max_bin = c(64L, 512L)), cv = TuneBoostTreeCv(folds = 10L), optimizer = TuneBoostTreeLimbo(command = command, fallback = !isTRUE(strict_limbo), acquisition = "ucb", kappa = 2.576, eps = 0), imbalance = TuneBoostTreeImbalance(scale_pos_weight = "auto"), performance = TuneBoostTreePerformance(metric = "pr_auc", backend = "auto"), control = TuneBoostTreeControl(parallel = "auto", verbose = TRUE))
}

#' Run Ultra-Optimized Bayesian Boosted-Tree Tuning
#'
#' @param formula A two-sided binary classification formula.
#' @param data A data.frame, tibble, or data.table containing training rows.
#' @param initial Integer initial design size or tabular warm-start grid.
#' @param nIter Integer number of optimizer iterations.
#' @param engine Either `"xgboost"`, `"lightgbm"`, or an engine configuration.
#' @param command Path or PATH command for the Limbo ask/tell executable.
#' @param strict_limbo Logical; when `TRUE`, Limbo must be configured.
#'
#' @return The same result object returned by `TuneBoostTreeBayesian()`.
#' @export
TuneBoostTreeBayesianUltra <- function(formula, data, initial = 20L, nIter = 60L, engine = "xgboost", command = Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_), strict_limbo = TRUE) {
  ultra <- TuneBoostTreeUltraConfig(command = command, strict_limbo = strict_limbo)
  TuneBoostTreeBayesian(formula = formula, data = data, initial = initial, nIter = nIter, engine = engine, boost = ultra$boost, search_space = ultra$search_space, cv = ultra$cv, optimizer = ultra$optimizer, imbalance = ultra$imbalance, performance = ultra$performance, control = ultra$control)
}

#' Tune Bayesian Gradient Boosted Tree Hyperparameters
#'
#' @param formula A two-sided formula with one binary outcome and numeric predictors.
#' @param data A data.frame, tibble, or data.table containing the training rows used during tuning.
#' @param initial Either `NULL`, an integer number of random initial points, or a tabular warm-start grid.
#' @param nIter Integer number of Bayesian optimization iterations after initialization.
#' @param engine Either `"xgboost"`, `"lightgbm"`, or an engine configuration from `TuneBoostTreeXgboost()`/`TuneBoostTreeLightgbm()`.
#' @param boost Boosting defaults from `TuneBoostTreeBoostParams()`.
#' @param search_space Bayesian search bounds from `TuneBoostTreeSearchSpace()`.
#' @param cv Cross-validation settings from `TuneBoostTreeCv()`.
#' @param optimizer Optimizer settings from `TuneBoostTreeLimbo()` or `TuneBoostTreeInternalOptimizer()`.
#' @param imbalance Class imbalance settings from `TuneBoostTreeImbalance()`.
#' @param performance Performance/scoring settings from `TuneBoostTreePerformance()`.
#' @param control Runtime controls from `TuneBoostTreeControl()`.
#'
#' @return A list with `bestHyperparameters`, `bestScore`, `initial`, `evaluationLog`, and `config`.
#' @export
TuneBoostTreeBayesian <- function(formula, data, initial = 10L, nIter = 30L, engine = "xgboost", boost = TuneBoostTreeBoostParams(), search_space = TuneBoostTreeSearchSpace(), cv = TuneBoostTreeCv(), optimizer = TuneBoostTreeLimbo(), imbalance = TuneBoostTreeImbalance(), performance = TuneBoostTreePerformance(), control = TuneBoostTreeControl()) {
  if (!inherits(formula, "formula") || length(formula) != 3L) cli::cli_abort("`formula` must be a two-sided formula.")
  if (!is.data.frame(data) || nrow(data) == 0L) cli::cli_abort("`data` must be a non-empty data.frame, tibble, or data.table.")
  data <- as.data.frame(data) # Normalizing once gives data.frame, tibble, and data.table callers stable downstream subsetting semantics.
  engine <- TuneBoostTree_ResolveEngine(engine)
  boost <- TuneBoostTree_ResolveBoost(boost)
  bounds <- TuneBoostTree_ResolveSearchSpace(search_space, boost)
  cv <- TuneBoostTree_ResolveCv(cv)
  optimizer <- TuneBoostTree_ResolveOptimizer(optimizer)
  imbalance <- TuneBoostTree_ResolveImbalance(imbalance)
  performance <- TuneBoostTree_ResolvePerformance(performance)
  control <- TuneBoostTree_ResolveControl(control)
  initialState <- TuneBoostTree_ResolveInitial(initial, bounds)
  initGridDt <- initialState$initGridDt
  initPoints <- initialState$initPoints
  nIter <- as.integer(nIter)
  if (length(nIter) != 1L || is.na(nIter) || nIter < 0L) cli::cli_abort("`nIter` must be a non-negative integer.")

  timerStart <- proc.time()[["elapsed"]]
  parameterNames <- names(bounds)
  nFolds <- cv$folds
  formulaInfoForTarget <- TuneBoostTree_ExtractFormulaInfo(formula, data)
  preparedTargetForCv <- TuneBoostTree_PrepareTarget(data[[formulaInfoForTarget$targetName]], NULL)
  nFolds <- TuneBoostTree_ValidateCvClassCounts(preparedTargetForCv$yData, nFolds)
  cv$folds <- nFolds
  nRoundsTuning <- boost$trees
  earlyStoppingRounds <- boost$stop_iter
  seed <- control$seed
  runtime <- TuneBoostTree_ResolveParallel(control$parallel, nrow(data), nFolds)
  nWorkersFolds <- runtime$workers
  workerThreads <- runtime$threads_per_worker
  prAucBackend <- TuneBoostTree_SelectPrAucBackend(performance$backend)
  engine_boost_tree <- engine$name
  evalMetric <- if (engine_boost_tree == "xgboost") engine$eval_metric else "average_precision"
  featureTypes <- engine$feature_types

  useBalancedCv <- !is.null(imbalance$balance_fn)
  if (useBalancedCv) {
    balancedFolds <- TuneBoostTree_PrepareBalancedFolds(formula, data, nFolds, imbalance$balance_fn, imbalance$balance_args, imbalance$scale_pos_weight, workerThreads, seed, engine_boost_tree, preparedTargetForCv$targetLevels)
    scalePosWeightValue <- NULL
  } else {
    formulaInfo <- formulaInfoForTarget
    preparedTrain <- TuneBoostTree_PrepareMatrix(formula, data, featureTypes, preparedTargetForCv$targetLevels, formulaInfo)
    classCounts <- table(preparedTrain$yData)
    if (length(classCounts) != 2L || any(classCounts == 0L)) cli::cli_abort("`data` must contain both binary classes.")
    scalePosWeightValue <- TuneBoostTree_ResolveScalePosWeight(preparedTrain$yData, imbalance$scale_pos_weight)
    folds <- TuneBoostTree_CreateStratifiedFolds(preparedTrain$yData, nFolds, seed)
    balancedFolds <- vector("list", length(folds))
    for (foldId in seq_along(folds)) {
      testIndex <- folds[[foldId]]
      trainIndex <- setdiff(seq_len(nrow(data)), testIndex)
      trainMatrix <- preparedTrain$xMatrix[trainIndex, , drop = FALSE]
      testMatrix <- preparedTrain$xMatrix[testIndex, , drop = FALSE]
      dtrain <- TuneBoostTree_CreateDataObject(trainMatrix, preparedTrain$yData[trainIndex], preparedTrain$featureTypes, workerThreads, engine_boost_tree)
      dtest <- TuneBoostTree_CreateDataObject(testMatrix, preparedTrain$yData[testIndex], preparedTrain$featureTypes, workerThreads, engine_boost_tree)
      foldScalePosWeight <- TuneBoostTree_ResolveScalePosWeight(preparedTrain$yData[trainIndex], imbalance$scale_pos_weight)
      if (engine_boost_tree == "xgboost") {
        balancedFolds[[foldId]] <- list(dtrain = dtrain, dtest = dtest, yTest = preparedTrain$yData[testIndex], scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels)
      } else {
        balancedFolds[[foldId]] <- list(dstrain = dtrain, dstest = dtest, xTest = as.matrix(testMatrix), yTest = preparedTrain$yData[testIndex], scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels)
      }
    }
  }

  cacheEnv <- new.env(parent = emptyenv())
  evaluationLogList <- vector("list", as.integer(initPoints) + as.integer(nIter) + 32L)
  logIndex <- 0L
  objective <- TuneBoostTree_EvaluateCv
  environment(objective) <- environment()

  if (isTRUE(control$verbose)) cli::cli_inform("Starting {.val {engine_boost_tree}} Bayesian tuning with {.val {nRoundsTuning}} trees, {.val {earlyStoppingRounds}} stop_iter, and {.val {nWorkersFolds}} fold worker(s).")
  set.seed(seed)
  tuningResult <- TuneBoostTree_RunOptimizer(objective = objective, bounds = bounds, initGridDt = initGridDt, initPoints = initPoints, nIter = nIter, acq = optimizer$acquisition, kappa = optimizer$kappa, eps = optimizer$eps, verbose = control$verbose, seed = seed, optimizerBackend = optimizer$type, limboCommand = optimizer$command, limboFallback = optimizer$fallback)

  evaluationLog <- if (logIndex > 0L) data.table::rbindlist(evaluationLogList[seq_len(logIndex)], fill = TRUE) else data.table::data.table()
  bestHyperparameters <- as.list(tuningResult$Best_Par)
  bestScore <- as.numeric(tuningResult$Best_Value)
  bestIteration <- TuneBoostTree_FindBestIteration(evaluationLog, bestHyperparameters, bestScore, bounds)
  if (is.null(bestIteration)) {
    bestSummary <- TuneBoostTree_RunCvManual(balancedFolds, bestHyperparameters, nRoundsTuning, earlyStoppingRounds, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend)
    bestIteration <- as.integer(bestSummary$bestIteration)
  }
  if (is.null(bestIteration) || is.na(bestIteration) || bestIteration < 1L) bestIteration <- as.integer(control$fallback_trees)
  bestHyperparameters$trees <- as.integer(bestIteration)
  bestHyperparameters$stop_iter <- as.integer(earlyStoppingRounds)
  bestHyperparameters$eval_metric <- evalMetric
  if (!useBalancedCv && is.null(bestHyperparameters$scale_pos_weight)) bestHyperparameters$scale_pos_weight <- scalePosWeightValue
  bestThresholdSummary <- TuneBoostTree_OptimizeThresholdCv(balancedFolds, bestHyperparameters, bestHyperparameters$trees, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend)
  bestHyperparameters$threshold <- as.numeric(bestThresholdSummary$threshold)

  newInitGridDt <- TuneBoostTree_CreateInitGrid(evaluationLog, bounds)
  returnedInitGridDt <- TuneBoostTree_CombineInitGrid(initGridDt, newInitGridDt, bounds)
  if (isTRUE(control$verbose)) cli::cli_inform("Finished Bayesian tuning in {.val {round(proc.time()[['elapsed']] - timerStart, 2)}} seconds.")

  list(bestHyperparameters = bestHyperparameters, bestScore = bestScore, bestThreshold = bestThresholdSummary, initial = returnedInitGridDt, evaluationLog = evaluationLog, config = list(engine = engine, boost = boost, search_space = bounds, cv = cv, optimizer = optimizer, imbalance = imbalance, performance = performance, control = control, parallel = runtime))
}

#' Resolve Engine Configuration
#' @noRd
TuneBoostTree_ResolveEngine <- function(engine) {
  if (is.character(engine)) {
    engineName <- match.arg(as.character(engine)[1L], c("xgboost", "lightgbm"))
    return(if (engineName == "xgboost") TuneBoostTreeXgboost() else TuneBoostTreeLightgbm())
  }
  if (!is.list(engine) || is.null(engine$name) || !(engine$name %in% c("xgboost", "lightgbm"))) cli::cli_abort("`engine` must be 'xgboost', 'lightgbm', or a TuneBoostTree engine configuration.")
  engine
}

#' Resolve Boost Configuration
#' @noRd
TuneBoostTree_ResolveBoost <- function(boost) {
  if (is.null(boost)) boost <- TuneBoostTreeBoostParams()
  if (!is.list(boost)) cli::cli_abort("`boost` must be created by `TuneBoostTreeBoostParams()` or be a compatible list.")
  defaults <- TuneBoostTreeBoostParams()
  defaults[names(boost)] <- boost
  TuneBoostTreeBoostParams(trees = defaults$trees, stop_iter = defaults$stop_iter, learn_rate = defaults$learn_rate, tree_depth = defaults$tree_depth, min_n = defaults$min_n, loss_reduction = defaults$loss_reduction, sample_size = defaults$sample_size, mtry = defaults$mtry)
}

#' Resolve Search Space
#' @noRd
TuneBoostTree_ResolveSearchSpace <- function(search_space, boost) {
  if (is.null(search_space)) search_space <- TuneBoostTreeSearchSpace()
  if (!is.list(search_space)) cli::cli_abort("`search_space` must be created by `TuneBoostTreeSearchSpace()` or be a compatible list.")
  defaults <- TuneBoostTreeSearchSpace()
  defaults[names(search_space)] <- search_space
  bounds <- do.call(TuneBoostTreeSearchSpace, defaults)
  fixedNames <- intersect(names(boost), names(bounds))[!vapply(boost[intersect(names(boost), names(bounds))], is.null, logical(1L))]
  for (parameterName in fixedNames) {
    value <- as.numeric(boost[[parameterName]])[1L]
    if (!is.finite(value)) cli::cli_abort("Fixed boost parameter `{parameterName}` must be finite.")
    bounds[[parameterName]] <- c(value, value)
  }
  bounds
}

#' Resolve Cross-Validation Configuration
#' @noRd
TuneBoostTree_ResolveCv <- function(cv) {
  if (is.null(cv)) cv <- TuneBoostTreeCv()
  if (!is.list(cv)) cli::cli_abort("`cv` must be created by `TuneBoostTreeCv()` or be a compatible list.")
  defaults <- TuneBoostTreeCv()
  defaults[names(cv)] <- cv
  TuneBoostTreeCv(folds = defaults$folds, stratified = defaults$stratified)
}

#' Resolve Optimizer Configuration
#' @noRd
TuneBoostTree_ResolveOptimizer <- function(optimizer) {
  if (is.null(optimizer)) optimizer <- TuneBoostTreeLimbo()
  if (is.character(optimizer)) optimizer <- if (identical(optimizer[1L], "internal")) TuneBoostTreeInternalOptimizer() else TuneBoostTreeLimbo()
  if (!is.list(optimizer) || is.null(optimizer$type)) cli::cli_abort("`optimizer` must be created by `TuneBoostTreeLimbo()` or `TuneBoostTreeInternalOptimizer()`.")
  if (!(optimizer$type %in% c("limbo", "internal", "rBayesianOptimization"))) cli::cli_abort("Unsupported optimizer type: {optimizer$type}")
  optimizer
}

#' Resolve Imbalance Configuration
#' @noRd
TuneBoostTree_ResolveImbalance <- function(imbalance) {
  if (is.null(imbalance)) imbalance <- TuneBoostTreeImbalance()
  if (!is.list(imbalance)) cli::cli_abort("`imbalance` must be created by `TuneBoostTreeImbalance()` or be a compatible list.")
  args <- if (is.null(imbalance$balance_args)) list() else imbalance$balance_args
  do.call(TuneBoostTreeImbalance, c(list(balance_fn = imbalance$balance_fn, scale_pos_weight = imbalance$scale_pos_weight), args))
}

#' Resolve Performance Configuration
#' @noRd
TuneBoostTree_ResolvePerformance <- function(performance) {
  if (is.null(performance)) performance <- TuneBoostTreePerformance()
  if (!is.list(performance)) cli::cli_abort("`performance` must be created by `TuneBoostTreePerformance()` or be a compatible list.")
  defaults <- TuneBoostTreePerformance()
  defaults[names(performance)] <- performance
  TuneBoostTreePerformance(metric = defaults$metric, backend = defaults$backend)
}

#' Resolve Runtime Control
#' @noRd
TuneBoostTree_ResolveControl <- function(control) {
  if (is.null(control)) control <- TuneBoostTreeControl()
  if (!is.list(control)) cli::cli_abort("`control` must be created by `TuneBoostTreeControl()` or be a compatible list.")
  defaults <- TuneBoostTreeControl()
  defaults[names(control)] <- control
  TuneBoostTreeControl(seed = defaults$seed, parallel = defaults$parallel, verbose = defaults$verbose, fallback_trees = defaults$fallback_trees)
}

#' Resolve Initial Optimizer State
#' @noRd
TuneBoostTree_ResolveInitial <- function(initial, bounds) {
  if (is.null(initial)) return(list(initGridDt = NULL, initPoints = 0L))
  if (is.list(initial) && !is.data.frame(initial) && !is.null(initial$initial)) initial <- initial$initial
  if (is.data.frame(initial)) return(list(initGridDt = TuneBoostTree_DeduplicateInitGrid(initial, bounds), initPoints = 0L))
  if (is.numeric(initial) && length(initial) == 1L && is.finite(initial) && initial >= 0) return(list(initGridDt = NULL, initPoints = as.integer(initial)))
  cli::cli_abort("`initial` must be `NULL`, a non-negative integer, or a data.frame/tibble/data.table warm-start grid.")
}

#' Resolve Parallel Runtime
#' @noRd
TuneBoostTree_ResolveParallel <- function(parallel, nRows, nFolds) {
  totalCores <- max(1L, parallel::detectCores(logical = TRUE))
  if (isFALSE(parallel) || identical(parallel, "sequential")) return(list(workers = 1L, threads_per_worker = totalCores))
  if (is.character(parallel) && identical(parallel[1L], "auto")) {
    workers <- if (nRows < 1000L) 1L else min(as.integer(nFolds), max(1L, floor(totalCores / 2L)))
    threads <- max(1L, floor(totalCores / workers))
    return(list(workers = as.integer(workers), threads_per_worker = as.integer(threads)))
  }
  if (is.list(parallel)) {
    strategy <- if (is.null(parallel$strategy)) "auto" else as.character(parallel$strategy)[1L]
    if (strategy == "sequential") return(list(workers = 1L, threads_per_worker = totalCores))
    if (strategy == "engine") return(list(workers = 1L, threads_per_worker = totalCores))
    workers <- if (identical(parallel$workers, "auto")) min(as.integer(nFolds), max(1L, floor(totalCores / 2L))) else as.integer(parallel$workers)
    if (length(workers) != 1L || is.na(workers) || workers < 1L) cli::cli_abort("Parallel `workers` must be positive or 'auto'.")
    threads <- if (identical(parallel$threads_per_worker, "auto")) max(1L, floor(totalCores / workers)) else as.integer(parallel$threads_per_worker)
    if (length(threads) != 1L || is.na(threads) || threads < 1L) cli::cli_abort("Parallel `threads_per_worker` must be positive or 'auto'.")
    return(list(workers = min(workers, as.integer(nFolds)), threads_per_worker = threads))
  }
  cli::cli_abort("`parallel` must be 'auto', FALSE, 'sequential', or `TuneBoostTreeParallel()`.")
}

#' Resolve Scale-Positive Weight
#' @noRd
TuneBoostTree_ResolveScalePosWeight <- function(yData, scale_pos_weight) {
  if (is.null(scale_pos_weight)) return(NULL)
  if (is.character(scale_pos_weight) && identical(scale_pos_weight, "auto")) {
    classCounts <- table(as.integer(yData))
    if (length(classCounts) != 2L || any(classCounts == 0L)) return(NULL)
    return(as.numeric(classCounts[["0"]] / classCounts[["1"]]))
  }
  as.numeric(scale_pos_weight)[1L]
}



#' Validate Cross-Validation Class Counts
#' @noRd
TuneBoostTree_ValidateCvClassCounts <- function(yData, nFolds) {
  classCounts <- table(as.integer(yData))
  if (length(classCounts) != 2L || any(classCounts == 0L)) cli::cli_abort("`data` must contain both binary classes.")
  minClassCount <- min(as.integer(classCounts))
  if (minClassCount < 2L) cli::cli_abort("The minority class must contain at least two observations for cross-validation.")
  if (minClassCount < as.integer(nFolds)) {
    cli::cli_warn("Minority class has {.val {minClassCount}} observation(s), fewer than requested {.val {nFolds}} fold(s); using {.val {minClassCount}} fold(s) so every validation fold contains both classes.")
    return(as.integer(minClassCount))
  }
  as.integer(nFolds)
}

#' Extract Formula Metadata
#'
#' @param formula A two-sided model formula.
#' @param data A data.frame containing all referenced columns.
#'
#' @details Internal helper that centralizes formula parsing so matrix preparation and prediction use identical feature ordering.
#'
#' @return A list with target name, predictor names, and the terms object.
#' @noRd
TuneBoostTree_ExtractFormulaInfo <- function(formula, data) {
  targetName <- all.vars(formula[[2L]])[1L] # The tuner supports one binary outcome, so a scalar target name drives all target handling.
  termsValue <- terms(formula, data = data) # Terms preserve the original predictor ordering for reproducible matrices.
  predictorNames <- attr(termsValue, "term.labels") # Direct predictor names are cheaper than constructing model matrices for numeric-only data.
  list(targetName = targetName, predictorNames = predictorNames, termsValue = termsValue) # Returning parsed metadata avoids repeated formula introspection.
}

#' Prepare a Binary Target
#'
#' @param targetData Outcome vector from the training data.
#' @param targetLevels Optional two-level class ordering where the second level is positive.
#'
#' @details Internal helper maps binary labels to 0/1 while preserving class labels for prediction output.
#'
#' @return A list with numeric target, levels, negative class, and positive class.
#' @noRd
TuneBoostTree_PrepareTarget <- function(targetData, targetLevels = NULL) {
  if (!is.factor(targetData)) cli::cli_abort("The dependent/target variable must be a factor with exactly two levels.")
  observedLevels <- levels(targetData)
  if (length(observedLevels) != 2L) cli::cli_abort("The dependent/target factor must have exactly two levels.")
  if (anyNA(targetData)) cli::cli_abort("The dependent/target factor must not contain missing values.")
  if (is.null(targetLevels)) {
    classCounts <- table(targetData)
    levelCounts <- as.integer(classCounts[observedLevels])
    if (any(levelCounts == 0L)) cli::cli_abort("The dependent/target factor must contain observations for both levels.")
    positiveClass <- if (levelCounts[1L] <= levelCounts[2L]) observedLevels[1L] else observedLevels[2L]
    negativeClass <- setdiff(observedLevels, positiveClass)[1L]
    targetLevels <- c(negativeClass, positiveClass)
  }
  targetLevels <- as.character(targetLevels)
  if (length(targetLevels) != 2L || anyNA(targetLevels) || !setequal(targetLevels, observedLevels)) cli::cli_abort("`targetLevels` must contain the two factor levels of the target.")
  positiveClass <- targetLevels[2L]
  negativeClass <- targetLevels[1L]
  yData <- as.integer(as.character(targetData) == positiveClass)
  list(yData = yData, targetLevels = targetLevels, negativeClass = negativeClass, positiveClass = positiveClass)
}

#' Prepare Numeric Feature Matrix
#'
#' @param formula A two-sided formula.
#' @param data A data.frame containing outcome and predictors.
#' @param featureTypes Optional feature type vector for XGBoost.
#' @param targetLevels Optional binary target level ordering.
#' @param formulaInfo Parsed formula metadata from `TuneBoostTree_ExtractFormulaInfo`.
#'
#' @details Converts numeric predictors to a double matrix and uses sparse storage only when the input is highly sparse.
#'
#' @return A list containing matrix, target, feature metadata, class metadata, and formula metadata.
#' @noRd
TuneBoostTree_PrepareMatrix <- function(formula, data, featureTypes = NULL, targetLevels = NULL, formulaInfo = NULL) {
  if (is.null(formulaInfo)) formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, data) # Allow public callers to skip pre-parsing while internal paths can reuse it.
  featureNames <- formulaInfo$predictorNames # A single feature vector guarantees consistent train/test column order.
  dataFrame <- as.data.frame(data) # Normalizing tibble/data.table inputs gives stable base subsetting semantics while preserving columns.
  xData <- dataFrame[, featureNames, drop = FALSE] # Restricting columns avoids accidental leakage from unused data fields.
  sparseLike <- vapply(xData, TuneBoostTree_IsSparseLikeColumn, logical(1L)) # sparsevctrs-style columns should end in sparse Matrix storage when possible.
  numericMatrix <- data.matrix(xData) # data.matrix is fast for already numeric data and preserves row order.
  storage.mode(numericMatrix) <- "double" # XGBoost and LightGBM expect numeric doubles for efficient native ingestion.
  colnames(numericMatrix) <- featureNames # Native boosters carry names forward for prediction-time alignment.
  xMatrix <- if (any(sparseLike) || mean(numericMatrix == 0) > 0.7) Matrix::Matrix(numericMatrix, sparse = TRUE) else numericMatrix # Sparse conversion is selected for sparsevctrs-like inputs or very sparse dense matrices.
  preparedTarget <- TuneBoostTree_PrepareTarget(dataFrame[[formulaInfo$targetName]], targetLevels) # Target preparation is centralized to keep class semantics identical.
  if (!is.null(featureTypes)) names(featureTypes) <- featureNames # Named feature types make accidental reordering easier to diagnose from model objects.
  list(xMatrix = xMatrix, yData = preparedTarget$yData, featureNames = featureNames, featureTypes = featureTypes, targetLevels = preparedTarget$targetLevels, targetName = formulaInfo$targetName, negativeClass = preparedTarget$negativeClass, positiveClass = preparedTarget$positiveClass, formulaInfo = formulaInfo) # This structure carries all metadata needed by fit, CV, and predict.
}

#' Detect Sparse-Like Columns
#' @noRd
TuneBoostTree_IsSparseLikeColumn <- function(column) {
  any(c("sparsevctrs_vctr", "sparse_vector", "sparse_double", "sparse_integer") %in% class(column))
}

#' Create Engine Data Object
#'
#' @param xMatrix Numeric matrix or sparse `dgCMatrix` of predictors.
#' @param yData Optional numeric target vector.
#' @param featureTypes Optional XGBoost feature type vector.
#' @param nThreads Integer engine threads for data construction.
#' @param engine_boost_tree Engine name, `"xgboost"` or `"lightgbm"`.
#'
#' @details This is the only internal function that constructs engine-native dataset objects.
#'
#' @return An `xgb.DMatrix` for XGBoost or `lgb.Dataset` for LightGBM.
#' @noRd
TuneBoostTree_CreateDataObject <- function(xMatrix, yData = NULL, featureTypes = NULL, nThreads = 1L, engine_boost_tree = "xgboost") {
  if (engine_boost_tree == "xgboost") {
    args <- list(data = xMatrix, nthread = as.integer(nThreads)) # XGBoost accepts nthread during DMatrix construction and benefits from bounded worker threads.
    if (!is.null(yData)) args$label <- yData # Labels are omitted for prediction matrices to avoid fake targets.
    if (!is.null(featureTypes)) args$feature_types <- unname(featureTypes) # Feature types are XGBoost-specific and therefore isolated to this branch.
    return(do.call(xgboost::xgb.DMatrix, args)) # do.call keeps optional arguments out of the call when absent.
  }
  lightgbm::lgb.Dataset(data = xMatrix, label = yData) # LightGBM Dataset has a smaller API, so only data and labels are supplied.
}

#' Create Stratified Folds
#'
#' @param yData Integer 0/1 target vector.
#' @param nFolds Integer number of folds.
#' @param seed Integer random seed.
#'
#' @details Internal fold splitter preserving the original alternating class allocation logic.
#'
#' @return A list of integer test-index vectors.
#' @noRd
TuneBoostTree_CreateStratifiedFolds <- function(yData, nFolds = 10L, seed = 42L) {
  set.seed(seed) # Fold allocation must be reproducible across tuning and outer-CV workflows.
  negativeIndex <- sample(which(as.integer(yData) == 0L)) # Shuffling within class preserves stratification without ordered-label artifacts.
  positiveIndex <- sample(which(as.integer(yData) == 1L)) # Independent class shuffles keep minority examples spread across folds.
  folds <- vector("list", as.integer(nFolds)) # Preallocating the fold list avoids growth in repeated outer workflows.
  for (foldId in seq_len(as.integer(nFolds))) {
    folds[[foldId]] <- c(negativeIndex[seq(foldId, length(negativeIndex), by = nFolds)], positiveIndex[seq(foldId, length(positiveIndex), by = nFolds)]) # Round-robin assignment keeps class proportions as even as counts allow.
  }
  folds # Returning only test indices keeps training complements cheap to derive.
}

#' Get Default Bayesian Bounds
#'
#' @details Internal helper returns the original seven-parameter search space shared by all engines.
#'
#' @return A named list of numeric bounds.
#' @noRd
TuneBoostTree_GetDefaultBounds <- function() {
  TuneBoostTreeSearchSpace() # Public search-space defaults use parsnip boost_tree names and include max_bin for native histogram engines.
}


#' Read Optional Hyperparameter
#' @noRd
TuneBoostTree_GetHyperparameter <- function(hyperparameters, parameterName, default = NULL) {
  if (!parameterName %in% names(hyperparameters) || is.null(hyperparameters[[parameterName]])) return(default)
  value <- as.numeric(hyperparameters[[parameterName]])[1L]
  if (!is.finite(value)) return(default)
  value
}

#' Build Engine Parameters
#'
#' @param hyperparameters Named list of canonical tuner hyperparameters.
#' @param nThreads Integer threads assigned to this model fit.
#' @param scalePosWeight Numeric positive-class weight.
#' @param seed Integer random seed.
#' @param evalMetric XGBoost evaluation metric name.
#' @param engine_boost_tree Engine name, `"xgboost"` or `"lightgbm"`.
#'
#' @details Translates canonical parameter names to engine-specific parameter lists.
#'
#' @return A named list ready for `xgb.train` or `lgb.train`.
#' @noRd
TuneBoostTree_BuildParams <- function(hyperparameters, nThreads = 1L, scalePosWeight = NULL, seed = 42L, evalMetric = "aucpr", engine_boost_tree = "xgboost") {
  learnRateValue <- as.numeric(hyperparameters[["learn_rate"]]) # Public learn_rate follows parsnip boost_tree naming.
  treeDepthValue <- as.integer(round(as.numeric(hyperparameters[["tree_depth"]]))) # Integer depth avoids engine-side coercion differences across packages.
  minNValue <- as.numeric(hyperparameters[["min_n"]]) # min_n is translated to the engine-specific minimum leaf/child-weight analogue.
  sampleSizeValue <- as.numeric(hyperparameters[["sample_size"]]) # Row sampling has equivalent tuning meaning across both engines.
  mtryValue <- as.numeric(hyperparameters[["mtry"]]) # The public API treats mtry as a safe predictor-sampling fraction in (0, 1].
  lossReductionValue <- as.numeric(hyperparameters[["loss_reduction"]]) # Split-gain regularization uses different names but same intent.
  maxBinValue <- as.integer(round(as.numeric(hyperparameters[["max_bin"]]))) # Histogram bin count must be integral for both engines.
  lambdaValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "lambda", NULL)
  alphaValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "alpha", NULL)
  maxDeltaStepValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "max_delta_step", NULL)
  colsampleBytreeValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "colsample_bytree", NULL)
  colsampleBylevelValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "colsample_bylevel", NULL)
  numLeavesValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "num_leaves", NULL)
  minDataInLeafValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "min_data_in_leaf", NULL)
  tunedScalePosWeight <- TuneBoostTree_GetHyperparameter(hyperparameters, "scale_pos_weight", NULL)
  scalePosWeight <- if (!is.null(tunedScalePosWeight)) tunedScalePosWeight else scalePosWeight
  scalePosWeight <- if (is.null(scalePosWeight)) NULL else as.numeric(scalePosWeight)[1L]
  if (engine_boost_tree == "xgboost") {
    params <- list(objective = "binary:logistic", eval_metric = evalMetric, grow_policy = "depthwise", tree_method = "hist", max_bin = maxBinValue, max_depth = treeDepthValue, eta = learnRateValue, gamma = lossReductionValue, subsample = sampleSizeValue, min_child_weight = minNValue, colsample_bynode = mtryValue, nthread = as.integer(nThreads), seed = as.integer(seed))
    if (!is.null(lambdaValue)) params$lambda <- as.numeric(lambdaValue)
    if (!is.null(alphaValue)) params$alpha <- as.numeric(alphaValue)
    if (!is.null(maxDeltaStepValue)) params$max_delta_step <- as.numeric(maxDeltaStepValue)
    if (!is.null(colsampleBytreeValue)) params$colsample_bytree <- as.numeric(colsampleBytreeValue)
    if (!is.null(colsampleBylevelValue)) params$colsample_bylevel <- as.numeric(colsampleBylevelValue)
    if (!is.null(scalePosWeight)) params$scale_pos_weight <- scalePosWeight
    return(params)
  }
  params <- list(objective = "binary", boosting = "gbdt", metric = "average_precision", max_bin = maxBinValue, max_depth = treeDepthValue, learning_rate = learnRateValue, min_gain_to_split = lossReductionValue, bagging_fraction = sampleSizeValue, bagging_freq = 1L, min_sum_hessian_in_leaf = minNValue, feature_fraction_bynode = mtryValue, num_threads = as.integer(nThreads), seed = as.integer(seed), verbosity = -1L, verbose = -1L)
  if (!is.null(lambdaValue)) params$lambda_l2 <- as.numeric(lambdaValue)
  if (!is.null(alphaValue)) params$lambda_l1 <- as.numeric(alphaValue)
  if (!is.null(colsampleBytreeValue)) params$feature_fraction <- as.numeric(colsampleBytreeValue)
  if (!is.null(numLeavesValue)) params$num_leaves <- as.integer(round(as.numeric(numLeavesValue)))
  if (!is.null(minDataInLeafValue)) params$min_data_in_leaf <- as.integer(round(as.numeric(minDataInLeafValue)))
  if (!is.null(scalePosWeight)) params$scale_pos_weight <- scalePosWeight
  params
}

#' Prepare Balanced Folds
#'
#' @param formula A two-sided formula.
#' @param data Full training data.frame.
#' @param nFolds Integer number of folds.
#' @param balanceFn Function applied once to each training partition.
#' @param balanceArgs Extra arguments forwarded only to `balanceFn`.
#' @param scalePosWeightSetting Numeric, `"auto"`, or `NULL` fold weight policy.
#' @param nThreads Integer threads for engine data construction.
#' @param seed Integer random seed.
#' @param engine_boost_tree Engine name, `"xgboost"` or `"lightgbm"`.
#'
#' @details Applies balancing once per fold and caches both train and test engine datasets for all objective evaluations.
#'
#' @return A list of fold objects containing cached engine datasets, labels, weights, and metadata.
#' @noRd
TuneBoostTree_PrepareBalancedFolds <- function(formula, data, nFolds, balanceFn, balanceArgs = list(), scalePosWeightSetting = "auto", nThreads = 1L, seed = 42L, engine_boost_tree = "xgboost", targetLevels = NULL) {
  formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, data) # Parsing once keeps all balanced folds on the same feature contract.
  preparedFull <- TuneBoostTree_PrepareMatrix(formula, data, NULL, targetLevels, formulaInfo) # Full data is needed only to create stratified fold indices.
  folds <- TuneBoostTree_CreateStratifiedFolds(preparedFull$yData, nFolds, seed) # Stratification is computed before balancing to evaluate on natural holdout distributions.
  balancedFolds <- vector("list", length(folds)) # Preallocation keeps fold setup linear in the number of folds.
  for (foldId in seq_along(folds)) {
    testIndex <- folds[[foldId]] # The test slice remains untouched by balancing for honest validation.
    trainData <- data[setdiff(seq_len(nrow(data)), testIndex), , drop = FALSE] # Only training rows are passed to the external balancing strategy.
    testData <- data[testIndex, , drop = FALSE] # Test rows are cached once to eliminate repeated DMatrix/Dataset creation.
    balancedTrain <- do.call(balanceFn, c(list(data = trainData, formula = formula), balanceArgs)) # Calling the balancer exactly once per fold avoids explosive work inside Bayesian iterations while forwarding function-specific arguments.
    preparedTrain <- TuneBoostTree_PrepareMatrix(formula, balancedTrain, NULL, preparedFull$targetLevels, formulaInfo) # Balanced data may be dense, so the sparse heuristic decides rather than forcing dgCMatrix.
    if (inherits(preparedTrain$xMatrix, "sparseMatrix")) preparedTrain$xMatrix <- as.matrix(preparedTrain$xMatrix) # ADASYN/NearMiss outputs are treated as dense to avoid sparse-conversion overhead after balancing.
    preparedTest <- TuneBoostTree_PrepareMatrix(formula, testData, NULL, preparedTrain$targetLevels, formulaInfo) # Test levels follow the balanced train levels to preserve class mapping.
    trainObject <- TuneBoostTree_CreateDataObject(preparedTrain$xMatrix, preparedTrain$yData, preparedTrain$featureTypes, nThreads, engine_boost_tree) # Cached train object amortizes engine data conversion across all hyperparameter evaluations.
    testObject <- TuneBoostTree_CreateDataObject(preparedTest$xMatrix, preparedTest$yData, preparedTest$featureTypes, nThreads, engine_boost_tree) # Cached test object satisfies the performance requirement for manual CV.
    foldScalePosWeight <- TuneBoostTree_ResolveScalePosWeight(preparedTrain$yData, scalePosWeightSetting) # Per-fold weights reflect the actual post-balancing distribution when auto weighting is requested.
    if (engine_boost_tree == "xgboost") {
      balancedFolds[[foldId]] <- list(dtrain = trainObject, dtest = testObject, yTest = preparedTest$yData, scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels) # XGBoost fold objects expose DMatrix entries only.
    } else {
      balancedFolds[[foldId]] <- list(dstrain = trainObject, dstest = testObject, xTest = as.matrix(preparedTest$xMatrix), yTest = preparedTest$yData, scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels) # LightGBM fold objects cache Dataset entries for training and matrices for documented prediction.
    }
  }
  balancedFolds # Returning self-contained fold objects enables safe parallel dispatch.
}

#' Run Manual Cross-Validation
#'
#' @param balancedFolds List of cached fold objects.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Integer tuning round ceiling.
#' @param earlyStoppingRounds Integer early-stopping patience.
#' @param seed Integer random seed.
#' @param nThreads Integer threads per worker.
#' @param nWorkersFolds Integer number of fold workers.
#' @param evalMetric XGBoost metric name.
#' @param engine_boost_tree Engine name, `"xgboost"` or `"lightgbm"`.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Runs cached folds sequentially or with base `parallel` while capping engine threads to avoid CPU oversubscription.
#'
#' @return A list with mean score, mean best iteration, and per-fold scores.
#' @noRd
TuneBoostTree_RunCvManual <- function(balancedFolds, hyperparameters, nRounds, earlyStoppingRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend = "auto") {
  totalCores <- max(1L, parallel::detectCores(logical = TRUE)) # Detecting cores here protects direct internal reuse with different worker counts.
  nWorkers <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds)) # More workers than folds adds overhead without additional parallelism.
  workerThreads <- max(1L, floor(totalCores / nWorkers)) # Thread division enforces nthread * workers <= total cores.
  workerThreads <- min(as.integer(nThreads), workerThreads) # Caller thread limits remain authoritative.
  foldIds <- seq_along(balancedFolds) # Explicit ids keep results ordered after parallel execution.
  if (nWorkers == 1L) {
    foldResults <- vector("list", length(foldIds)) # Sequential preallocation avoids parallel overhead for the default path.
    for (i in foldIds) foldResults[[i]] <- TuneBoostTree_RunOneFold(balancedFolds[[i]], hyperparameters, nRounds, earlyStoppingRounds, seed + i, workerThreads, evalMetric, engine_boost_tree, prAucBackend) # A plain loop is fastest when only one worker is requested.
  } else if (.Platform$OS.type == "windows") {
    cluster <- parallel::makeCluster(nWorkers) # Windows lacks fork support, so PSOCK workers are required.
    on.exit(parallel::stopCluster(cluster), add = TRUE) # Cluster cleanup prevents orphan R worker processes.
    foldResults <- parallel::parLapply(cluster, foldIds, TuneBoostTree_RunFoldById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, earlyStoppingRounds = earlyStoppingRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree, prAucBackend = prAucBackend) # parLapply ships self-contained fold data to each worker.
  } else {
    foldResults <- parallel::mclapply(foldIds, TuneBoostTree_RunFoldById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, earlyStoppingRounds = earlyStoppingRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree, prAucBackend = prAucBackend, mc.cores = nWorkers) # Forked workers minimize serialization overhead on Unix-like systems.
  }
  foldScores <- vapply(foldResults, `[[`, numeric(1L), "score") # Numeric extraction keeps aggregation independent of backend result classes.
  foldBestIter <- vapply(foldResults, `[[`, integer(1L), "bestIteration") # Best iterations are averaged to provide a stable final round count.
  if (anyNA(foldScores)) cli::cli_abort("At least one validation fold produced undefined PR-AUC; reduce `folds` or provide more positive-class observations.")
  list(score = as.numeric(mean(foldScores)), bestIteration = as.integer(round(mean(foldBestIter))), foldScores = foldScores) # Mean PR-AUC and mean early-stop iteration summarize one hyperparameter evaluation.
}

#' Evaluate One Bayesian Parameter Set
#'
#' @param learn_rate Learning-rate candidate.
#' @param tree_depth Depth candidate.
#' @param min_n Minimum node-size candidate.
#' @param sample_size Row-sampling candidate.
#' @param mtry Predictor-sampling fraction candidate.
#' @param loss_reduction Split-gain candidate.
#' @param max_bin Histogram-bin candidate.
#'
#' @details Top-level objective used by the optimizer; its environment is rebound by `TuneBoostTreeBayesian` to call-local state.
#'
#' @return A list with `Score` and `Pred` for optimizer adapters.
#' @noRd
TuneBoostTree_EvaluateCv <- function(...) {
  hyperparameters <- list(...)
  hyperparameters <- hyperparameters[parameterNames]
  normalizedData <- TuneBoostTree_NormalizeParams(as.data.frame(hyperparameters, stringsAsFactors = FALSE), parameterNames) # Normalization prevents cache misses from equivalent rounded integer parameters.
  hyperparameters <- as.list(normalizedData[1L, parameterNames, drop = FALSE])
  cacheKey <- paste(unlist(normalizedData[1L, parameterNames, drop = FALSE], use.names = FALSE), collapse = "|") # The required pipe-joined key is compact and deterministic.
  if (exists(cacheKey, envir = cacheEnv, inherits = FALSE)) {
    cachedResult <- get(cacheKey, envir = cacheEnv, inherits = FALSE) # Hash lookup avoids duplicate CV runs proposed by the optimizer.
    return(list(Score = as.numeric(cachedResult$score), Pred = 0)) # The optimizer requires a fixed return shape even for cached values.
  }
  cvSummary <- TuneBoostTree_RunCvManual(balancedFolds, hyperparameters, nRoundsTuning, earlyStoppingRounds, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend) # Manual CV uses cached data objects and fold-level parallelism.
  scoreValue <- as.numeric(cvSummary$score) # A scalar score is needed by optimizer adapters.
  bestIteration <- as.integer(cvSummary$bestIteration) # Logging best iteration lets the final model avoid retuning rounds.
  logIndex <<- logIndex + 1L # Updating the call-local cursor avoids growing a data.frame inside the hot path.
  evaluationLogList[[logIndex]] <<- data.frame(normalizedData[1L, parameterNames, drop = FALSE], Value = scoreValue, bestIteration = bestIteration, stringsAsFactors = FALSE) # List-backed logging preserves the original high-performance pattern.
  assign(cacheKey, list(score = scoreValue, bestIteration = bestIteration), envir = cacheEnv) # The per-call cache prevents repeated expensive CV evaluations.
  list(Score = scoreValue, Pred = 0) # Optimizer adapters expect this stable list contract.
}

#' Run One Fold by Identifier
#'
#' @param foldId Integer fold id.
#' @param balancedFolds List of cached folds.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Integer tuning round ceiling.
#' @param earlyStoppingRounds Integer early-stopping patience.
#' @param seed Integer random seed.
#' @param nThreads Integer threads for this fold worker.
#' @param evalMetric XGBoost metric name.
#' @param engine_boost_tree Engine name.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Small top-level adapter keeps parallel workers self-contained and avoids closures over fold state.
#'
#' @return A fold result list.
#' @noRd
TuneBoostTree_RunFoldById <- function(foldId, balancedFolds, hyperparameters, nRounds, earlyStoppingRounds, seed, nThreads, evalMetric, engine_boost_tree, prAucBackend = "auto") {
  TuneBoostTree_RunOneFold(balancedFolds[[foldId]], hyperparameters, nRounds, earlyStoppingRounds, seed + foldId, nThreads, evalMetric, engine_boost_tree, prAucBackend) # Delegating by id keeps result order deterministic across parallel backends.
}

#' Run One Cached Fold
#'
#' @param foldData Cached fold object.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Integer tuning round ceiling.
#' @param earlyStoppingRounds Integer early-stopping patience.
#' @param seed Integer random seed.
#' @param nThreads Integer threads for this fold worker.
#' @param evalMetric XGBoost metric name.
#' @param engine_boost_tree Engine name.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Engine-specific training and prediction are isolated here for manual CV.
#'
#' @return A list with fold score and best iteration.
#' @noRd
TuneBoostTree_RunOneFold <- function(foldData, hyperparameters, nRounds, earlyStoppingRounds, seed, nThreads, evalMetric, engine_boost_tree, prAucBackend = "auto") {
  paramsValue <- TuneBoostTree_BuildParams(hyperparameters, nThreads, foldData$scalePosWeight, seed, evalMetric, engine_boost_tree) # Building params inside the worker applies the oversubscription-safe thread count.
  if (engine_boost_tree == "xgboost") {
    foldModel <- xgboost::xgb.train(params = paramsValue, data = foldData$dtrain, nrounds = as.integer(nRounds), watchlist = list(train = foldData$dtrain, eval = foldData$dtest), early_stopping_rounds = as.integer(earlyStoppingRounds), maximize = TRUE, verbose = 0L) # XGBoost early stopping reduces effective tuning rounds aggressively.
    bestIterFold <- as.integer(foldModel$best_iteration) # The native booster records the selected iteration when early stopping fires.
    if (is.null(bestIterFold) || is.na(bestIterFold) || bestIterFold < 1L) bestIterFold <- as.integer(nRounds) # No early stop means the full tuning ceiling was used.
    predictedProbability <- as.numeric(stats::predict(foldModel, newdata = foldData$dtest)) # Prediction uses the cached DMatrix to avoid data reconstruction.
  } else {
    foldModel <- lightgbm::lgb.train(params = paramsValue, data = foldData$dstrain, nrounds = as.integer(nRounds), valids = list(eval = foldData$dstest), early_stopping_rounds = as.integer(earlyStoppingRounds), verbose = -1L) # LightGBM receives Dataset objects cached during fold preparation.
    bestIterFold <- as.integer(foldModel$best_iter) # LightGBM stores early-stopped rounds on best_iter in current boosters.
    if (is.null(bestIterFold) || is.na(bestIterFold) || bestIterFold < 1L) bestIterFold <- as.integer(nRounds) # Full rounds are used when LightGBM does not expose best_iter.
    predictedProbability <- as.numeric(stats::predict(foldModel, data = foldData$xTest)) # Matrix prediction follows LightGBM booster prediction semantics while Dataset creation remains cached for training.
  }
  list(score = TuneBoostTree_CalculatePrAuc(foldData$yTest, predictedProbability, backend = prAucBackend), bestIteration = bestIterFold) # PR-AUC is computed consistently across engines for objective comparability.
}


#' Run Hyperparameter Optimizer
#' @noRd
TuneBoostTree_RunOptimizer <- function(objective, bounds, initGridDt = NULL, initPoints = 10L, nIter = 30L, acq = "ucb", kappa = 2.576, eps = 0, verbose = TRUE, seed = 42L, optimizerBackend = "internal", limboCommand = NA_character_, limboFallback = TRUE) {
  if (identical(optimizerBackend, "limbo")) {
    if (!is.na(limboCommand) && nzchar(limboCommand) && nzchar(Sys.which(limboCommand))) {
      cli::cli_warn("Limbo command integration is not available in this R-only build; using the internal optimizer adapter.")
      return(TuneBoostTree_RunInternalOptimizer(objective, bounds, initGridDt, initPoints, nIter, seed))
    }
    if (!isTRUE(limboFallback)) cli::cli_abort("Limbo optimizer command is not available and `fallback = FALSE`.")
    cli::cli_warn("Limbo optimizer command is not available; using the internal optimizer fallback.")
    return(TuneBoostTree_RunInternalOptimizer(objective, bounds, initGridDt, initPoints, nIter, seed))
  }
  if (identical(optimizerBackend, "rBayesianOptimization") && requireNamespace("rBayesianOptimization", quietly = TRUE)) {
    return(TuneBoostTree_RunRBayesianOptimization(objective, bounds, initGridDt, initPoints, nIter, acq, kappa, eps, verbose, seed))
  }
  if (identical(optimizerBackend, "rBayesianOptimization")) cli::cli_warn("Package {.pkg rBayesianOptimization} is not available; using the internal optimizer fallback.")
  TuneBoostTree_RunInternalOptimizer(objective, bounds, initGridDt, initPoints, nIter, seed)
}

#' Run Internal Safe Optimizer
#' @noRd
TuneBoostTree_RunInternalOptimizer <- function(objective, bounds, initGridDt = NULL, initPoints = 10L, nIter = 30L, seed = 42L) {
  set.seed(as.integer(seed))
  parameterNames <- names(bounds)
  candidates <- TuneBoostTree_SampleCandidates(bounds, as.integer(initPoints) + as.integer(nIter))
  if (!is.null(initGridDt) && nrow(initGridDt) > 0L) {
    warmStart <- as.data.frame(initGridDt[, parameterNames, drop = FALSE], stringsAsFactors = FALSE)
    candidates <- rbind(warmStart, candidates)
  }
  if (nrow(candidates) == 0L) cli::cli_abort("Optimizer received no candidate hyperparameters to evaluate.")
  bestValue <- -Inf
  bestPar <- NULL
  for (rowId in seq_len(nrow(candidates))) {
    candidate <- as.list(candidates[rowId, parameterNames, drop = FALSE])
    result <- do.call(objective, candidate)
    value <- as.numeric(result$Score)[1L]
    if (is.finite(value) && value > bestValue) {
      bestValue <- value
      bestPar <- candidate
    }
  }
  if (is.null(bestPar)) cli::cli_abort("All optimizer candidate evaluations failed or returned non-finite scores.")
  list(Best_Par = bestPar, Best_Value = bestValue, History = candidates)
}

#' Run rBayesianOptimization Backend
#' @noRd
TuneBoostTree_RunRBayesianOptimization <- function(objective, bounds, initGridDt = NULL, initPoints = 10L, nIter = 30L, acq = "ucb", kappa = 2.576, eps = 0, verbose = TRUE, seed = 42L) {
  set.seed(as.integer(seed))
  normalizedBounds <- lapply(bounds, function(x) c(as.numeric(x[1L]), as.numeric(x[2L])))
  result <- rBayesianOptimization::BayesianOptimization(FUN = objective, bounds = normalizedBounds, init_grid_dt = initGridDt, init_points = as.integer(initPoints), n_iter = as.integer(nIter), acq = acq, kappa = kappa, eps = eps, verbose = isTRUE(verbose))
  list(Best_Par = as.list(result$Best_Par), Best_Value = as.numeric(result$Best_Value), History = result$History)
}

#' Sample Internal Optimizer Candidates
#' @noRd
TuneBoostTree_SampleCandidates <- function(bounds, n) {
  parameterNames <- names(bounds)
  n <- as.integer(n)
  out <- as.data.frame(setNames(rep(list(numeric(n)), length(parameterNames)), parameterNames))
  if (n <= 0L) return(out)
  for (parameterName in parameterNames) {
    lower <- as.numeric(bounds[[parameterName]][1L])
    upper <- as.numeric(bounds[[parameterName]][2L])
    out[[parameterName]] <- stats::runif(n, lower, upper)
  }
  TuneBoostTree_NormalizeParams(out, parameterNames)
}

#' Optimize Decision Threshold From Cross-Validated Predictions
#' @noRd
TuneBoostTree_OptimizeThresholdCv <- function(balancedFolds, hyperparameters, nRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend = "auto") {
  predictionSummary <- TuneBoostTree_RunCvPredictions(balancedFolds, hyperparameters, nRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree)
  TuneBoostTree_OptimizeThreshold(predictionSummary$actual, predictionSummary$predicted)
}

#' Run CV Predictions Without Early Stopping
#' @noRd
TuneBoostTree_RunCvPredictions <- function(balancedFolds, hyperparameters, nRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree) {
  totalCores <- max(1L, parallel::detectCores(logical = TRUE))
  nWorkers <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds))
  workerThreads <- min(as.integer(nThreads), max(1L, floor(totalCores / nWorkers)))
  foldIds <- seq_along(balancedFolds)
  if (nWorkers == 1L) {
    foldResults <- lapply(foldIds, function(i) TuneBoostTree_RunOneFoldPrediction(balancedFolds[[i]], hyperparameters, nRounds, seed + i, workerThreads, evalMetric, engine_boost_tree))
  } else if (.Platform$OS.type == "windows") {
    cluster <- parallel::makeCluster(nWorkers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    foldResults <- parallel::parLapply(cluster, foldIds, TuneBoostTree_RunFoldPredictionById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree)
  } else {
    foldResults <- parallel::mclapply(foldIds, TuneBoostTree_RunFoldPredictionById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree, mc.cores = nWorkers)
  }
  list(actual = unlist(lapply(foldResults, `[[`, "actual"), use.names = FALSE), predicted = unlist(lapply(foldResults, `[[`, "predicted"), use.names = FALSE))
}

#' Run One Prediction Fold by Identifier
#' @noRd
TuneBoostTree_RunFoldPredictionById <- function(foldId, balancedFolds, hyperparameters, nRounds, seed, nThreads, evalMetric, engine_boost_tree) {
  TuneBoostTree_RunOneFoldPrediction(balancedFolds[[foldId]], hyperparameters, nRounds, seed + foldId, nThreads, evalMetric, engine_boost_tree)
}

#' Run One Prediction Fold
#' @noRd
TuneBoostTree_RunOneFoldPrediction <- function(foldData, hyperparameters, nRounds, seed, nThreads, evalMetric, engine_boost_tree) {
  paramsValue <- TuneBoostTree_BuildParams(hyperparameters, nThreads, foldData$scalePosWeight, seed, evalMetric, engine_boost_tree)
  if (engine_boost_tree == "xgboost") {
    foldModel <- xgboost::xgb.train(params = paramsValue, data = foldData$dtrain, nrounds = as.integer(nRounds), verbose = 0L)
    predictedProbability <- as.numeric(stats::predict(foldModel, newdata = foldData$dtest))
  } else {
    foldModel <- lightgbm::lgb.train(params = paramsValue, data = foldData$dstrain, nrounds = as.integer(nRounds), verbose = -1L)
    predictedProbability <- as.numeric(stats::predict(foldModel, data = foldData$xTest))
  }
  list(actual = foldData$yTest, predicted = predictedProbability)
}

#' Optimize Binary Classification Threshold
#' @noRd
TuneBoostTree_OptimizeThreshold <- function(actual, predicted) {
  actual <- as.integer(actual)
  predicted <- as.numeric(predicted)
  valid <- is.finite(predicted) & !is.na(actual)
  actual <- actual[valid]
  predicted <- predicted[valid]
  if (length(actual) == 0L || length(unique(actual)) < 2L) return(list(threshold = 0.5, metric = "f1", score = NA_real_))
  thresholds <- sort(unique(predicted))
  candidates <- unique(pmin(pmax(c(0.5, thresholds), .Machine$double.eps), 1 - .Machine$double.eps))
  scores <- vapply(candidates, function(threshold) TuneBoostTree_F1Score(actual, predicted >= threshold), numeric(1L))
  bestIndex <- which.max(scores)
  list(threshold = as.numeric(candidates[bestIndex]), metric = "f1", score = as.numeric(scores[bestIndex]))
}

#' Calculate F1 Score
#' @noRd
TuneBoostTree_F1Score <- function(actual, predictedClass) {
  tp <- sum(actual == 1L & predictedClass)
  fp <- sum(actual == 0L & predictedClass)
  fn <- sum(actual == 1L & !predictedClass)
  precision <- if ((tp + fp) == 0L) 0 else tp / (tp + fp)
  recall <- if ((tp + fn) == 0L) 0 else tp / (tp + fn)
  if ((precision + recall) == 0) return(0)
  2 * precision * recall / (precision + recall)
}

#' Calculate PR AUC
#'
#' @param actual Integer 0/1 labels.
#' @param predicted Numeric positive-class probabilities.
#'
#' @details Internal trapezoidal precision-recall AUC matching the current implementation.
#'
#' @return Numeric PR-AUC value.
#' @noRd
TuneBoostTree_CalculatePrAuc <- function(actual, predicted, backend = "auto") {
  backend <- TuneBoostTree_SelectPrAucBackend(backend) # Backend selection is centralized so explicit C/Fortran requests fail safely only when unavailable.
  actual <- as.integer(actual) # Native and Rfast paths require compact integer labels.
  predicted <- as.numeric(predicted) # Sorting and native calls operate on doubles.
  if (length(actual) != length(predicted) || length(actual) == 0L) return(NA_real_) # Shape mismatches indicate an invalid scorer input and should never reach engines.
  if (anyNA(actual) || anyNA(predicted) || any(!is.finite(predicted))) return(NA_real_) # Non-finite probabilities would make ranking undefined.
  positiveCount <- sum(actual == 1L) # Recall denominator must count true positives in the validation target.
  if (positiveCount == 0L) return(NA_real_) # PR-AUC is undefined without positive examples.
  if (identical(backend, "c")) return(TuneBoostTree_CalculatePrAucC(actual, predicted)) # The C backend is the fastest exact scorer when the shared object is loaded.
  if (identical(backend, "fortran")) return(TuneBoostTree_CalculatePrAucFortran(actual, predicted)) # The Fortran backend provides an alternative compiled implementation for HPC stacks.
  if (identical(backend, "rfast")) return(TuneBoostTree_CalculatePrAucRfast(actual, predicted, positiveCount)) # Rfast accelerates ranking when native project code has not been compiled.
  TuneBoostTree_CalculatePrAucR(actual, predicted, positiveCount) # The base-R fallback keeps the script portable on machines without compiled helpers.
}


#' Select PR-AUC Backend
#'
#' @param backend Requested backend name.
#'
#' @details `auto` prefers compiled C, then compiled Fortran, then Rfast, and finally base R. Explicit unavailable compiled or Rfast requests fall back to base R rather than aborting a long tuning job.
#'
#' @return A resolved backend name.
#' @noRd
TuneBoostTree_SelectPrAucBackend <- function(backend = "auto") {
  backend <- match.arg(as.character(backend)[1L], c("auto", "c", "fortran", "rfast", "r")) # match.arg provides a clear validation error for unsupported scorer names.
  if (identical(backend, "auto")) {
    if (TuneBoostTree_LoadNativeBackend("c")) return("c") # The C implementation avoids R allocation in the hottest scorer path.
    if (TuneBoostTree_LoadNativeBackend("fortran")) return("fortran") # Fortran is useful on systems where BLAS/HPC toolchains are preferred.
    if (requireNamespace("Rfast", quietly = TRUE)) return("rfast") # Rfast is already supported and is the best no-project-compile fallback.
    return("r") # Base R guarantees portability.
  }
  if (identical(backend, "c") && !TuneBoostTree_LoadNativeBackend("c")) return("r") # Explicit C stays safe on machines where the shared object has not been built.
  if (identical(backend, "fortran") && !TuneBoostTree_LoadNativeBackend("fortran")) return("r") # Explicit Fortran also degrades safely instead of stopping optimization.
  if (identical(backend, "rfast") && !requireNamespace("Rfast", quietly = TRUE)) return("r") # Missing optional packages should not break a tuning run.
  backend # Available explicit backends are returned unchanged.
}

#' Load Optional Native Backend
#'
#' @param backend Either `"c"` or `"fortran"`.
#'
#' @details Checks whether the installed package DLL containing the registered C and Fortran routines is loaded.
#'
#' @return Logical indicating whether the symbol is available.
#' @noRd
TuneBoostTree_LoadNativeBackend <- function(backend) {
  packageDll <- "TuneBoostTreeBayesian" # Installed packages load one shared object with this DLL stem.
  packageDll %in% names(getLoadedDLLs()) # Package installation compiles and loads C and Fortran helpers together; otherwise callers safely fall back.
}

#' Calculate PR-AUC with Compiled C
#'
#' @noRd
TuneBoostTree_CalculatePrAucC <- function(actual, predicted) {
  as.numeric(.Call("tbtb_pr_auc_c", actual, predicted, PACKAGE = "TuneBoostTreeBayesian")) # .Call returns a scalar REALSXP from the registered package-native C implementation.
}

#' Calculate PR-AUC with Compiled Fortran
#' @noRd
TuneBoostTree_CalculatePrAucFortran <- function(actual, predicted) {
  out <- .Fortran("tbtb_pr_auc_f", n = as.integer(length(actual)), actual = as.integer(actual), predicted = as.double(predicted), score = as.double(NA_real_), PACKAGE = "TuneBoostTreeBayesian") # .Fortran copies inputs, keeping caller vectors immutable.
  as.numeric(out$score) # The Fortran subroutine writes the scalar score in-place.
}

#' Calculate PR-AUC with Rfast Ranking
#' @noRd
TuneBoostTree_CalculatePrAucRfast <- function(actual, predicted, positiveCount = sum(actual == 1L)) {
  orderIndex <- Rfast::Order(as.numeric(predicted), stable = TRUE, descending = TRUE) # Rfast performs the ranking in compiled code while stable ties match base R ordering.
  TuneBoostTree_CalculatePrAucOrdered(actual[orderIndex], positiveCount) # Shared accumulation keeps backend semantics aligned.
}

#' Calculate PR-AUC with Base R Ranking
#' @noRd
TuneBoostTree_CalculatePrAucR <- function(actual, predicted, positiveCount = sum(actual == 1L)) {
  orderIndex <- order(predicted, decreasing = TRUE) # Base R is the fully portable scorer.
  TuneBoostTree_CalculatePrAucOrdered(actual[orderIndex], positiveCount) # Shared accumulation avoids drift between fallback implementations.
}

#' Accumulate Ordered PR-AUC
#' @noRd
TuneBoostTree_CalculatePrAucOrdered <- function(actualOrd, positiveCount) {
  tp <- cumsum(actualOrd == 1L) # Cumulative true positives define recall at each threshold.
  fp <- cumsum(actualOrd == 0L) # Cumulative false positives define precision at each threshold.
  precision <- c(1, tp / pmax(tp + fp, 1)) # The leading precision anchor preserves the original integration convention.
  recall <- c(0, tp / positiveCount) # The leading zero recall anchors the PR curve at no selected positives.
  sum((recall[-1L] - recall[-length(recall)]) * precision[-1L]) # Right-continuous accumulation matches the existing scorer.
}

#' Normalize Parameters
#'
#' @param parameterData data.frame of candidate parameters.
#' @param parameterNames Ordered parameter names to normalize.
#'
#' @details Rounds integer-like parameters and stabilizes continuous values for cache keys and matching.
#'
#' @return A normalized data.frame.
#' @noRd
TuneBoostTree_NormalizeParams <- function(parameterData, parameterNames) {
  parameterData <- as.data.frame(parameterData, stringsAsFactors = FALSE) # Converting first makes data.frame and data.table inputs behave identically.
  parameterData <- parameterData[, parameterNames, drop = FALSE] # Column restriction keeps cache keys independent of log metadata.
  for (parameterName in parameterNames) parameterData[[parameterName]] <- as.numeric(parameterData[[parameterName]]) # Numeric coercion makes optimizer output and warm-start grids comparable.
  integerParameters <- intersect(c("tree_depth", "min_n", "max_bin", "num_leaves", "min_data_in_leaf"), parameterNames)
  for (parameterName in integerParameters) parameterData[[parameterName]] <- as.integer(round(parameterData[[parameterName]])) # Integer hyperparameters are canonicalized before cache-key generation.
  for (parameterName in setdiff(parameterNames, integerParameters)) parameterData[[parameterName]] <- round(parameterData[[parameterName]], digits = 12L) # Rounding continuous values prevents floating-point noise from defeating deduplication.
  parameterData # Returning the same shape simplifies downstream replacement.
}

#' Compare Scores
#'
#' @param scoreA First numeric score.
#' @param scoreB Second numeric score.
#' @param tolerance Relative tolerance for equality.
#'
#' @details Internal helper for matching optimizer best values back to the evaluation log.
#'
#' @return Logical scalar.
#' @noRd
TuneBoostTree_IsScoreMatch <- function(scoreA, scoreB, tolerance = 1e-6) {
  scoreA <- as.numeric(scoreA) # Numeric coercion tolerates data.table/data.frame scalar extraction.
  scoreB <- as.numeric(scoreB) # Matching should not depend on the container type of the optimizer result.
  is.finite(scoreA) && is.finite(scoreB) && abs(scoreA - scoreB) <= tolerance * max(1, abs(scoreA), abs(scoreB)) # Relative tolerance avoids false mismatches from minor numeric formatting differences.
}


#' Complete Parameter Grid Columns
#' @noRd
TuneBoostTree_CompleteParameterGrid <- function(gridData, bounds) {
  gridData <- as.data.frame(gridData, stringsAsFactors = FALSE)
  for (parameterName in names(bounds)) {
    if (!parameterName %in% names(gridData)) gridData[[parameterName]] <- mean(as.numeric(bounds[[parameterName]]))
  }
  gridData
}

#' Create Initialization Grid
#'
#' @param historyData Evaluation log data.frame or data.table.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Converts logged evaluations into the warm-start schema expected by Bayesian optimization.
#'
#' @return A data.frame or `NULL`.
#' @noRd
TuneBoostTree_CreateInitGrid <- function(historyData, bounds) {
  if (is.null(historyData) || nrow(historyData) == 0L) return(NULL) # Empty logs cannot seed future optimizer calls.
  requiredNames <- c(names(bounds), "Value") # Warm starts require parameter columns plus Value.
  historyData <- TuneBoostTree_CompleteParameterGrid(historyData, bounds) # Backfills newly added tunable parameters for older warm-start histories.
  out <- as.data.frame(historyData[, requiredNames, drop = FALSE], stringsAsFactors = FALSE) # Data.frame output matches the optimizer's init_grid_dt expectation.
  out <- out[stats::complete.cases(out), , drop = FALSE] # Incomplete rows would fail or corrupt warm-start optimization.
  if (nrow(out) == 0L) return(NULL) # Returning NULL avoids passing an empty unusable grid.
  out # Complete history rows are reusable as future initial grid points.
}

#' Combine Initialization Grids
#'
#' @param initGridDt Existing warm-start grid.
#' @param newInitGridDt Newly created warm-start grid.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Merges previous and current evaluations, retaining only the best duplicate score per parameter key.
#'
#' @return A deduplicated data.frame or `NULL`.
#' @noRd
TuneBoostTree_CombineInitGrid <- function(initGridDt, newInitGridDt, bounds) {
  if (is.null(initGridDt)) return(newInitGridDt) # No previous grid means the new evaluations are sufficient.
  if (is.null(newInitGridDt)) return(initGridDt) # No new evaluations means the previous warm-start history is preserved.
  TuneBoostTree_DeduplicateInitGrid(rbind(initGridDt, newInitGridDt), bounds) # Deduplication keeps the strongest observed score for repeated candidates.
}

#' Deduplicate Initialization Grid
#'
#' @param gridData Warm-start grid data.frame.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Normalizes hyperparameters and keeps the row with maximum `Value` for each parameter key.
#'
#' @return A deduplicated data.frame or `NULL`.
#' @noRd
TuneBoostTree_DeduplicateInitGrid <- function(gridData, bounds) {
  if (is.null(gridData)) return(NULL) # NULL warm-starts are valid and should pass through unchanged.
  if (nrow(gridData) == 0L) return(gridData) # Empty grids remain empty without unnecessary work.
  parameterNames <- names(bounds) # Bounds define the canonical parameter set and ordering.
  requiredNames <- c(parameterNames, "Value") # The optimizer requires each candidate's score in Value.
  gridData <- TuneBoostTree_CompleteParameterGrid(gridData, bounds) # Backfills newly added tunable parameters for older warm-start histories.
  gridData <- as.data.frame(gridData[, requiredNames, drop = FALSE], stringsAsFactors = FALSE) # Dropping extra columns prevents optimizer API surprises.
  gridData <- gridData[stats::complete.cases(gridData), , drop = FALSE] # Complete cases are required for deterministic cache keys.
  if (nrow(gridData) == 0L) return(gridData) # Preserve data.frame type for empty-but-valid grids.
  normalizedData <- TuneBoostTree_NormalizeParams(gridData, parameterNames) # Normalization aligns warm-start rows with objective cache keys.
  for (parameterName in parameterNames) gridData[[parameterName]] <- normalizedData[[parameterName]] # Replacing original values makes returned grids canonical.
  key <- do.call(paste, c(gridData[, parameterNames, drop = FALSE], sep = "|")) # Pipe-separated keys match the evaluation cache convention.
  gridData$key__ <- key # A temporary key column enables vectorized duplicate handling without nested helper functions.
  gridData$order__ <- seq_len(nrow(gridData)) # Original row order provides deterministic tie-breaking after sorting.
  gridData <- gridData[order(gridData$key__, -gridData$Value, gridData$order__), , drop = FALSE] # Sorting places the best score for each normalized key first.
  gridData <- gridData[!duplicated(gridData$key__), , drop = FALSE] # First duplicate retention keeps the strongest observed score per candidate.
  gridData[, setdiff(names(gridData), c("key__", "order__")), drop = FALSE] # Temporary columns are removed before returning an optimizer-compatible grid.
}

#' Find Best Iteration
#'
#' @param evaluationLog Evaluation log data.table.
#' @param bestHyperparameters Named best-parameter list.
#' @param bestScore Numeric best score.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Finds the early-stopped iteration corresponding to the optimizer's best parameter/score pair.
#'
#' @return Integer best iteration or `NULL`.
#' @noRd
TuneBoostTree_FindBestIteration <- function(evaluationLog, bestHyperparameters, bestScore, bounds) {
  if (is.null(evaluationLog) || nrow(evaluationLog) == 0L) return(NULL) # Warm-start-only results may not have a fresh log row.
  parameterNames <- names(bounds) # The bounds order is the same order used by normalization and logging.
  normalizedBest <- TuneBoostTree_NormalizeParams(as.data.frame(bestHyperparameters, stringsAsFactors = FALSE), parameterNames) # Optimizer output needs the same rounding as logged candidates.
  normalizedLog <- TuneBoostTree_NormalizeParams(evaluationLog, parameterNames) # Log rows are normalized before equality matching.
  matched <- rep(TRUE, nrow(normalizedLog)) # Starting with all rows makes conjunction over parameters simple and fast.
  for (parameterName in parameterNames) matched <- matched & (normalizedLog[[parameterName]] == normalizedBest[[parameterName]][1L]) # Exact comparison is safe after canonical rounding.
  matched <- matched & vapply(evaluationLog$Value, TuneBoostTree_IsScoreMatch, logical(1L), scoreB = bestScore) # Score tolerance distinguishes duplicate parameter rows with different outcomes.
  if (!any(matched)) return(NULL) # Caller can recompute CV if the best row came only from init_grid_dt.
  as.integer(evaluationLog$bestIteration[[which(matched)[1L]]]) # The first matching row provides the stored effective round count.
}

#' Split Data Into Stratified Boost-Tree Folds
#'
#' @param yData Integer or logical binary target vector.
#' @param nFolds Integer number of folds.
#' @param seed Integer random seed.
#'
#' @details Public wrapper intended for outer cross-validation or workflows that need the same stratification as the tuner.
#'
#' @return A list of integer test-index vectors.
#' @export
SplitDataBoostTreeFolds <- function(yData, nFolds = 10L, seed = 42L) {
  if (length(yData) == 0L || anyNA(yData)) cli::cli_abort("`yData` must be a non-empty binary vector without NA.") # Public wrapper should fail before producing invalid folds.
  yData <- as.integer(yData) # The internal splitter expects numeric 0/1 labels.
  if (!all(yData %in% c(0L, 1L))) cli::cli_abort("`yData` must contain only 0 and 1 values.") # Stratification logic assumes binary labels.
  if (any(table(yData) < as.integer(nFolds))) cli::cli_abort("Each class must contain at least `nFolds` observations.") # Empty class slices would create invalid validation folds.
  TuneBoostTree_CreateStratifiedFolds(yData, nFolds, seed) # Delegation keeps public and internal fold behavior identical.
}

#' Fit a Boosted Tree Model
#'
#' @param formula A two-sided formula with one binary outcome and numeric predictors.
#' @param dataTrain Training data.frame.
#' @param hyperparameters Named list from `TuneBoostTreeBayesian` or equivalent canonical names.
#' @param featureTypes Optional XGBoost feature type vector.
#' @param targetLevels Optional two-level target ordering.
#' @param scalePosWeight Optional positive-class weight; computed when `NULL`.
#' @param nThreads Integer engine threads.
#' @param seed Integer random seed.
#' @param verbose Engine verbosity.
#' @param engine_boost_tree Engine name, `"xgboost"` or `"lightgbm"`.
#'
#' @details Trains the final model with canonical hyperparameters and dispatches only at the engine boundary.
#'
#' @return A named list containing model object, params, feature metadata, class metadata, rounds, and engine.
#' @export
FitBoostTreeModel <- function(formula, dataTrain, hyperparameters, featureTypes = NULL, targetLevels = NULL, scalePosWeight = NULL, nThreads = 8L, seed = 42L, verbose = 0L, engine_boost_tree = "xgboost") {
  if (!(engine_boost_tree %in% c("xgboost", "lightgbm"))) cli::cli_abort("`engine_boost_tree` must be 'xgboost' or 'lightgbm'.") # Public dispatch must reject unknown engines.
  preparedTrain <- TuneBoostTree_PrepareMatrix(formula, dataTrain, featureTypes, targetLevels, NULL) # Matrix preparation mirrors tuning so final fit sees identical features.
  classCounts <- table(preparedTrain$yData) # Class counts provide the default imbalance weight.
  if (length(classCounts) != 2L || any(classCounts == 0L)) cli::cli_abort("`dataTrain` must contain both binary classes.") # Binary boosters require both classes for meaningful training.
  scalePosWeight <- if (is.null(scalePosWeight)) as.numeric(classCounts[["0"]] / classCounts[["1"]]) else as.numeric(scalePosWeight) # Preserve original class-weight heuristic unless caller overrides.
  evalMetric <- if (is.null(hyperparameters$eval_metric)) "aucpr" else as.character(hyperparameters$eval_metric) # Stored tuning metrics make final params auditable.
  scalePosWeight <- if (!is.null(hyperparameters$scale_pos_weight)) hyperparameters$scale_pos_weight else scalePosWeight
  paramsValue <- TuneBoostTree_BuildParams(hyperparameters, nThreads, scalePosWeight, seed, evalMetric, engine_boost_tree) # Engine params are translated in one internal function.
  nRounds <- if (is.null(hyperparameters$trees)) 100L else as.integer(hyperparameters$trees) # Tuned best iteration controls final training length.
  trainObject <- TuneBoostTree_CreateDataObject(preparedTrain$xMatrix, preparedTrain$yData, preparedTrain$featureTypes, nThreads, engine_boost_tree) # Final training uses the same centralized data-object builder.
  if (engine_boost_tree == "xgboost") {
    model <- xgboost::xgb.train(params = paramsValue, data = trainObject, nrounds = nRounds, verbose = as.integer(verbose)) # XGBoost training receives DMatrix and canonical translated params.
  } else {
    model <- lightgbm::lgb.train(params = paramsValue, data = trainObject, nrounds = nRounds, verbose = as.integer(verbose)) # LightGBM training receives Dataset and num_iterations via nrounds.
  }
  threshold <- if (is.null(hyperparameters$threshold)) 0.5 else as.numeric(hyperparameters$threshold)[1L]
  list(model = model, params = paramsValue, featureNames = preparedTrain$featureNames, featureTypes = preparedTrain$featureTypes, targetLevels = preparedTrain$targetLevels, targetName = preparedTrain$targetName, negativeClass = preparedTrain$negativeClass, positiveClass = preparedTrain$positiveClass, formulaInfo = preparedTrain$formulaInfo, nRounds = nRounds, threshold = threshold, engine = engine_boost_tree) # The returned object contains everything prediction/performance needs.
}

#' Predict With a Boosted Tree Model
#'
#' @param modelObj Model object returned by `FitBoostTreeModel`.
#' @param newdata New data.frame containing all predictor columns.
#' @param threshold Positive-class probability threshold.
#' @param engine_boost_tree Optional engine override; defaults to `modelObj$engine`.
#'
#' @details Dispatches prediction based on the stored engine and returns class labels plus both class probabilities.
#'
#' @return A data.frame with `predictedClass`, `probabilityFirstClass`, and `probabilitySecondClass`.
#' @export
PredictBoostTreeModel <- function(modelObj, newdata, threshold = NULL, engine_boost_tree = NULL) {
  if (!is.data.frame(newdata) || nrow(newdata) == 0L) cli::cli_abort("`newdata` must be a non-empty data.frame.") # Public prediction should catch malformed scoring data early.
  if (is.null(threshold)) threshold <- if (!is.null(modelObj$threshold)) modelObj$threshold else 0.5 # Tuned models carry an optimized threshold; legacy models fall back to 0.5.
  threshold <- as.numeric(threshold) # Numeric thresholds support integer caller input without downstream surprises.
  if (length(threshold) != 1L || is.na(threshold) || threshold <= 0 || threshold >= 1) cli::cli_abort("`threshold` must be between 0 and 1.") # Binary classification thresholds outside (0,1) are not meaningful.
  engine <- if (is.null(engine_boost_tree)) modelObj$engine else engine_boost_tree # Stored engine is the default so callers do not repeat configuration.
  if (!(engine %in% c("xgboost", "lightgbm"))) cli::cli_abort("Model engine must be 'xgboost' or 'lightgbm'.") # Prediction dispatch must be exhaustive.
  featureNames <- modelObj$featureNames # Feature names from training define required scoring columns.
  missingFeatureNames <- setdiff(featureNames, names(newdata)) # Missing columns should be reported before native predict errors.
  if (length(missingFeatureNames) > 0L) cli::cli_abort("`newdata` is missing required predictors: {paste(missingFeatureNames, collapse = ', ')}") # Explicit names speed up user correction.
  newdataFrame <- as.data.frame(newdata) # Normalizing tibble/data.table inputs gives stable prediction subsetting semantics.
  numericMatrix <- data.matrix(newdataFrame[, featureNames, drop = FALSE]) # Prediction matrices must use the training feature order.
  storage.mode(numericMatrix) <- "double" # Native predictors expect numeric matrices for efficient scoring.
  colnames(numericMatrix) <- featureNames # Names are preserved for engines that verify feature alignment.
  if (engine == "xgboost") {
    nThreads <- if (is.null(modelObj$params$nthread)) 1L else as.integer(modelObj$params$nthread) # Prediction DMatrix uses the training thread cap when available.
    predictionObject <- TuneBoostTree_CreateDataObject(numericMatrix, NULL, modelObj$featureTypes, nThreads, "xgboost") # XGBoost prediction expects DMatrix for consistency with training.
    probabilitySecondClass <- as.numeric(stats::predict(modelObj$model, newdata = predictionObject)) # XGBoost returns positive-class probabilities for binary logistic models.
  } else {
    probabilitySecondClass <- as.numeric(stats::predict(modelObj$model, data = numericMatrix)) # LightGBM predict works directly on a numeric matrix.
  }
  probabilityFirstClass <- 1 - probabilitySecondClass # Binary probabilities are complements under logistic objectives.
  predictedClass <- ifelse(probabilitySecondClass >= threshold, modelObj$targetLevels[2L], modelObj$targetLevels[1L]) # Thresholding uses the stored class ordering.
  out <- data.frame(predictedClass = predictedClass, probabilityFirstClass = probabilityFirstClass, probabilitySecondClass = probabilitySecondClass, stringsAsFactors = FALSE) # A simple data.frame preserves the legacy prediction contract.
  attr(out, "targetName") <- modelObj$targetName # Metadata supports downstream performance helpers without extra arguments.
  attr(out, "targetLevels") <- modelObj$targetLevels # Returning class levels keeps predictions self-describing.
  out # The prediction frame is ready for confusion summaries and user scoring.
}

#' Evaluate Boosted Tree Performance
#'
#' @param modelObj Model object returned by `FitBoostTreeModel`.
#' @param testData Test data.frame containing predictors and outcome.
#' @param formula A two-sided formula identifying the outcome.
#'
#' @details Calls `PredictBoostTreeModel` internally and computes PR-AUC plus a confusion summary.
#'
#' @return A list with `prAuc`, `confusionSummary`, and `predictions`.
#' @export
PerformanceBoostTreeModel <- function(modelObj, testData, formula) {
  predictions <- PredictBoostTreeModel(modelObj, testData) # Reusing the public predictor guarantees identical engine dispatch and output schema.
  formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, testData) # Formula parsing identifies the observed outcome column for scoring.
  preparedTarget <- TuneBoostTree_PrepareTarget(testData[[formulaInfo$targetName]], modelObj$targetLevels) # Target preparation mirrors training labels for PR-AUC.
  prAuc <- TuneBoostTree_CalculatePrAuc(preparedTarget$yData, predictions$probabilitySecondClass) # The same internal PR-AUC implementation keeps tuning and holdout metrics comparable.
  confusionSummary <- table(actual = testData[[formulaInfo$targetName]], predicted = predictions$predictedClass) # A compact confusion table helps diagnose threshold behavior.
  list(prAuc = prAuc, confusionSummary = confusionSummary, predictions = predictions) # Returning raw predictions supports custom downstream metrics.
}
