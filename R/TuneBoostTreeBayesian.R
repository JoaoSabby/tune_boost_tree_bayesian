#' Tune Bayesian Gradient Boosted Tree Hyperparameters
#'
#' @param formula A two-sided formula with one binary outcome and numeric predictors.
#' @param dataTrain A data.frame containing the training rows used during tuning.
#' @param nFolds Integer number of stratified folds for cross-validation.
#' @param initGridDt Optional data.frame returned by a previous call for warm-starting Bayesian optimization.
#' @param initPoints Integer number of random initial points when `initGridDt` is not enough.
#' @param nIter Integer number of Bayesian optimization iterations after initialization.
#' @param nRoundsTuning Integer boosting-round ceiling used only inside cross-validation tuning.
#' @param nRoundsFinal Integer fallback boosting-round count for the final model when CV cannot infer one.
#' @param earlyStoppingRounds Integer patience for aggressive early stopping during tuning.
#' @param seed Integer random seed forwarded to fold splitting and model engines.
#' @param nThreads Integer maximum threads available to a single sequential model fit.
#' @param nWorkersFolds Integer number of fold workers for manual CV; one keeps a fast sequential loop.
#' @param bounds Named list of Bayesian bounds; defaults preserve the original seven-parameter search space.
#' @param featureTypes Optional XGBoost feature type vector aligned to predictors.
#' @param evalMetric Metric name for XGBoost; LightGBM always uses `average_precision` internally.
#' @param acq Acquisition function passed to `rBayesianOptimization::BayesianOptimization`.
#' @param kappa Exploration parameter passed to Bayesian optimization.
#' @param eps Improvement parameter passed to Bayesian optimization.
#' @param verbose Logical or integer controlling optimizer and engine verbosity.
#' @param scalePosWeight Optional class-imbalance weight for unbalanced CV; computed from data when `NULL`.
#' @param balanceFn Optional function with signature `function(data, formula)` applied once per fold.
#' @param engine_boost_tree Boosting engine, either `"xgboost"` or `"lightgbm"`.
#' @param prAucBackend PR-AUC implementation, one of `"auto"`, `"c"`, `"fortran"`, `"rfast"`, or `"r"`.
#'
#' @details
#' This function tunes the same seven hyperparameters for XGBoost and LightGBM while translating names only at the engine boundary.
#' `nRoundsTuning` is intentionally separate from `nRoundsFinal` because tuning with very high final-training round counts is often the dominant runtime cost; the default tuning ceiling is 500 with early stopping at 20 rounds.
#' Warm-starting is supported by passing the returned `initGridDt` from one call into a later call; the grid is deduplicated and forwarded as `init_grid_dt` to `rBayesianOptimization::BayesianOptimization` so previous evaluations are reused.
#'
#' @return A list with `bestHyperparameters`, `bestScore`, `initGridDt`, and `evaluationLog`.
#' @export
TuneBoostTreeBayesian <- function(
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
) {
  if (!inherits(formula, "formula") || length(formula) != 3L) cli::cli_abort("`formula` must be a two-sided formula.") # Fail early because user-facing entry points should report malformed model specifications clearly.
  if (!is.data.frame(dataTrain) || nrow(dataTrain) == 0L) cli::cli_abort("`dataTrain` must be a non-empty data.frame.") # Protect downstream engine calls from opaque data-shape errors.
  if (!(engine_boost_tree %in% c("xgboost", "lightgbm"))) cli::cli_abort("`engine_boost_tree` must be 'xgboost' or 'lightgbm'.") # Keep engine dispatch branches exhaustive.
  prAucBackend <- TuneBoostTree_SelectPrAucBackend(prAucBackend) # Resolving the scorer once avoids repeated namespace and native-symbol checks in every fold.
  if (!is.null(balanceFn) && !is.function(balanceFn)) cli::cli_abort("`balanceFn` must be a function when supplied.") # Balance callbacks are user supplied, so type errors should be explicit.
  if (engine_boost_tree == "xgboost" && !(evalMetric %in% c("aucpr", "auc"))) cli::cli_abort("`evalMetric` must be 'aucpr' or 'auc' for XGBoost.") # XGBoost receives the metric verbatim and unsupported names fail late.

  timerStart <- proc.time()[["elapsed"]] # proc.time keeps elapsed reporting monotonic and compliant with non-wall-clock timing guidance.
  parameterNames <- c("eta", "maxDepth", "minChildWeight", "subsample", "colsampleBynode", "gamma", "maxBin") # A single ordered parameter vector keeps cache keys, grids, and logs aligned.
  bounds <- if (is.null(bounds)) TuneBoostTree_GetDefaultBounds() else bounds # Defaults preserve the original search surface unless the caller narrows it.
  missingBounds <- setdiff(parameterNames, names(bounds)) # Public validation belongs at orchestration boundaries, not internal helpers.
  extraBounds <- setdiff(names(bounds), parameterNames) # Rejecting extras prevents silent optimizer arguments that the objective ignores.
  if (length(missingBounds) > 0L || length(extraBounds) > 0L) cli::cli_abort("`bounds` must contain exactly the seven supported hyperparameters.") # Exact bounds keep the multi-engine interface stable.
  initGridDt <- TuneBoostTree_DeduplicateInitGrid(initGridDt, bounds = bounds) # Deduping before optimization avoids redundant warm-start objective calls.

  totalCores <- max(1L, parallel::detectCores(logical = TRUE)) # Runtime CPU detection prevents hard-coded thread assumptions across hosts.
  nWorkersFolds <- max(1L, as.integer(nWorkersFolds)) # Worker counts below one would break parallel backends.
  workerThreads <- max(1L, floor(totalCores / nWorkersFolds)) # Dividing threads by workers avoids oversubscribing native engine thread pools.
  workerThreads <- min(as.integer(nThreads), workerThreads) # Respecting caller limits prevents this function from using more threads than requested.

  useBalancedCv <- !is.null(balanceFn) # Balanced CV requires manual fold preparation because each training partition changes.
  if (useBalancedCv) {
    balancedFolds <- TuneBoostTree_PrepareBalancedFolds(formula, dataTrain, nFolds, balanceFn, workerThreads, seed, engine_boost_tree) # Balance once per fold and cache engine datasets for repeated objective calls.
    scalePosWeightValue <- NULL # Per-fold balanced weights are stored on each fold to reflect the callback output.
  } else {
    formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, dataTrain) # Reusing formula parsing avoids repeated model-frame work.
    preparedTrain <- TuneBoostTree_PrepareMatrix(formula, dataTrain, featureTypes, NULL, formulaInfo) # The unbalanced path uses the full training matrix for stratified CV folds.
    classCounts <- table(preparedTrain$yData) # Weight inference requires class counts only once for the full training data.
    if (length(classCounts) != 2L || any(classCounts == 0L)) cli::cli_abort("`dataTrain` must contain both binary classes.") # Engines cannot learn a binary objective from a single class.
    scalePosWeightValue <- if (is.null(scalePosWeight)) as.numeric(classCounts[["0"]] / classCounts[["1"]]) else as.numeric(scalePosWeight) # The original imbalance heuristic is preserved for unbalanced CV.
    folds <- TuneBoostTree_CreateStratifiedFolds(preparedTrain$yData, nFolds, seed) # Stratification stabilizes PR-AUC estimates for imbalanced outcomes.
    balancedFolds <- vector("list", length(folds)) # The manual runner is shared by balanced and unbalanced paths to avoid duplicate training logic.
    for (foldId in seq_along(folds)) {
      testIndex <- folds[[foldId]] # Keeping indices local makes each fold object self-contained for parallel workers.
      trainIndex <- setdiff(seq_len(nrow(dataTrain)), testIndex) # The complement defines the training partition without storing a full fold matrix twice.
      trainMatrix <- preparedTrain$xMatrix[trainIndex, , drop = FALSE] # Slicing cached matrices avoids rebuilding model matrices inside the optimizer.
      testMatrix <- preparedTrain$xMatrix[testIndex, , drop = FALSE] # Cached test objects satisfy the no-recreation rule for CV evaluation.
      dtrain <- TuneBoostTree_CreateDataObject(trainMatrix, preparedTrain$yData[trainIndex], preparedTrain$featureTypes, workerThreads, engine_boost_tree) # Engine-specific data construction is centralized for maintainability.
      dtest <- TuneBoostTree_CreateDataObject(testMatrix, preparedTrain$yData[testIndex], preparedTrain$featureTypes, workerThreads, engine_boost_tree) # Test data objects are cached once per fold for performance.
      if (engine_boost_tree == "xgboost") {
        balancedFolds[[foldId]] <- list(dtrain = dtrain, dtest = dtest, yTest = preparedTrain$yData[testIndex], scalePosWeight = scalePosWeightValue, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels) # XGBoost branch stores DMatrix objects under engine-native names.
      } else {
        balancedFolds[[foldId]] <- list(dstrain = dtrain, dstest = dtest, xTest = as.matrix(testMatrix), yTest = preparedTrain$yData[testIndex], scalePosWeight = scalePosWeightValue, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels) # LightGBM branch keeps the cached Dataset and raw matrix because prediction APIs score matrices.
      }
    }
  }

  cacheEnv <- new.env(parent = emptyenv()) # A call-local hash cache avoids repeated CV runs without package-level state.
  evaluationLogList <- vector("list", as.integer(initPoints) + as.integer(nIter) + 32L) # Preallocation avoids repeated list growth during Bayesian callbacks.
  logIndex <- 0L # An integer cursor allows cheap append semantics for the evaluation log.
  objective <- TuneBoostTree_EvaluateCv # Reusing a top-level function satisfies the no nested function-definition requirement.
  environment(objective) <- environment() # Binding the objective to call-local state avoids global variables while matching the optimizer API.

  if (isTRUE(verbose)) cli::cli_inform("Starting {.val {engine_boost_tree}} Bayesian tuning with {.val {nRoundsTuning}} tuning rounds and {.val {earlyStoppingRounds}} early-stopping rounds.") # Runtime context helps users understand the performance-oriented defaults.
  set.seed(seed) # Seeding immediately before optimization preserves reproducibility of initial random points.
  tuningResult <- rBayesianOptimization::BayesianOptimization(
    FUN = objective,
    bounds = bounds,
    init_grid_dt = initGridDt,
    init_points = as.integer(initPoints),
    n_iter = as.integer(nIter),
    acq = acq,
    kappa = kappa,
    eps = eps,
    verbose = verbose
  ) # The optimizer contract requires FUN to return list(Score, Pred), which the top-level objective does.

  evaluationLog <- if (logIndex > 0L) data.table::rbindlist(evaluationLogList[seq_len(logIndex)], fill = TRUE) else data.table::data.table() # Consolidating once preserves the fast log pattern from the original implementation.
  bestHyperparameters <- as.list(tuningResult$Best_Par) # The optimizer returns the canonical parameter names used by callers.
  bestScore <- as.numeric(tuningResult$Best_Value) # Storing a scalar avoids surprises from optimizer-specific numeric classes.
  bestIteration <- TuneBoostTree_FindBestIteration(evaluationLog, bestHyperparameters, bestScore, bounds) # Reuse the logged CV result when available to avoid another expensive CV pass.
  if (is.null(bestIteration)) {
    bestSummary <- TuneBoostTree_RunCvManual(balancedFolds, bestHyperparameters, nRoundsTuning, earlyStoppingRounds, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend) # A fallback CV run recovers the best iteration when the optimizer best came from warm-start data.
    bestIteration <- as.integer(bestSummary$bestIteration) # Final training should use the effective early-stopped iteration from tuning.
  }
  if (is.null(bestIteration) || is.na(bestIteration) || bestIteration < 1L) bestIteration <- as.integer(nRoundsFinal) # A final-round fallback keeps downstream training usable if every metric is unavailable.
  bestHyperparameters$nRounds <- as.integer(bestIteration) # The returned structure preserves the original final-model contract.
  bestHyperparameters$evalMetric <- if (engine_boost_tree == "xgboost") evalMetric else "average_precision" # Reporting the effective engine metric avoids ambiguity for LightGBM users.
  if (!useBalancedCv) bestHyperparameters$scalePosWeight <- as.numeric(scalePosWeightValue) # Unbalanced CV uses one global class weight, so it is safe to return.

  newInitGridDt <- TuneBoostTree_CreateInitGrid(evaluationLog, bounds) # The log is transformed into a warm-startable grid for subsequent calls.
  returnedInitGridDt <- TuneBoostTree_CombineInitGrid(initGridDt, newInitGridDt, bounds) # Combining old and new points preserves cross-call optimizer history.
  if (isTRUE(verbose)) cli::cli_inform("Finished Bayesian tuning in {.val {round(proc.time()[['elapsed']] - timerStart, 2)}} seconds.") # proc.time-based reporting follows the required timing source.

  list(bestHyperparameters = bestHyperparameters, bestScore = bestScore, initGridDt = returnedInitGridDt, evaluationLog = evaluationLog) # The return shape remains compatible with the original tuning workflow.
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
  if (is.null(targetLevels)) {
    targetLevels <- if (is.factor(targetData)) rev(levels(targetData)) else sort(unique(as.character(targetData))) # The factor reversal preserves the current implementation's positive-class convention.
  }
  targetLevels <- as.character(targetLevels) # Character levels avoid factor code leakage when comparing labels.
  positiveClass <- targetLevels[2L] # The second level is the positive class for binary logistic objectives.
  negativeClass <- targetLevels[1L] # The first level is used for complementary probabilities and class output.
  yData <- as.integer(as.character(targetData) == positiveClass) # Engines require numeric 0/1 labels for binary objectives.
  list(yData = yData, targetLevels = targetLevels, negativeClass = negativeClass, positiveClass = positiveClass) # Keeping label metadata with y avoids recomputation in fit/predict helpers.
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
  xData <- data[, featureNames, drop = FALSE] # Restricting columns avoids accidental leakage from unused data fields.
  numericMatrix <- data.matrix(xData) # data.matrix is fast for already numeric data and preserves row order.
  storage.mode(numericMatrix) <- "double" # XGBoost and LightGBM expect numeric doubles for efficient native ingestion.
  colnames(numericMatrix) <- featureNames # Native boosters carry names forward for prediction-time alignment.
  xMatrix <- if (mean(numericMatrix == 0) > 0.7) Matrix::Matrix(numericMatrix, sparse = TRUE) else numericMatrix # Sparse conversion is only worth the overhead when zeros dominate the original matrix.
  preparedTarget <- TuneBoostTree_PrepareTarget(data[[formulaInfo$targetName]], targetLevels) # Target preparation is centralized to keep class semantics identical.
  if (!is.null(featureTypes)) names(featureTypes) <- featureNames # Named feature types make accidental reordering easier to diagnose from model objects.
  list(xMatrix = xMatrix, yData = preparedTarget$yData, featureNames = featureNames, featureTypes = featureTypes, targetLevels = preparedTarget$targetLevels, targetName = formulaInfo$targetName, negativeClass = preparedTarget$negativeClass, positiveClass = preparedTarget$positiveClass, formulaInfo = formulaInfo) # This structure carries all metadata needed by fit, CV, and predict.
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
  list(eta = c(0.01, 0.15), maxDepth = c(2L, 10L), minChildWeight = c(1, 50), subsample = c(0.5, 1), colsampleBynode = c(0.3, 1), gamma = c(0, 5), maxBin = c(32L, 256L)) # Exact values preserve compatibility with the current implementation and prompt requirements.
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
TuneBoostTree_BuildParams <- function(hyperparameters, nThreads = 1L, scalePosWeight = 1, seed = 42L, evalMetric = "aucpr", engine_boost_tree = "xgboost") {
  etaValue <- as.numeric(hyperparameters[["eta"]]) # Canonical eta is translated to learning_rate only for LightGBM.
  maxDepthValue <- as.integer(round(as.numeric(hyperparameters[["maxDepth"]]))) # Integer depth avoids engine-side coercion differences across packages.
  minChildWeightValue <- as.numeric(hyperparameters[["minChildWeight"]]) # Hessian/child-weight semantics are engine-native but share one bound.
  subsampleValue <- as.numeric(hyperparameters[["subsample"]]) # Row sampling has equivalent tuning meaning across both engines.
  colsampleBynodeValue <- as.numeric(hyperparameters[["colsampleBynode"]]) # Node-level feature sampling is mapped per engine.
  gammaValue <- as.numeric(hyperparameters[["gamma"]]) # Split-gain regularization uses different names but same intent.
  maxBinValue <- as.integer(round(as.numeric(hyperparameters[["maxBin"]]))) # Histogram bin count must be integral for both engines.
  scalePosWeight <- as.numeric(scalePosWeight) # Native boosters expect a scalar numeric weight.
  if (engine_boost_tree == "xgboost") {
    return(list(objective = "binary:logistic", eval_metric = evalMetric, grow_policy = "depthwise", tree_method = "hist", max_bin = maxBinValue, max_depth = maxDepthValue, eta = etaValue, gamma = gammaValue, subsample = subsampleValue, min_child_weight = minChildWeightValue, colsample_bynode = colsampleBynodeValue, scale_pos_weight = scalePosWeight, nthread = as.integer(nThreads), seed = as.integer(seed))) # XGBoost parameters are grouped in one branch to avoid cross-engine leakage.
  }
  list(objective = "binary", boosting = "gbdt", metric = "average_precision", max_bin = maxBinValue, max_depth = maxDepthValue, learning_rate = etaValue, min_gain_to_split = gammaValue, bagging_fraction = subsampleValue, bagging_freq = 1L, min_sum_hessian_in_leaf = minChildWeightValue, feature_fraction_bynode = colsampleBynodeValue, scale_pos_weight = scalePosWeight, num_threads = as.integer(nThreads), seed = as.integer(seed), verbosity = -1L, verbose = -1L) # LightGBM-specific names and verbosity are isolated to this branch.
}

#' Prepare Balanced Folds
#'
#' @param formula A two-sided formula.
#' @param data Full training data.frame.
#' @param nFolds Integer number of folds.
#' @param balanceFn Function applied once to each training partition.
#' @param nThreads Integer threads for engine data construction.
#' @param seed Integer random seed.
#' @param engine_boost_tree Engine name, `"xgboost"` or `"lightgbm"`.
#'
#' @details Applies balancing once per fold and caches both train and test engine datasets for all objective evaluations.
#'
#' @return A list of fold objects containing cached engine datasets, labels, weights, and metadata.
#' @noRd
TuneBoostTree_PrepareBalancedFolds <- function(formula, data, nFolds, balanceFn, nThreads = 1L, seed = 42L, engine_boost_tree = "xgboost") {
  formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, data) # Parsing once keeps all balanced folds on the same feature contract.
  preparedFull <- TuneBoostTree_PrepareMatrix(formula, data, NULL, NULL, formulaInfo) # Full data is needed only to create stratified fold indices.
  folds <- TuneBoostTree_CreateStratifiedFolds(preparedFull$yData, nFolds, seed) # Stratification is computed before balancing to evaluate on natural holdout distributions.
  balancedFolds <- vector("list", length(folds)) # Preallocation keeps fold setup linear in the number of folds.
  for (foldId in seq_along(folds)) {
    testIndex <- folds[[foldId]] # The test slice remains untouched by balancing for honest validation.
    trainData <- data[setdiff(seq_len(nrow(data)), testIndex), , drop = FALSE] # Only training rows are passed to the external balancing strategy.
    testData <- data[testIndex, , drop = FALSE] # Test rows are cached once to eliminate repeated DMatrix/Dataset creation.
    balancedTrain <- balanceFn(trainData, formula) # Calling the balancer exactly once per fold avoids explosive work inside Bayesian iterations.
    preparedTrain <- TuneBoostTree_PrepareMatrix(formula, balancedTrain, NULL, NULL, formulaInfo) # Balanced data may be dense, so the sparse heuristic decides rather than forcing dgCMatrix.
    if (inherits(preparedTrain$xMatrix, "sparseMatrix")) preparedTrain$xMatrix <- as.matrix(preparedTrain$xMatrix) # ADASYN/NearMiss outputs are treated as dense to avoid sparse-conversion overhead after balancing.
    preparedTest <- TuneBoostTree_PrepareMatrix(formula, testData, NULL, preparedTrain$targetLevels, formulaInfo) # Test levels follow the balanced train levels to preserve class mapping.
    trainObject <- TuneBoostTree_CreateDataObject(preparedTrain$xMatrix, preparedTrain$yData, preparedTrain$featureTypes, nThreads, engine_boost_tree) # Cached train object amortizes engine data conversion across all hyperparameter evaluations.
    testObject <- TuneBoostTree_CreateDataObject(preparedTest$xMatrix, preparedTest$yData, preparedTest$featureTypes, nThreads, engine_boost_tree) # Cached test object satisfies the performance requirement for manual CV.
    classCounts <- table(preparedTrain$yData) # Per-fold weights reflect the actual post-balancing distribution.
    foldScalePosWeight <- as.numeric(classCounts[["0"]] / classCounts[["1"]]) # The original negative/positive ratio heuristic is retained per balanced fold.
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
  list(score = as.numeric(mean(foldScores, na.rm = TRUE)), bestIteration = as.integer(round(mean(foldBestIter, na.rm = TRUE))), foldScores = foldScores) # Mean PR-AUC and mean early-stop iteration summarize one hyperparameter evaluation.
}

#' Evaluate One Bayesian Parameter Set
#'
#' @param eta Learning-rate candidate.
#' @param maxDepth Depth candidate.
#' @param minChildWeight Child-weight candidate.
#' @param subsample Row-sampling candidate.
#' @param colsampleBynode Node feature-sampling candidate.
#' @param gamma Split-gain candidate.
#' @param maxBin Histogram-bin candidate.
#'
#' @details Top-level objective used by the optimizer; its environment is rebound by `TuneBoostTreeBayesian` to call-local state.
#'
#' @return A list with `Score` and `Pred` as required by `rBayesianOptimization`.
#' @noRd
TuneBoostTree_EvaluateCv <- function(eta, maxDepth, minChildWeight, subsample, colsampleBynode, gamma, maxBin) {
  hyperparameters <- list(eta = eta, maxDepth = maxDepth, minChildWeight = minChildWeight, subsample = subsample, colsampleBynode = colsampleBynode, gamma = gamma, maxBin = maxBin) # Canonical parameter names keep the objective engine-neutral.
  normalizedData <- TuneBoostTree_NormalizeParams(as.data.frame(hyperparameters, stringsAsFactors = FALSE), parameterNames) # Normalization prevents cache misses from equivalent rounded integer parameters.
  cacheKey <- paste(unlist(normalizedData[1L, parameterNames, drop = FALSE], use.names = FALSE), collapse = "|") # The required pipe-joined key is compact and deterministic.
  if (exists(cacheKey, envir = cacheEnv, inherits = FALSE)) {
    cachedResult <- get(cacheKey, envir = cacheEnv, inherits = FALSE) # Hash lookup avoids duplicate CV runs proposed by the optimizer.
    return(list(Score = as.numeric(cachedResult$score), Pred = 0)) # The optimizer requires a fixed return shape even for cached values.
  }
  cvSummary <- TuneBoostTree_RunCvManual(balancedFolds, hyperparameters, nRoundsTuning, earlyStoppingRounds, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend) # Manual CV uses cached data objects and fold-level parallelism.
  scoreValue <- as.numeric(cvSummary$score) # A scalar score is needed by BayesianOptimization.
  bestIteration <- as.integer(cvSummary$bestIteration) # Logging best iteration lets the final model avoid retuning rounds.
  logIndex <<- logIndex + 1L # Updating the call-local cursor avoids growing a data.frame inside the hot path.
  evaluationLogList[[logIndex]] <<- data.frame(eta = as.numeric(normalizedData$eta), maxDepth = as.numeric(normalizedData$maxDepth), minChildWeight = as.numeric(normalizedData$minChildWeight), subsample = as.numeric(normalizedData$subsample), colsampleBynode = as.numeric(normalizedData$colsampleBynode), gamma = as.numeric(normalizedData$gamma), maxBin = as.numeric(normalizedData$maxBin), Value = scoreValue, bestIteration = bestIteration, stringsAsFactors = FALSE) # List-backed logging preserves the original high-performance pattern.
  assign(cacheKey, list(score = scoreValue, bestIteration = bestIteration), envir = cacheEnv) # The per-call cache prevents repeated expensive CV evaluations.
  list(Score = scoreValue, Pred = 0) # rBayesianOptimization expects this exact list contract.
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
  if ("maxDepth" %in% parameterNames) parameterData$maxDepth <- as.integer(round(parameterData$maxDepth)) # Depth is an integer hyperparameter despite Bayesian numeric proposals.
  if ("maxBin" %in% parameterNames) parameterData$maxBin <- as.integer(round(parameterData$maxBin)) # Histogram bins are integral for both engines.
  for (parameterName in setdiff(parameterNames, c("maxDepth", "maxBin"))) parameterData[[parameterName]] <- round(parameterData[[parameterName]], digits = 12L) # Rounding continuous values prevents floating-point noise from defeating deduplication.
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
  requiredNames <- c(names(bounds), "Value") # rBayesianOptimization warm starts require parameter columns plus Value.
  historyData <- as.data.frame(historyData, stringsAsFactors = FALSE) # Converting first avoids data.table column-selection semantics in warm-start creation.
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
  gridData <- as.data.frame(gridData, stringsAsFactors = FALSE) # Converting first keeps data.table warm-start inputs on base subsetting semantics.
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
  evalMetric <- if (is.null(hyperparameters$evalMetric)) "aucpr" else as.character(hyperparameters$evalMetric) # Stored tuning metrics make final params auditable.
  paramsValue <- TuneBoostTree_BuildParams(hyperparameters, nThreads, scalePosWeight, seed, evalMetric, engine_boost_tree) # Engine params are translated in one internal function.
  nRounds <- if (is.null(hyperparameters$nRounds)) 100L else as.integer(hyperparameters$nRounds) # Tuned best iteration controls final training length.
  trainObject <- TuneBoostTree_CreateDataObject(preparedTrain$xMatrix, preparedTrain$yData, preparedTrain$featureTypes, nThreads, engine_boost_tree) # Final training uses the same centralized data-object builder.
  if (engine_boost_tree == "xgboost") {
    model <- xgboost::xgb.train(params = paramsValue, data = trainObject, nrounds = nRounds, verbose = as.integer(verbose)) # XGBoost training receives DMatrix and canonical translated params.
  } else {
    model <- lightgbm::lgb.train(params = paramsValue, data = trainObject, nrounds = nRounds, verbose = as.integer(verbose)) # LightGBM training receives Dataset and num_iterations via nrounds.
  }
  list(model = model, params = paramsValue, featureNames = preparedTrain$featureNames, featureTypes = preparedTrain$featureTypes, targetLevels = preparedTrain$targetLevels, targetName = preparedTrain$targetName, negativeClass = preparedTrain$negativeClass, positiveClass = preparedTrain$positiveClass, formulaInfo = preparedTrain$formulaInfo, nRounds = nRounds, engine = engine_boost_tree) # The returned object contains everything prediction/performance needs.
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
PredictBoostTreeModel <- function(modelObj, newdata, threshold = 0.5, engine_boost_tree = NULL) {
  if (!is.data.frame(newdata) || nrow(newdata) == 0L) cli::cli_abort("`newdata` must be a non-empty data.frame.") # Public prediction should catch malformed scoring data early.
  threshold <- as.numeric(threshold) # Numeric thresholds support integer caller input without downstream surprises.
  if (length(threshold) != 1L || is.na(threshold) || threshold <= 0 || threshold >= 1) cli::cli_abort("`threshold` must be between 0 and 1.") # Binary classification thresholds outside (0,1) are not meaningful.
  engine <- if (is.null(engine_boost_tree)) modelObj$engine else engine_boost_tree # Stored engine is the default so callers do not repeat configuration.
  if (!(engine %in% c("xgboost", "lightgbm"))) cli::cli_abort("Model engine must be 'xgboost' or 'lightgbm'.") # Prediction dispatch must be exhaustive.
  featureNames <- modelObj$featureNames # Feature names from training define required scoring columns.
  missingFeatureNames <- setdiff(featureNames, names(newdata)) # Missing columns should be reported before native predict errors.
  if (length(missingFeatureNames) > 0L) cli::cli_abort("`newdata` is missing required predictors: {paste(missingFeatureNames, collapse = ', ')}") # Explicit names speed up user correction.
  numericMatrix <- data.matrix(newdata[, featureNames, drop = FALSE]) # Prediction matrices must use the training feature order.
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
