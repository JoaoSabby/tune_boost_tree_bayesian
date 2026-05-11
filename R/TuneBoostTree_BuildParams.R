#' Montar parâmetros da engine
#'
#' @param hyperparameters Lista nomeada de hiperparâmetros canônicos do tuner.
#' @param nThreads Inteiro com threads atribuídas ao ajuste do modelo.
#' @param scalePosWeight Numeric positive-class weight.
#' @param seed Inteiro usado como semente aleatória.
#' @param evalMetric Nome da métrica de avaliação do XGBoost.
#' @param engine Nome da engine, `"xgboost"` ou `"lightgbm"`.
#'
#' @details Traduz nomes canônicos de parâmetros para listas específicas de cada engine.
#'
#' @return Lista nomeada pronta para `xgb.train` ou `lgb.train`.
#' @noRd

TuneBoostTree_BuildParams <- function(
  hyperparameters,
  nThreads = 1L,
  scalePosWeight = NULL,
  seed = 42L,
  evalMetric = "aucpr",
  engine = "xgboost"
) {

  learnRateValue <- as.numeric(hyperparameters[["learn_rate"]])
  treeDepthValue <- as.integer(round(as.numeric(hyperparameters[["tree_depth"]])))
  minNValue <- as.numeric(hyperparameters[["min_n"]])
  sampleSizeValue <- as.numeric(hyperparameters[["sample_size"]])
  mtryRaw <- hyperparameters[["mtry"]]
  if(is.null(mtryRaw) || (is.character(mtryRaw) && identical(mtryRaw[1L], "default"))) {
    mtryValue <- 0.8
  } else {
    mtryValue <- as.numeric(mtryRaw)[1L]
  }
  lossReductionValue <- as.numeric(hyperparameters[["loss_reduction"]])
  maxBinValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "max_bin", 255L)
  maxBinValue <- as.integer(round(maxBinValue))
  lambdaValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "lambda", NULL)
  alphaValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "alpha", NULL)
  maxDeltaStepValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "max_delta_step", NULL)
  colsampleBytreeValue <- TuneBoostTree_GetHyperparameter(
    hyperparameters,
    "colsample_bytree",
    NULL
  )
  colsampleBylevelValue <- TuneBoostTree_GetHyperparameter(
    hyperparameters,
    "colsample_bylevel",
    NULL
  )
  numLeavesValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "num_leaves", NULL)
  minDataInLeafValue <- TuneBoostTree_GetHyperparameter(
    hyperparameters,
    "min_data_in_leaf",
    NULL
  )
  tunedScalePosWeight <- TuneBoostTree_GetHyperparameter(
    hyperparameters,
    "scale_pos_weight",
    NULL
  )
  if(!is.null(tunedScalePosWeight)) {
    scalePosWeight <- tunedScalePosWeight
  }
  if(!is.null(scalePosWeight)) {
    scalePosWeight <- as.numeric(scalePosWeight)[1L]
  }
  if(engine == "xgboost") {
    params <- list(
      objective = "binary:logistic",
      eval_metric = evalMetric,
      grow_policy = "depthwise",
      tree_method = "hist",
      max_bin = maxBinValue,
      max_depth = treeDepthValue,
      eta = learnRateValue,
      gamma = lossReductionValue,
      subsample = sampleSizeValue,
      min_child_weight = minNValue,
      colsample_bynode = mtryValue,
      nthread = as.integer(nThreads),
      seed = as.integer(seed)
    )
    if(!is.null(lambdaValue)) {
      params$lambda <- as.numeric(lambdaValue)
    }
    if(!is.null(alphaValue)) {
      params$alpha <- as.numeric(alphaValue)
    }
    if(!is.null(maxDeltaStepValue)) {
      params$max_delta_step <- as.numeric(maxDeltaStepValue)
    }
    if(!is.null(colsampleBytreeValue)) {
      params$colsample_bytree <- as.numeric(colsampleBytreeValue)
    }
    if(!is.null(colsampleBylevelValue)) {
      params$colsample_bylevel <- as.numeric(colsampleBylevelValue)
    }
    if(!is.null(scalePosWeight)) {
      params$scale_pos_weight <- scalePosWeight
    }
    return(params)
  }
  lightgbmMetric <- as.character(evalMetric)[1L]
  if(identical(lightgbmMetric, "aucpr")) {
    lightgbmMetric <- "average_precision"
  }
  params <- list(
    objective = "binary",
    boosting = "gbdt",
    metric = lightgbmMetric,
    max_bin = maxBinValue,
    max_depth = treeDepthValue,
    learning_rate = learnRateValue,
    min_gain_to_split = lossReductionValue,
    bagging_fraction = sampleSizeValue,
    bagging_freq = 1L,
    min_sum_hessian_in_leaf = minNValue,
    feature_fraction_bynode = mtryValue,
    num_threads = as.integer(nThreads),
    seed = as.integer(seed),
    verbosity = -1L,
    verbose = -1L
  )
  if(!is.null(lambdaValue)) {
    params$lambda_l2 <- as.numeric(lambdaValue)
  }
  if(!is.null(alphaValue)) {
    params$lambda_l1 <- as.numeric(alphaValue)
  }
  if(!is.null(colsampleBytreeValue)) {
    params$feature_fraction <- as.numeric(colsampleBytreeValue)
  }
  if(!is.null(numLeavesValue)) {
    params$num_leaves <- as.integer(round(as.numeric(numLeavesValue)))
  }
  if(!is.null(minDataInLeafValue)) {
    params$min_data_in_leaf <- as.integer(round(as.numeric(minDataInLeafValue)))
  }
  if(!is.null(scalePosWeight)) {
    params$scale_pos_weight <- scalePosWeight
  }
  params
}
####
## Fim
#

