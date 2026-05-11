#' Predizer com modelo boosted tree
#'
#' @param modelObj Objeto de modelo retornado por [FitBoostTreeModel()].
#' @param newdata Novo data.frame contendo todas as colunas preditoras.
#' @param threshold Limiar de probabilidade da classe positiva. Quando `NULL`,
#'   usa `modelObj$threshold` se existir; caso contrário, usa `0.5`.
#' @param engine Sobrescrita opcional da engine; por padrão usa
#'   `modelObj$engine`.
#'
#' @details Despacha a predição conforme a engine armazenada e retorna classes
#'   preditas e probabilidades das duas classes.
#'
#' @return Tibble com `predictedClass`, `probabilityFirstClass` e
#'   `probabilitySecondClass`. A segunda probabilidade corresponde à classe
#'   positiva armazenada no modelo.
#' @export

PredictBoostTreeModel <- function(modelObj, newdata, threshold = NULL, engine = NULL) {

  if(!is.data.frame(newdata) || nrow(newdata) == 0L) {
    cli::cli_abort("`newdata` must be a non-empty data.frame.")
  }

  if(is.null(threshold)) {
    if(!is.null(modelObj$threshold)) {
      threshold <- modelObj$threshold
    } else {
      threshold <- 0.5
    }
  }

  threshold <- as.numeric(threshold)

  if(length(threshold) != 1L || is.na(threshold) || threshold <= 0 || threshold >= 1) {
    cli::cli_abort("`threshold` must be between 0 and 1.")
  }

  if(is.null(engine)) {
    engine <- modelObj$engine
  }

  if(!(engine %in% c("xgboost", "lightgbm"))) {
    cli::cli_abort("Model engine must be 'xgboost' or 'lightgbm'.")
  }

  featureNames <- modelObj$featureNames
  missingFeatureNames <- setdiff(featureNames, names(newdata))

  if(length(missingFeatureNames) > 0L) {
    cli::cli_abort("`newdata` is missing required predictors: {TuneBoostTree_FormatQuotedNames(missingFeatureNames)}")
  }

  newdataFrame <- as.data.frame(newdata)
  numericMatrix <- data.matrix(newdataFrame[, featureNames, drop = FALSE])
  storage.mode(numericMatrix) <- "double"
  colnames(numericMatrix) <- featureNames

  if(engine == "xgboost") {
    if(is.null(modelObj$params$nthread)) {
      nThreads <- 1L
    } else {
      nThreads <- as.integer(modelObj$params$nthread)
    }

    predictionObject <- TuneBoostTree_CreateDataObject(
      xMatrix = numericMatrix,
      yData = NULL,
      featureTypes = modelObj$featureTypes,
      nThreads = nThreads,
      engine = "xgboost"
    )

    probabilitySecondClass <- as.numeric(
      stats::predict(modelObj$model, newdata = predictionObject)
    )
  } else {
    probabilitySecondClass <- as.numeric(stats::predict(modelObj$model, data = numericMatrix))
  }

  probabilityFirstClass <- 1 - probabilitySecondClass
  predictedClass <- ifelse(
    probabilitySecondClass >= threshold,
    modelObj$targetLevels[2L],
    modelObj$targetLevels[1L]
  )

  out <- tibble::tibble(
    predictedClass = predictedClass,
    probabilityFirstClass = probabilityFirstClass,
    probabilitySecondClass = probabilitySecondClass
  )

  attr(out, "targetName") <- modelObj$targetName
  attr(out, "targetLevels") <- modelObj$targetLevels

  out
}
####
## Fim
#

