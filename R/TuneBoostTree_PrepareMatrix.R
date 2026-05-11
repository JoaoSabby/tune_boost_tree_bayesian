#' Preparar matriz numérica de preditores
#'
#' @param formula Fórmula de duas faces.
#' @param data data.frame contendo desfecho e preditores.
#' @param featureTypes Vetor opcional de tipos de features do XGBoost.
#' @param targetLevels Ordenação opcional dos níveis do alvo binário.
#' @param formulaInfo Metadados da fórmula já analisados por `TuneBoostTree_ExtractFormulaInfo`.
#'
#' @details Converte preditores numéricos para matriz double e usa
#'   armazenamento esparso apenas quando a entrada é altamente esparsa.
#'
#' @return Lista com matriz, alvo, metadados de features, classes e fórmula.
#' @noRd

TuneBoostTree_PrepareMatrix <- function(formula, data, featureTypes = NULL, targetLevels = NULL, formulaInfo = NULL) {

  if(is.null(formulaInfo)) {
    formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, data)
  }
  featureNames <- formulaInfo$predictorNames
  dataFrame <- as.data.frame(data)
  xData <- dataFrame[, featureNames, drop = FALSE]
  sparseLike <- vapply(xData, TuneBoostTree_IsSparseLikeColumn, logical(1L))
  if(any(sparseLike)) {
    sparseRatio <- 1
  } else {
    sparseRatio <- TuneBoostTree_EstimateSparseRatio(xData)
  }
  numericMatrix <- data.matrix(xData)
  storage.mode(numericMatrix) <- "double"
  colnames(numericMatrix) <- featureNames
  if(any(sparseLike) || sparseRatio > 0.7) {
    xMatrix <- Matrix::Matrix(numericMatrix, sparse = TRUE)
  } else {
    xMatrix <- numericMatrix
  }
  preparedTarget <- TuneBoostTree_PrepareTarget(
    dataFrame[[formulaInfo$targetName]],
    targetLevels
  )
  if(!is.null(featureTypes)) {
    names(featureTypes) <- featureNames
  }
  list(
    xMatrix = xMatrix,
    yData = preparedTarget$yData,
    featureNames = featureNames,
    featureTypes = featureTypes,
    targetLevels = preparedTarget$targetLevels,
    targetName = formulaInfo$targetName,
    negativeClass = preparedTarget$negativeClass,
    positiveClass = preparedTarget$positiveClass,
    formulaInfo = formulaInfo
  )
}
####
## Fim
#


