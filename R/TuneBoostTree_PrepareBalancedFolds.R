#' Preparar folds balanceados
#'
#' @param formula Fórmula de duas faces.
#' @param data Full training data.frame.
#' @param nFolds Inteiro com número de folds.
#' @param balanceFn Função aplicada uma vez a cada partição de treino.
#' @param balanceArgs Argumentos extras repassados apenas para `balanceFn`.
#' @param scalePosWeightSetting Política de peso do fold: numérica, `"auto"` ou `NULL`.
#' @param nThreads Inteiro com threads para construção dos dados da engine.
#' @param seed Inteiro usado como semente aleatória.
#' @param engine Nome da engine, `"xgboost"` ou `"lightgbm"`.
#'
#' @details Aplica balanceamento uma vez por fold e armazena matrizes R de
#'   treino e teste para que cada worker crie seu próprio objeto nativo da
#'   engine.
#'
#' @return Lista de objetos de fold com matrizes R, rótulos, pesos e metadados.
#' @noRd

TuneBoostTree_PrepareBalancedFolds <- function(
  formula,
  data,
  nFolds,
  balanceFn,
  balanceArgs = list(),
  scalePosWeightSetting = "auto",
  nThreads = 1L,
  seed = 42L,
  engine = "xgboost",
  targetLevels = NULL
) {

  formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, data)
  dataFrame <- as.data.frame(data)
  preparedTarget <- TuneBoostTree_PrepareTarget(
    dataFrame[[formulaInfo$targetName]],
    targetLevels
  )
  folds <- TuneBoostTree_CreateStratifiedFolds(preparedTarget$yData, nFolds, seed)
  balancedFolds <- vector("list", length(folds))
  for(foldId in seq_along(folds)) {
    testIndex <- folds[[foldId]]
    trainData <- data[setdiff(seq_len(nrow(data)), testIndex), , drop = FALSE]
    testData <- data[testIndex, , drop = FALSE]
    balancedTrain <- do.call(
      balanceFn,
      c(list(data = trainData, formula = formula), balanceArgs)
    )
    preparedTrain <- TuneBoostTree_PrepareMatrix(
      formula,
      balancedTrain,
      NULL,
      preparedTarget$targetLevels,
      formulaInfo
    )
    preparedTest <- TuneBoostTree_PrepareMatrix(
      formula,
      testData,
      NULL,
      preparedTrain$targetLevels,
      formulaInfo
    )
    foldScalePosWeight <- TuneBoostTree_ResolveScalePosWeight(
      preparedTrain$yData,
      scalePosWeightSetting
    )

    balancedFolds[[foldId]] <- list(
      xTrain = preparedTrain$xMatrix,
      yTrain = preparedTrain$yData,
      xTest = preparedTest$xMatrix,
      yTest = preparedTest$yData,
      featureTypes = preparedTrain$featureTypes,
      scalePosWeight = foldScalePosWeight,
      featureNames = preparedTrain$featureNames,
      targetLevels = preparedTrain$targetLevels
    )
  }
  balancedFolds
}
####
## Fim
#

