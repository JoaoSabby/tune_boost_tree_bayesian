#' Avaliar performance de modelo boosted tree
#'
#' @param modelObj Objeto de modelo retornado por [FitBoostTreeModel()].
#' @param testData data.frame de teste contendo preditores e variável resposta.
#' @param formula Fórmula de duas faces que identifica a variável resposta.
#'
#' @details Chama [PredictBoostTreeModel()] internamente e calcula PR-AUC e uma
#'   tabela de confusão resumida.
#'
#' @return Lista com `prAuc`, `confusionSummary` e `predictions`.
#'   `confusionSummary` é uma tibble com colunas `actual`, `predicted` e
#'   `count`; `predictions` é o tibble retornado por [PredictBoostTreeModel()].
#' @export

PerformanceBoostTreeModel <- function(modelObj, testData, formula) {

  predictions <- PredictBoostTreeModel(modelObj, testData)
  formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, testData)
  preparedTarget <- TuneBoostTree_PrepareTarget(
    testData[[formulaInfo$targetName]],
    modelObj$targetLevels
  )
  prAuc <- TuneBoostTree_CalculatePrAuc(
    preparedTarget$yData,
    predictions$probabilitySecondClass
  )
  confusionTable <- table(
    actual = testData[[formulaInfo$targetName]],
    predicted = predictions$predictedClass
  )
  confusionSummary <- tibble::as_tibble(
    as.data.frame(confusionTable, stringsAsFactors = FALSE)
  )
  names(confusionSummary) <- c("actual", "predicted", "count")
  list(
    prAuc = prAuc,
    confusionSummary = confusionSummary,
    predictions = predictions
  )
}
####
## Fim
#
