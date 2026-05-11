#' Extrair metadados da fórmula
#'
#' @param formula Fórmula de modelo de duas faces.
#' @param data data.frame contendo todas as colunas referenciadas.
#'
#' @details Auxiliar interno que centraliza a análise da fórmula para que
#'   preparação de matriz e predição usem a mesma ordem de features.
#'
#' @return Lista com nome do alvo, nomes dos preditores e objeto de termos.
#' @noRd

TuneBoostTree_ExtractFormulaInfo <- function(formula, data) {

  targetName <- all.vars(formula[[2L]])[1L]
  termsValue <- terms(formula, data = data)
  predictorNames <- attr(termsValue, "term.labels")
  list(
    targetName = targetName,
    predictorNames = predictorNames,
    termsValue = termsValue
  )
}
####
## Fim
#

