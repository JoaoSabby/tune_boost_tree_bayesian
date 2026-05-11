#' Normalizar parâmetros
#'
#' @param parameterData data.frame of candidate parameters.
#' @param parameterNames Nomes ordenados dos parâmetros a normalizar.
#'
#' @details Arredonda parâmetros inteiros e estabiliza valores contínuos para chaves de cache e comparação.
#'
#' @return data.frame normalizado.
#' @noRd

TuneBoostTree_NormalizeParams <- function(parameterData, parameterNames) {

  parameterData <- as.data.frame(parameterData, stringsAsFactors = FALSE)
  parameterData <- parameterData[, parameterNames, drop = FALSE]
  for(parameterName in parameterNames) {
    parameterData[[parameterName]] <- as.numeric(parameterData[[parameterName]])
  }
  integerParameters <- intersect(
    c("tree_depth", "min_n", "max_bin", "num_leaves", "min_data_in_leaf"),
    parameterNames
  )
  for(parameterName in integerParameters) {
    parameterData[[parameterName]] <- as.integer(round(parameterData[[parameterName]]))
  }
  for(parameterName in setdiff(parameterNames, integerParameters)) {
    parameterData[[parameterName]] <- round(parameterData[[parameterName]], digits = 12L)
  }
  parameterData
}
####
## Fim
#

