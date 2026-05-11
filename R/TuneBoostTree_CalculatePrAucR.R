#' Calcular PR-AUC com ordenação em R base
#' @noRd

TuneBoostTree_CalculatePrAucR <- function(actual, predicted, positiveCount = sum(actual == 1L)) {

  orderIndex <- order(predicted, decreasing = TRUE)
  TuneBoostTree_CalculatePrAucOrdered(actual[orderIndex], positiveCount)
}
####
## Fim
#

