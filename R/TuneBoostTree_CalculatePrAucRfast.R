#' Calcular PR-AUC com ordenação Rfast
#' @noRd

TuneBoostTree_CalculatePrAucRfast <- function(actual, predicted, positiveCount = sum(actual == 1L)) {

  orderIndex <- Rfast::Order(as.numeric(predicted), stable = TRUE, descending = TRUE)
  TuneBoostTree_CalculatePrAucOrdered(actual[orderIndex], positiveCount)
}
####
## Fim
#

