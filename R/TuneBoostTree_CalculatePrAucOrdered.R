#' Acumular PR-AUC ordenada
#' @noRd

TuneBoostTree_CalculatePrAucOrdered <- function(actualOrd, positiveCount) {

  tp <- cumsum(actualOrd == 1L)
  fp <- cumsum(actualOrd == 0L)
  precision <- c(1, tp / pmax(tp + fp, 1))
  recall <- c(0, tp / positiveCount)
  sum((recall[-1L] - recall[-length(recall)]) * precision[-1L])
}
####
## Fim
#

