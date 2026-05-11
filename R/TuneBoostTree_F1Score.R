#' Calculate F1 Score
#' @noRd

TuneBoostTree_F1Score <- function(actual, predictedClass) {

  tp <- sum(actual == 1L & predictedClass)
  fp <- sum(actual == 0L & predictedClass)
  fn <- sum(actual == 1L & !predictedClass)
  if((tp + fp) == 0L) {
    precision <- 0
  } else {
    precision <- tp / (tp + fp)
  }
  if((tp + fn) == 0L) {
    recall <- 0
  } else {
    recall <- tp / (tp + fn)
  }
  if((precision + recall) == 0) {
    return(0)
  }
  2 * precision * recall / (precision + recall)
}
####
## Fim
#

