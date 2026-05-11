#' Otimizar limiar de classificação binária
#' @noRd

TuneBoostTree_OptimizeThreshold <- function(actual, predicted) {

  actual <- as.integer(actual)
  predicted <- as.numeric(predicted)
  valid <- is.finite(predicted) & !is.na(actual)
  actual <- actual[valid]
  predicted <- predicted[valid]
  if(length(actual) == 0L || length(unique(actual)) < 2L) {
    return(
      list(
        threshold = 0.5,
        metric = "f1",
        score = NA_real_
      )
    )
  }
  thresholds <- sort(unique(predicted))
  candidates <- unique(
    pmin(pmax(c(0.5, thresholds), .Machine$double.eps), 1 - .Machine$double.eps)
  )
  scores <- vapply(
    candidates,
    function(threshold) TuneBoostTree_F1Score(actual, predicted >= threshold),
    numeric(1L)
  )
  bestIndex <- which.max(scores)
  list(
    threshold = as.numeric(candidates[bestIndex]),
    metric = "f1",
    score = as.numeric(scores[bestIndex])
  )
}
####
## Fim
#

