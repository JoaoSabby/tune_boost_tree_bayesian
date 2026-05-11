#' Calculate PR AUC
#'
#' @param actual Rótulos inteiros 0/1.
#' @param predicted Numeric positive-class probabilities.
#'
#' @details AUC precisão-revocação trapezoidal interna compatível com a implementação atual.
#'
#' @return Numeric PR-AUC value.
#' @noRd

TuneBoostTree_CalculatePrAuc <- function(actual, predicted, backend = "auto") {

  backend <- TuneBoostTree_SelectPrAucBackend(backend)
  actual <- as.integer(actual)
  predicted <- as.numeric(predicted)
  if(length(actual) != length(predicted) || length(actual) == 0L) {
    return(NA_real_)
  }
  if(anyNA(actual) || anyNA(predicted) || any(!is.finite(predicted))) {
    return(NA_real_)
  }
  positiveCount <- sum(actual == 1L)
  if(positiveCount == 0L) {
    return(NA_real_)
  }
  if(identical(backend, "c")) {
    return(TuneBoostTree_CalculatePrAucC(actual, predicted))
  }
  if(identical(backend, "fortran")) {
    return(TuneBoostTree_CalculatePrAucFortran(actual, predicted))
  }
  if(identical(backend, "rfast")) {
    return(TuneBoostTree_CalculatePrAucRfast(actual, predicted, positiveCount))
  }
  TuneBoostTree_CalculatePrAucR(actual, predicted, positiveCount)
}
####
## Fim
#


