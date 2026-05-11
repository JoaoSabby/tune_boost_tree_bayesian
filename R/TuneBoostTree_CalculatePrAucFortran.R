#' Calcular PR-AUC com Fortran compilado
#' @noRd

TuneBoostTree_CalculatePrAucFortran <- function(actual, predicted) {

  out <- .Fortran(
    "tbtb_pr_auc_f",
    n = as.integer(length(actual)),
    actual = as.integer(actual),
    predicted = as.double(predicted),
    score = as.double(NA_real_),
    PACKAGE = "TuneBoostTreeBayesian"
  )
  as.numeric(out$score)
}
####
## Fim
#

