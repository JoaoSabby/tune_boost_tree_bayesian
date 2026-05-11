#' Calcular PR-AUC com C compilado
#'
#' @noRd

TuneBoostTree_CalculatePrAucC <- function(actual, predicted) {

  as.numeric(.Call("tbtb_pr_auc_c", actual, predicted, PACKAGE = "TuneBoostTreeBayesian"))
}
####
## Fim
#

