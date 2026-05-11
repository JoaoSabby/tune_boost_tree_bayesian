#' Verificar resultado inválido de fold
#' @noRd

TuneBoostTree_IsInvalidFoldResult <- function(foldResult) {

  if(inherits(foldResult, "try-error") || inherits(foldResult, "error")) {
    return(TRUE)
  }

  if(!is.list(foldResult)) {
    return(TRUE)
  }

  missingNames <- setdiff(c("score", "bestIteration"), names(foldResult))
  if(length(missingNames) > 0L) {
    return(TRUE)
  }

  if(!is.null(foldResult$errorMessage)) {
    return(TRUE)
  }

  !is.finite(as.numeric(foldResult$score)[1L]) ||
    !is.finite(as.integer(foldResult$bestIteration)[1L])
}
####
## Fim
#

