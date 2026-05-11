#' Verificar resultado inválido de fold de predição
#' @noRd

TuneBoostTree_IsInvalidFoldPredictionResult <- function(foldResult) {

  if(inherits(foldResult, "try-error") || inherits(foldResult, "error")) {
    return(TRUE)
  }

  if(!is.list(foldResult)) {
    return(TRUE)
  }

  missingNames <- setdiff(c("actual", "predicted"), names(foldResult))
  length(missingNames) > 0L
}
####
## Fim
#

