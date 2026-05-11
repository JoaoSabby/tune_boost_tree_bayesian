#' Ler hiperparâmetro opcional
#' @noRd

TuneBoostTree_GetHyperparameter <- function(hyperparameters, parameterName, default = NULL) {

  if(!parameterName %in% names(hyperparameters) || is.null(hyperparameters[[parameterName]])) {
    return(default)
  }
  value <- as.numeric(hyperparameters[[parameterName]])[1L]
  if(!is.finite(value)) {
    return(default)
  }
  value
}
####
## Fim
#

