#' Validar e limitar candidato do otimizador
#' @noRd

TuneBoostTree_ValidateCandidate <- function(candidate, bounds) {

  parameterNames <- names(bounds)
  candidate <- as.data.frame(candidate, stringsAsFactors = FALSE)
  missingNames <- setdiff(parameterNames, names(candidate))
  if(length(missingNames) > 0L) {
    cli::cli_abort("Optimizer candidate is missing required column(s): {TuneBoostTree_FormatQuotedNames(missingNames)}.")
  }
  candidate <- candidate[, parameterNames, drop = FALSE]
  for(parameterName in parameterNames) {
    value <- as.numeric(candidate[[parameterName]])
    if(anyNA(value) || any(!is.finite(value))) {
      cli::cli_abort("Optimizer candidate column '{parameterName}' contains non-finite value(s).")
    }
    lower <- as.numeric(bounds[[parameterName]][1L])
    upper <- as.numeric(bounds[[parameterName]][2L])
    candidate[[parameterName]] <- pmin(pmax(value, lower), upper)
  }
  TuneBoostTree_NormalizeParams(candidate, parameterNames)
}
####
## Fim
#

