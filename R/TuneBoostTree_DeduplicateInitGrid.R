#' Deduplicar grade inicial por parâmetros normalizados
#' @noRd

TuneBoostTree_DeduplicateInitGrid <- function(initGrid, bounds) {

  if(is.null(initGrid)) {
    return(NULL)
  }

  initGrid <- as.data.frame(initGrid, stringsAsFactors = FALSE)
  if(nrow(initGrid) == 0L) {
    return(initGrid)
  }

  parameterNames <- names(bounds)
  initGrid <- TuneBoostTree_CompleteParameterGrid(initGrid, bounds)
  extraNames <- setdiff(names(initGrid), parameterNames)
  parameterData <- TuneBoostTree_ValidateCandidate(initGrid[, parameterNames, drop = FALSE], bounds)
  out <- data.frame(parameterData, stringsAsFactors = FALSE)

  for(extraName in extraNames) {
    out[[extraName]] <- initGrid[[extraName]]
  }

  if(!"Value" %in% names(out)) {
    out$Value <- NA_real_
  }
  out$Value <- as.numeric(out$Value)

  if(nrow(out) == 0L) {
    return(out)
  }

  key <- do.call(
    paste,
    c(lapply(parameterNames, function(parameterName) out[[parameterName]]), sep = "\r")
  )
  orderValue <- ifelse(is.finite(out$Value), out$Value, -Inf)
  keepRows <- unlist(
    tapply(
      seq_len(nrow(out)),
      key,
      function(rowIds) rowIds[which.max(orderValue[rowIds])][[1L]]
    ),
    use.names = FALSE
  )

  out[sort(keepRows), , drop = FALSE]
}
####
## Fim
#

