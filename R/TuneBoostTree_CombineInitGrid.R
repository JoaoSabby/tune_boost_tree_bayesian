#' Combinar grades iniciais antigas e novas
#' @noRd

TuneBoostTree_CombineInitGrid <- function(oldInitGrid, newInitGrid, bounds) {

  if(is.null(oldInitGrid) || nrow(oldInitGrid) == 0L) {
    return(TuneBoostTree_DeduplicateInitGrid(newInitGrid, bounds))
  }
  if(is.null(newInitGrid) || nrow(newInitGrid) == 0L) {
    return(TuneBoostTree_DeduplicateInitGrid(oldInitGrid, bounds))
  }

  commonNames <- union(names(oldInitGrid), names(newInitGrid))
  oldData <- as.data.frame(oldInitGrid, stringsAsFactors = FALSE)
  newData <- as.data.frame(newInitGrid, stringsAsFactors = FALSE)
  for(name in setdiff(commonNames, names(oldData))) {
    oldData[[name]] <- NA
  }
  for(name in setdiff(commonNames, names(newData))) {
    newData[[name]] <- NA
  }

  TuneBoostTree_DeduplicateInitGrid(
    rbind(oldData[, commonNames, drop = FALSE], newData[, commonNames, drop = FALSE]),
    bounds
  )
}
####
## Fim
#

