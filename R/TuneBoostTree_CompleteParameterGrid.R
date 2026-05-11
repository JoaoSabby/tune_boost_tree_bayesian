#' Complete Parameter Grid Columns
#' @noRd

TuneBoostTree_CompleteParameterGrid <- function(gridData, bounds) {

  gridData <- as.data.frame(gridData, stringsAsFactors = FALSE)
  for(parameterName in names(bounds)) {
    if(!parameterName %in% names(gridData)) {
      gridData[[parameterName]] <- mean(as.numeric(bounds[[parameterName]]))
    }
  }
  gridData
}
####
## Fim
#

