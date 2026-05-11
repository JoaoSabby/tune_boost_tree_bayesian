#' Resolver peso da classe positiva
#' @noRd

TuneBoostTree_ResolveScalePosWeight <- function(yData, scale_pos_weight) {

  if(is.null(scale_pos_weight)) {
    return(NULL)
  }
  if(is.character(scale_pos_weight) && identical(scale_pos_weight, "auto")) {
    classCounts <- table(as.integer(yData))
    if(length(classCounts) != 2L || any(classCounts == 0L)) {
      return(NULL)
    }
    return(as.numeric(classCounts[["0"]] / classCounts[["1"]]))
  }
  as.numeric(scale_pos_weight)[1L]
}
####
## Fim
#



