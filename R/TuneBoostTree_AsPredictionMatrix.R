#' Preparar matriz de predição preservando representação esparsa
#' @noRd

TuneBoostTree_AsPredictionMatrix <- function(xMatrix) {

  if(inherits(xMatrix, "sparseMatrix")) {
    return(xMatrix)
  }
  as.matrix(xMatrix)
}
####
## Fim
#

