#' Núcleo exponencial quadrático
#' @noRd

TuneBoostTree_RbfKernel <- function(xA, xB, lengthScale) {

  xA <- as.matrix(xA)
  xB <- as.matrix(xB)
  scaledA <- sweep(xA, 2L, lengthScale, "/")
  scaledB <- sweep(xB, 2L, lengthScale, "/")
  dist2 <- outer(
    .rowSums(scaledA * scaledA, nrow(scaledA), ncol(scaledA)),
    .rowSums(scaledB * scaledB, nrow(scaledB), ncol(scaledB)),
    "+") - 2 * tcrossprod(scaledA,
    scaledB
  )
  exp(-0.5 * pmax(dist2, 0))
}
####
## Fim
#

