#' Detectar colunas com perfil esparso
#' @noRd

TuneBoostTree_IsSparseLikeColumn <- function(column) {

  any(c("sparsevctrs_vctr", "sparse_vector", "sparse_double", "sparse_integer") %in% class(column))
}
####
## Fim
#

