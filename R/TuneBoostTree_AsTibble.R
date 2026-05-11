#' Converter tabelas retornadas em tibbles
#' @noRd

TuneBoostTree_AsTibble <- function(x) {

  if(is.null(x)) {
    return(NULL)
  }
  tibble::as_tibble(x)
}
####
## Fim
#

