#' Resolver estado inicial do otimizador
#' @noRd

TuneBoostTree_ResolveInitial <- function(initial, bounds) {

  if(is.null(initial)) {
    return(
      list(
        initGridDt = NULL,
        initPoints = 0L
      )
    )
  }
  if(is.list(initial) && !is.data.frame(initial) && !is.null(initial$initial)) {
    initial <- initial$initial
  }
  if(is.data.frame(initial)) {
    return(
      list(
        initGridDt = TuneBoostTree_DeduplicateInitGrid(initial, bounds),
        initPoints = 0L
      )
    )
  }
  if(is.numeric(initial) && length(initial) == 1L && is.finite(initial) && initial >= 0) {
    return(
      list(
        initGridDt = NULL,
        initPoints = as.integer(initial)
      )
    )
  }
  cli::cli_abort("`initial` must be `NULL`, a non-negative integer, or a data.frame/tibble/data.table warm-start grid.")
}
####
## Fim
#

