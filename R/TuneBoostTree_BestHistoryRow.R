#' Selecionar melhor linha do histórico
#' @noRd

TuneBoostTree_BestHistoryRow <- function(history, parameterNames) {

  if(is.null(history) || nrow(history) == 0L || !any(is.finite(history$Value))) {
    return(
      list(
        Best_Par = NULL,
        Best_Value = -Inf
      )
    )
  }
  bestId <- which.max(history$Value)
  list(
    Best_Par = as.list(history[bestId, parameterNames, drop = FALSE]),
    Best_Value = as.numeric(history$Value[[bestId]])
  )
}
####
## Fim
#

