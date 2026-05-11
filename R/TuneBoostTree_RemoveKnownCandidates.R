#' Remover candidatos já avaliados
#' @noRd

TuneBoostTree_RemoveKnownCandidates <- function(pool, history, parameterNames) {

  if(is.null(history) || nrow(history) == 0L) {
    return(pool)
  }
  poolKey <- do.call(paste, c(TuneBoostTree_NormalizeParams(pool, parameterNames), sep = "|"))
  historyKey <- do.call(
    paste,
    c(TuneBoostTree_NormalizeParams(history, parameterNames), sep = "|")
  )
  pool[!(poolKey %in% historyKey), parameterNames, drop = FALSE]
}
####
## Fim
#

