#' Criar grade inicial a partir do log de avaliações
#' @noRd

TuneBoostTree_CreateInitGrid <- function(evaluationLog, bounds) {

  if(is.null(evaluationLog) || nrow(evaluationLog) == 0L) {
    return(NULL)
  }
  TuneBoostTree_DeduplicateInitGrid(evaluationLog, bounds)
}
####
## Fim
#

