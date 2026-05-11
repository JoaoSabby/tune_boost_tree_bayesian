#' Encontrar melhor iteração associada ao melhor score
#' @noRd

TuneBoostTree_FindBestIteration <- function(evaluationLog, hyperparameters, bestScore, bounds) {

  if(is.null(evaluationLog) || nrow(evaluationLog) == 0L || is.null(bounds)) {
    return(NULL)
  }

  parameterNames <- names(bounds)
  missingNames <- setdiff(c(parameterNames, "Value", "bestIteration"), names(evaluationLog))
  if(length(missingNames) > 0L) {
    return(NULL)
  }

  scoreMatches <- vapply(
    evaluationLog$Value,
    TuneBoostTree_IsScoreMatch,
    logical(1L),
    scoreB = bestScore
  )
  if(!any(scoreMatches)) {
    return(NULL)
  }

  parameterData <- as.data.frame(hyperparameters[parameterNames], stringsAsFactors = FALSE)
  parameterData <- TuneBoostTree_NormalizeParams(parameterData, parameterNames)
  logData <- TuneBoostTree_NormalizeParams(evaluationLog[, parameterNames, drop = FALSE], parameterNames)
  parameterMatches <- rep(TRUE, nrow(logData))
  for(parameterName in parameterNames) {
    parameterMatches <- parameterMatches & logData[[parameterName]] == parameterData[[parameterName]][[1L]]
  }

  candidateRows <- which(scoreMatches & parameterMatches & is.finite(evaluationLog$bestIteration))
  if(length(candidateRows) == 0L) {
    candidateRows <- which(scoreMatches & is.finite(evaluationLog$bestIteration))
  }
  if(length(candidateRows) == 0L) {
    return(NULL)
  }

  bestIteration <- as.integer(round(evaluationLog$bestIteration[[candidateRows[[1L]]]]))
  if(is.na(bestIteration) || bestIteration < 1L) {
    return(NULL)
  }
  bestIteration
}
####
## Fim
#

