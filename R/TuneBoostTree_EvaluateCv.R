#' Avaliar um conjunto Bayesiano de parâmetros
#'
#' @param learn_rate Learning-rate candidate.
#' @param tree_depth Depth candidate.
#' @param min_n Minimum node-size candidate.
#' @param sample_size Row-sampling candidate.
#' @param mtry Predictor-sampling fraction candidate.
#' @param loss_reduction Split-gain candidate.
#' @param max_bin Histogram-bin candidate.
#'
#' @details Objetivo de topo usado pelo otimizador; seu ambiente é religado
#'   por `TuneBoostTree()` ao estado local da chamada.
#'
#' @return Lista com `Score` e `Pred` para adaptadores de otimizador.
#' @noRd

TuneBoostTree_EvaluateCv <- function(...) {

  hyperparameters <- list(...)
  hyperparameters <- hyperparameters[parameterNames]
  fixedBoostNames <- setdiff(
    names(boost)[!vapply(boost, is.null, logical(1L))],
    c("trees", "stop_iter")
  )
  for(fixedName in setdiff(fixedBoostNames, names(hyperparameters))) {
    hyperparameters[[fixedName]] <- boost[[fixedName]]
  }
  normalizedData <- TuneBoostTree_NormalizeParams(
    as.data.frame(hyperparameters[parameterNames], stringsAsFactors = FALSE),
    parameterNames
  )
  hyperparameters <- as.list(normalizedData[1L, parameterNames, drop = FALSE])
  for(fixedName in setdiff(fixedBoostNames, names(hyperparameters))) {
    hyperparameters[[fixedName]] <- boost[[fixedName]]
  }
  formattedParameterValues <- format(
    unlist(normalizedData[1L, parameterNames, drop = FALSE], use.names = FALSE),
    digits = 17L
  )

  cacheKey <- paste(
    paste(parameterNames, formattedParameterValues, sep = "="),
    collapse = "|"
  )
  if(exists(cacheKey, envir = cacheEnv, inherits = FALSE)) {
    cachedResult <- get(cacheKey, envir = cacheEnv, inherits = FALSE)
    return(
      list(
        Score = as.numeric(cachedResult$score),
        Pred = 0
      )
    )
  }
  cvSummary <- TuneBoostTree_RunCvManual(
    balancedFolds,
    hyperparameters,
    nRoundsTuning,
    earlyStoppingRounds,
    seed,
    workerThreads,
    nWorkersFolds,
    evalMetric,
    engine,
    prAucBackend
  )
  scoreValue <- as.numeric(cvSummary$score)
  bestIteration <- as.integer(cvSummary$bestIteration)
  logIndex <<- logIndex + 1L
  evaluationLogList[[logIndex]] <<- data.frame(
    normalizedData[1L, parameterNames, drop = FALSE],
    Value = scoreValue,
    bestIteration = bestIteration,
    stringsAsFactors = FALSE
  )

  assign(
    x = cacheKey,
    value = list(score = scoreValue, bestIteration = bestIteration),
    envir = cacheEnv
  )

  list(Score = scoreValue, Pred = 0)
}
####
## Fim
#

