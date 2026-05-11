#' Executar um fold de predição por identificador
#' @noRd

TuneBoostTree_RunFoldPredictionById <- function(
  foldId,
  balancedFolds,
  hyperparameters,
  nRounds,
  seed,
  nThreads,
  evalMetric,
  engine,
  prAucBackend = "auto"
) {

  TuneBoostTree_RunOneFoldPrediction(
    foldData = balancedFolds[[foldId]],
    hyperparameters = hyperparameters,
    nRounds = nRounds,
    seed = seed + foldId,
    nThreads = nThreads,
    evalMetric = evalMetric,
    engine = engine,
    prAucBackend = prAucBackend
  )
}
####
## Fim
#

