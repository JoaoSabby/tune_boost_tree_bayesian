#' Executar predições de CV sem parada antecipada
#' @noRd

TuneBoostTree_RunCvPredictions <- function(
  balancedFolds,
  hyperparameters,
  nRounds,
  seed,
  nThreads,
  nWorkersFolds,
  evalMetric,
  engine,
  prAucBackend = "auto"
) {

  nWorkers <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds))
  workerThreads <- max(1L, as.integer(nThreads))
  foldIds <- seq_along(balancedFolds)
  TuneBoostTree_SetPassiveOpenMp()
  if(nWorkers == 1L) {
    foldResults <- lapply(
      X = foldIds,
      FUN = function(i) {
        TuneBoostTree_RunOneFoldPrediction(
          foldData = balancedFolds[[i]],
          hyperparameters = hyperparameters,
          nRounds = nRounds,
          seed = seed + i,
          nThreads = workerThreads,
          evalMetric = evalMetric,
          engine = engine,
          prAucBackend = prAucBackend
        )
      }
    )
  } else if(.Platform$OS.type == "windows") {
    cluster <- parallel::makeCluster(nWorkers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)

    foldResults <- parallel::parLapply(
      cl = cluster,
      X = foldIds,
      fun = TuneBoostTree_RunFoldPredictionById,
      balancedFolds = balancedFolds,
      hyperparameters = hyperparameters,
      nRounds = nRounds,
      seed = seed,
      nThreads = workerThreads,
      evalMetric = evalMetric,
      engine = engine,
      prAucBackend = prAucBackend
    )
  } else {
    foldResults <- parallel::mclapply(
      X = foldIds,
      FUN = TuneBoostTree_RunFoldPredictionById,
      balancedFolds = balancedFolds,
      hyperparameters = hyperparameters,
      nRounds = nRounds,
      seed = seed,
      nThreads = workerThreads,
      evalMetric = evalMetric,
      engine = engine,
      prAucBackend = prAucBackend,
      mc.cores = nWorkers,
      mc.set.seed = FALSE
    )
  }
  invalidFold <- vapply(foldResults, TuneBoostTree_IsInvalidFoldPredictionResult, logical(1L))

  if(any(invalidFold)) {
    failedFolds <- paste(foldIds[invalidFold], collapse = ", ")
    cli::cli_abort("Validation prediction fold(s) failed before returning predictions: {failedFolds}.")
  }

  list(
    actual = unlist(lapply(foldResults, `[[`, "actual"), use.names = FALSE),
    predicted = unlist(lapply(foldResults, `[[`, "predicted"), use.names = FALSE)
  )
}
####
## Fim
#

