#' Executar validação cruzada manual
#'
#' @param balancedFolds Lista de folds com matrizes R serializáveis.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Inteiro com limite de rodadas da tunagem.
#' @param earlyStoppingRounds Inteiro com paciência da parada antecipada.
#' @param seed Inteiro usado como semente aleatória.
#' @param nThreads Inteiro com threads por worker.
#' @param nWorkersFolds Inteiro com número de workers de folds.
#' @param evalMetric Nome da métrica do XGBoost.
#' @param engine Nome da engine, `"xgboost"` ou `"lightgbm"`.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Executa folds em cache sequencialmente ou com `parallel` base,
#'   limitando threads da engine para evitar sobrecarga de CPU.
#'
#' @return Lista com score médio, melhor iteração média e scores por fold.
#' @noRd

TuneBoostTree_RunCvManual <- function(
  balancedFolds,
  hyperparameters,
  nRounds,
  earlyStoppingRounds,
  seed,
  nThreads,
  nWorkersFolds,
  evalMetric,
  engine,
  prAucBackend = "auto"
) {

  nWorkers <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds))
  workerThreads <- max(1L, as.integer(nThreads))
  if(identical(engine, "lightgbm") && nWorkers > 1L) {
    nWorkers <- 1L
    workerThreads <- max(1L, as.integer(nThreads) * max(1L, as.integer(nWorkersFolds)))
  }
  foldIds <- seq_along(balancedFolds)
  TuneBoostTree_SetPassiveOpenMp()
  if(nWorkers == 1L) {
    foldResults <- vector("list", length(foldIds))

    for(i in foldIds) {
      foldResults[[i]] <- TuneBoostTree_RunFoldById(
        foldId = i,
        balancedFolds = balancedFolds,
        hyperparameters = hyperparameters,
        nRounds = nRounds,
        earlyStoppingRounds = earlyStoppingRounds,
        seed = seed,
        nThreads = workerThreads,
        evalMetric = evalMetric,
        engine = engine,
        prAucBackend = prAucBackend
      )
    }
  } else if(.Platform$OS.type == "windows") {
    cluster <- parallel::makeCluster(nWorkers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)

    foldResults <- parallel::parLapply(
      cl = cluster,
      X = foldIds,
      fun = TuneBoostTree_RunFoldById,
      balancedFolds = balancedFolds,
      hyperparameters = hyperparameters,
      nRounds = nRounds,
      earlyStoppingRounds = earlyStoppingRounds,
      seed = seed,
      nThreads = workerThreads,
      evalMetric = evalMetric,
      engine = engine,
      prAucBackend = prAucBackend
    )
  } else {
    foldResults <- parallel::mclapply(
      X = foldIds,
      FUN = TuneBoostTree_RunFoldById,
      balancedFolds = balancedFolds,
      hyperparameters = hyperparameters,
      nRounds = nRounds,
      earlyStoppingRounds = earlyStoppingRounds,
      seed = seed,
      nThreads = workerThreads,
      evalMetric = evalMetric,
      engine = engine,
      prAucBackend = prAucBackend,
      mc.cores = nWorkers,
      mc.set.seed = FALSE
    )
  }
  invalidFold <- vapply(foldResults, TuneBoostTree_IsInvalidFoldResult, logical(1L))

  if(any(invalidFold)) {
    failedFolds <- paste(foldIds[invalidFold], collapse = ", ")
    foldMessages <- vapply(
      foldResults[invalidFold],
      function(foldResult) {
        if(is.list(foldResult) && !is.null(foldResult$errorMessage)) {
          return(as.character(foldResult$errorMessage)[1L])
        }
        if(inherits(foldResult, "try-error") || inherits(foldResult, "error")) {
          return(as.character(foldResult)[1L])
        }
        "Unknown fold failure."
      },
      character(1L)
    )
    foldMessages <- unique(foldMessages[nzchar(foldMessages)])
    detail <- paste(utils::head(foldMessages, 3L), collapse = " | ")
    cli::cli_abort("Validation fold(s) failed before returning a score: {failedFolds}. First error(s): {detail}")
  }

  foldScores <- vapply(foldResults, `[[`, numeric(1L), "score")
  foldBestIter <- vapply(foldResults, `[[`, integer(1L), "bestIteration")

  if(anyNA(foldScores)) {
    cli::cli_abort(paste0(
      "At least one validation fold produced undefined PR-AUC; reduce `folds` ",
      "or provide more positive-class observations."
    ))
  }

  list(
    score = as.numeric(mean(foldScores)),
    bestIteration = as.integer(round(mean(foldBestIter))),
    foldScores = foldScores
  )
}
####
## Fim
#

