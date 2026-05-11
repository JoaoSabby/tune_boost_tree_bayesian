#' Executar otimizador Limbo ask/tell
#' @noRd

TuneBoostTree_RunLimboOptimizer <- function(
  objective,
  bounds,
  initGridDt = NULL,
  initPoints = 10L,
  nIter = 30L,
  acq = "ucb",
  kappa = 2.576,
  eps = 0,
  seed = 42L,
  limboCommand = NA_character_
) {

  set.seed(as.integer(seed))
  parameterNames <- names(bounds)
  history <- TuneBoostTree_EvaluateInitialCandidates(objective, bounds, initGridDt, initPoints)
  best <- TuneBoostTree_BestHistoryRow(history, parameterNames)
  for(iteration in seq_len(as.integer(nIter))) {
    candidate <- TuneBoostTree_RequestLimboCandidate(
      limboCommand,
      bounds,
      history,
      acq,
      kappa,
      eps,
      seed,
      iteration
    )
    candidate <- TuneBoostTree_ValidateCandidate(candidate, bounds)
    value <- as.numeric(do.call(objective, as.list(candidate[1L, parameterNames, drop = FALSE]))$Score)[1L]
    if(is.finite(value)) {
      history <- rbind(
        history,
        data.frame(candidate[1L, parameterNames, drop = FALSE], Value = value, stringsAsFactors = FALSE)
      )
      if(value > best$Best_Value) {
        best <- list(
          Best_Par = as.list(candidate[1L, parameterNames, drop = FALSE]),
          Best_Value = value
        )
      }
    }
  }
  if(!is.finite(best$Best_Value)) {
    cli::cli_abort("Limbo did not produce any finite optimizer score.")
  }
  list(
    Best_Par = best$Best_Par,
    Best_Value = best$Best_Value,
    History = history
  )
}
####
## Fim
#

