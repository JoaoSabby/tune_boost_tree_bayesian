#' Executar otimizador interno seguro
#' @noRd

TuneBoostTree_RunInternalOptimizer <- function(
  objective,
  bounds,
  initGridDt = NULL,
  initPoints = 10L,
  nIter = 30L,
  seed = 42L,
  acq = "ucb",
  kappa = 2.576,
  eps = 0
) {

  set.seed(as.integer(seed))
  parameterNames <- names(bounds)
  history <- TuneBoostTree_EvaluateInitialCandidates(objective, bounds, initGridDt, initPoints)
  best <- TuneBoostTree_BestHistoryRow(history, parameterNames)
  if(!is.finite(best$Best_Value)) {
    cli::cli_abort("All initial optimizer candidate evaluations failed or returned non-finite scores.")
  }
  for(iteration in seq_len(as.integer(nIter))) {
    candidate <- TuneBoostTree_ProposeInternalBayesianCandidate(
      history,
      bounds,
      acq,
      kappa,
      eps,
      seed + iteration
    )
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
  list(
    Best_Par = best$Best_Par,
    Best_Value = best$Best_Value,
    History = history
  )
}
####
## Fim
#

