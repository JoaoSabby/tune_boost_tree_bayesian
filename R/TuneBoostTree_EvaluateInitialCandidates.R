#' Avaliar candidatos iniciais e de aquecimento
#' @noRd

TuneBoostTree_EvaluateInitialCandidates <- function(objective, bounds, initGridDt = NULL, initPoints = 10L) {

  parameterNames <- names(bounds)
  candidates <- TuneBoostTree_SampleCandidates(bounds, as.integer(initPoints))
  if(!is.null(initGridDt) && nrow(initGridDt) > 0L) {
    candidates <- rbind(
      TuneBoostTree_ValidateCandidate(initGridDt[, parameterNames, drop = FALSE], bounds),
      candidates
    )
  }
  if(nrow(candidates) == 0L) {
    candidates <- TuneBoostTree_SampleCandidates(bounds, max(1L, 2L * length(parameterNames)))
  }
  values <- rep(NA_real_, nrow(candidates))
  for(rowId in seq_len(nrow(candidates))) {
    candidateScore <- do.call(
      objective,
      as.list(candidates[rowId, parameterNames, drop = FALSE])
    )$Score

    values[[rowId]] <- as.numeric(candidateScore)[1L]
  }
  out <- data.frame(
    candidates[, parameterNames, drop = FALSE],
    Value = values,
    stringsAsFactors = FALSE
  )
  out[is.finite(out$Value), , drop = FALSE]
}
####
## Fim
#

