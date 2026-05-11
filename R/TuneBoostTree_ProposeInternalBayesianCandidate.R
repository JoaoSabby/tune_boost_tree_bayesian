#' Propor candidato com aquisição gaussiana leve
#' @noRd

TuneBoostTree_ProposeInternalBayesianCandidate <- function(
  history,
  bounds,
  acq = "ucb",
  kappa = 2.576,
  eps = 0,
  seed = 42L
) {

  if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    oldSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    oldSeed <- NULL
  }
  on.exit({
    if(is.null(oldSeed)) {
      if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", oldSeed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  parameterNames <- names(bounds)
  poolSize <- max(512L, min(8192L, 1024L * length(parameterNames)))
  pool <- TuneBoostTree_SampleCandidates(bounds, poolSize)
  pool <- TuneBoostTree_RemoveKnownCandidates(pool, history, parameterNames)
  if(nrow(pool) == 0L) {
    pool <- TuneBoostTree_SampleCandidates(bounds, poolSize)
  }
  if(nrow(history) < max(4L, length(parameterNames) + 1L)) {
    return(pool[1L, parameterNames, drop = FALSE])
  }
  score <- TuneBoostTree_AcquisitionScores(history, pool, bounds, acq, kappa, eps)
  pool[which.max(score), parameterNames, drop = FALSE]
}
####
## Fim
#

