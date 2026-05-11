#' Compare Scores
#'
#' @param scoreA First numeric score.
#' @param scoreB Second numeric score.
#' @param tolerance Relative tolerance for equality.
#'
#' @details Auxiliar interno para relacionar melhores valores do otimizador ao log de avaliação.
#'
#' @return Logical scalar.
#' @noRd

TuneBoostTree_IsScoreMatch <- function(scoreA, scoreB, tolerance = 1e-6) {

  scoreA <- as.numeric(scoreA)
  scoreB <- as.numeric(scoreB)
  is.finite(scoreA) && is.finite(scoreB) && abs(scoreA - scoreB) <= tolerance * max(1, abs(scoreA), abs(scoreB))
}
####
## Fim
#


