#' Preparar alvo binário
#'
#' @param targetData Vetor de desfecho dos dados de treino.
#' @param targetLevels Ordenação opcional de dois níveis, na qual o segundo nível é positivo.
#'
#' @details Auxiliar interno que mapeia rótulos binários para 0/1 preservando os nomes das classes para a predição.
#'
#' @return Lista com alvo numérico, níveis, classe negativa e classe positiva.
#' @noRd

TuneBoostTree_PrepareTarget <- function(targetData, targetLevels = NULL) {

  if(!is.factor(targetData)) {
    cli::cli_abort("The dependent/target variable must be a factor with exactly two levels.")
  }
  observedLevels <- levels(targetData)
  if(length(observedLevels) != 2L) {
    cli::cli_abort("The dependent/target factor must have exactly two levels.")
  }
  if(anyNA(targetData)) {
    cli::cli_abort("The dependent/target factor must not contain missing values.")
  }
  if(is.null(targetLevels)) {
    classCounts <- table(targetData)
    levelCounts <- as.integer(classCounts[observedLevels])
    if(any(levelCounts == 0L)) {
      cli::cli_abort("The dependent/target factor must contain observations for both levels.")
    }
    if(levelCounts[1L] <= levelCounts[2L]) {
      positiveClass <- observedLevels[1L]
    } else {
      positiveClass <- observedLevels[2L]
    }
    negativeClass <- setdiff(observedLevels, positiveClass)[1L]
    targetLevels <- c(negativeClass, positiveClass)
  }
  targetLevels <- as.character(targetLevels)
  if(length(targetLevels) != 2L || anyNA(targetLevels) || !setequal(targetLevels, observedLevels)) {
    cli::cli_abort("`targetLevels` must contain the two factor levels of the target.")
  }
  positiveClass <- targetLevels[2L]
  negativeClass <- targetLevels[1L]
  yData <- as.integer(as.character(targetData) == positiveClass)
  list(
    yData = yData,
    targetLevels = targetLevels,
    negativeClass = negativeClass,
    positiveClass = positiveClass
  )
}
####
## Fim
#

