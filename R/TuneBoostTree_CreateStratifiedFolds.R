#' Criar folds estratificados
#'
#' @param yData Vetor alvo inteiro 0/1.
#' @param nFolds Inteiro com número de folds.
#' @param seed Inteiro usado como semente aleatória.
#'
#' @details Divisor interno de folds que preserva a lógica original de alocação alternada por classe.
#'
#' @return Lista de vetores inteiros com índices de teste.
#' @noRd

TuneBoostTree_CreateStratifiedFolds <- function(yData, nFolds = 10L, seed = 42L) {

  if(length(yData) == 0L || anyNA(yData)) {
    cli::cli_abort("`yData` must be a non-empty binary vector encoded as 0/1 or FALSE/TRUE.")
  }
  if(is.logical(yData)) {
    yData <- as.integer(yData)
  } else if(is.numeric(yData) && all(yData %in% c(0, 1))) {
    yData <- as.integer(yData)
  } else {
    cli::cli_abort("`yData` must be a non-empty binary vector encoded as 0/1 or FALSE/TRUE.")
  }

  nFolds <- as.integer(nFolds)

  if(length(nFolds) != 1L || is.na(nFolds) || nFolds < 2L) {
    cli::cli_abort("`nFolds` must be a single integer greater than or equal to 2.")
  }

  classCounts <- table(yData)
  if(length(classCounts) != 2L || any(classCounts == 0L)) {
    cli::cli_abort("`yData` must contain both binary classes.")
  }
  if(any(classCounts < nFolds)) {
    cli::cli_abort("Each binary class must contain at least `nFolds` observations for stratified folds.")
  }

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

  set.seed(seed)
  negativeIndex <- sample(which(yData == 0L))
  positiveIndex <- sample(which(yData == 1L))
  folds <- vector("list", nFolds)
  for(foldId in seq_len(nFolds)) {
    folds[[foldId]] <- c(
      negativeIndex[seq(foldId, length(negativeIndex), by = nFolds)],
      positiveIndex[seq(foldId, length(positiveIndex), by = nFolds)]
    )
  }
  folds
}
####
## Fim
#

