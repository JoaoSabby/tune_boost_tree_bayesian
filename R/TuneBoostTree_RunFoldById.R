#' Executar um fold por identificador
#'
#' @param foldId Inteiro identificador do fold.
#' @param balancedFolds Lista de folds com matrizes R serializáveis.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Inteiro com limite de rodadas da tunagem.
#' @param earlyStoppingRounds Inteiro com paciência da parada antecipada.
#' @param seed Inteiro usado como semente aleatória.
#' @param nThreads Inteiro com threads deste worker de fold.
#' @param evalMetric Nome da métrica do XGBoost.
#' @param engine Nome da engine.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Adaptador pequeno de topo que mantém workers paralelos
#'   autocontidos e evita closures sobre o estado dos folds.
#'
#' @return Lista com resultado do fold.
#' @noRd

TuneBoostTree_RunFoldById <- function(
  foldId,
  balancedFolds,
  hyperparameters,
  nRounds,
  earlyStoppingRounds,
  seed,
  nThreads,
  evalMetric,
  engine,
  prAucBackend = "auto"
) {

  tryCatch(
    TuneBoostTree_RunOneFold(
      foldData = balancedFolds[[foldId]],
      hyperparameters = hyperparameters,
      nRounds = nRounds,
      earlyStoppingRounds = earlyStoppingRounds,
      seed = seed + foldId,
      nThreads = nThreads,
      evalMetric = evalMetric,
      engine = engine,
      prAucBackend = prAucBackend
    ),
    error = function(e) {
      list(
        score = NA_real_,
        bestIteration = NA_integer_,
        errorMessage = conditionMessage(e)
      )
    }
  )
}
####
## Fim
#

