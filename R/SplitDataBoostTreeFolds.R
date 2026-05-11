#' Dividir dados em folds estratificados para boost-tree
#'
#' @param yData Vetor binário inteiro ou lógico da variável resposta.
#' @param nFolds Inteiro com número de folds.
#' @param seed Inteiro usado como semente aleatória.
#'
#' @description
#' Cria folds estratificados para dados binários 0/1, usando a mesma lógica de
#' estratificação do tuner.
#'
#' @return Lista de vetores inteiros. Cada elemento contém os índices de teste de
#'   um fold; os índices de treino são obtidos pelo complemento.
#' @export

SplitDataBoostTreeFolds <- function(yData, nFolds = 10L, seed = 42L) {

  TuneBoostTree_CreateStratifiedFolds(yData = yData, nFolds = nFolds, seed = seed)
}
####
## Fim
#

