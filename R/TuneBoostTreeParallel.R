#' Montar configuração explícita de paralelismo
#'
#' @description
#' Descreve como [TuneBoostTree()] divide CPU entre folds avaliados em paralelo e
#' threads internas da engine. Use este construtor quando `parallel = "auto"` em
#' [TuneBoostTreeControl()] não for específico o suficiente para o ambiente.
#'
#' @param workers Inteiro positivo ou `"auto"`. Número de folds avaliados
#'   simultaneamente. Valores maiores podem acelerar validação cruzada, mas cada
#'   worker treina um modelo e consome memória. Valores acima do número efetivo de
#'   folds são limitados automaticamente.
#' @param threads_per_worker Inteiro positivo ou `"auto"`. Número de threads de
#'   XGBoost/LightGBM alocado para cada worker. Aumentar este valor acelera cada
#'   fit individual, mas pode reduzir eficiência quando há muitos workers.
#' @param strategy Uma de `"auto"`, `"folds"`, `"engine"` ou `"sequential"`.
#'   `"folds"` prioriza vários folds simultâneos. `"engine"` prioriza um fold por
#'   vez com mais threads na engine. `"sequential"` desativa paralelismo entre
#'   folds. `"auto"` escolhe uma divisão balanceada.
#'
#' @return Lista validada com classe `tbtb_parallel`, contendo `workers`,
#'   `threads_per_worker` e `strategy`.
#' @export

TuneBoostTreeParallel <- function(
  workers = "auto",
  threads_per_worker = "auto",
  strategy = c("auto", "folds", "engine", "sequential")
) {

  out <- list(
    workers = workers,
    threads_per_worker = threads_per_worker,
    strategy = match.arg(strategy)
  )
  class(out) <- c("tbtb_parallel", "list")
  out
}
####
## Fim
#


