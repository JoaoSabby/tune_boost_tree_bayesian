#' Montar configuração ultraotimizada
#'
#' @description
#' Monta um conjunto opinativo de configurações de alto desempenho para
#' [TuneBoostTree()]. O perfil aumenta o orçamento de árvores, usa Limbo como
#' otimizador preferencial, mantém PR-AUC como métrica e ativa paralelismo
#' automático.
#'
#' @param command `NULL` ou caminho/comando do executável ask/tell do Limbo. O
#'   valor é repassado a [TuneBoostTreeOptimizerLimbo()].
#' @param strict_limbo Lógico escalar. `TRUE` define `fallback = FALSE` no
#'   otimizador Limbo, exigindo executável funcional. `FALSE` permite fallback
#'   para o otimizador interno quando Limbo não estiver disponível.
#'
#' @return Lista nomeada com os blocos `boost`, `searchSpace`, `cv`, `optimizer`,
#'   `imbalance`, `performance` e `control`, todos prontos para uso em
#'   [TuneBoostTree()].
#' @export

TuneBoostTreeUltraConfig <- function(command = NULL, strict_limbo = TRUE) {

  list(
    boost = TuneBoostTreeBoostParams(
      trees = 1000L,
      stop_iter = 30L,
      mtry = 1,
      max_bin = 256L
    ),
    searchSpace = TuneBoostTreeSearchSpace(
      learn_rate = c(0.005, 0.2),
      tree_depth = c(2L, 12L),
      min_n = c(1, 80),
      loss_reduction = c(0, 8),
      sample_size = c(0.55, 1)
    ),
    cv = TuneBoostTreeCv(folds = 10L),
    optimizer = TuneBoostTreeOptimizerLimbo(
      command = command,
      fallback = !isTRUE(strict_limbo),
      acquisition = "ucb",
      kappa = 2.576,
      eps = 0
    ),
    imbalance = TuneBoostTreeImbalance(scale_pos_weight = "auto"),
    performance = TuneBoostTreePerformance(metric = "pr_auc", backend = "auto"),
    control = TuneBoostTreeControl(parallel = "auto", verbose = TRUE)
  )
}
####
## Fim
#


