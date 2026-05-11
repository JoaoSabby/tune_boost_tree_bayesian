#' Executar tuning ultraotimizado de boosted trees
#'
#' @description
#' Atalho que cria [TuneBoostTreeUltraConfig()] e chama [TuneBoostTree()] com essa
#' configuração. Use quando quiser o perfil de alto desempenho sem montar cada
#' bloco manualmente.
#'
#' @param formula Fórmula de duas faces para classificação binária, repassada a
#'   [TuneBoostTree()].
#' @param data data.frame, tibble ou data.table com as linhas de treino,
#'   repassado a [TuneBoostTree()].
#' @param initial Inteiro, `NULL` ou tibble/data.frame de warm-start, repassado a
#'   [TuneBoostTree()].
#' @param nIter Inteiro com número de iterações do otimizador após a
#'   inicialização.
#' @param engine `"xgboost"`, `"lightgbm"` ou configuração de engine criada por
#'   [TuneBoostTreeXgboost()] ou [TuneBoostTreeLightgbm()].
#' @param command Caminho/comando do executável ask/tell do Limbo, repassado a
#'   [TuneBoostTreeUltraConfig()].
#' @param strict_limbo Lógico escalar. `TRUE` exige Limbo funcional; `FALSE`
#'   permite fallback interno.
#'
#' @return O mesmo objeto `tbtb_tune_result` retornado por [TuneBoostTree()], com
#'   `bestHyperparameters`, `bestScore`, `bestThreshold`, `initial` (tibble),
#'   `evaluationLog` (tibble) e `config`.
#' @export

TuneBoostTreeBayesianUltra <- function(
  formula,
  data,
  initial = 20L,
  nIter = 60L,
  engine = "lightgbm",
  command = NULL,
  strict_limbo = TRUE
) {

  ultra <- TuneBoostTreeUltraConfig(command = command, strict_limbo = strict_limbo)
  TuneBoostTree(
    formula = formula,
    data = data,
    initial = initial,
    nIter = nIter,
    engine = engine,
    boost = ultra$boost,
    searchSpace = ultra$searchSpace,
    cv = ultra$cv,
    optimizer = ultra$optimizer,
    imbalance = ultra$imbalance,
    performance = ultra$performance,
    control = ultra$control
  )
}
####
## Fim
#


