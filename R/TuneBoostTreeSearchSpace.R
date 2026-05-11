#' Montar espaço de busca Bayesiano
#'
#' @description
#' Define os limites inferiores e superiores dos hiperparâmetros que serão
#' explorados pelo otimizador de [TuneBoostTree()]. O espaço de busca controla o
#' que varia; [TuneBoostTreeBoostParams()] controla o que fica fixo. Sempre que
#' possível, os nomes seguem `parsnip::boost_tree()` para facilitar migração de
#' fluxos tidymodels.
#'
#' @details
#' Cada argumento deve ser `NULL` ou um vetor de tamanho 2 no formato
#' `c(limite_inferior, limite_superior)`. O limite inferior precisa ser menor que
#' o superior. Parâmetros com `NULL` não são tunados. Parâmetros inteiros, como
#' `tree_depth`, `max_bin`, `num_leaves` e `min_data_in_leaf`, são amostrados em
#' escala numérica e arredondados antes do ajuste.
#'
#' Parâmetros com o mesmo nome fixados em [TuneBoostTreeBoostParams()] prevalecem
#' sobre o espaço de busca. Por exemplo, se `boost = TuneBoostTreeBoostParams(mtry
#' = "default")`, `mtry` fica em 0.8 mesmo que exista um limite de `mtry` aqui.
#'
#' @param learn_rate Vetor numérico de tamanho 2 em `(0, 1]`. Limites da taxa de
#'   aprendizado. Faixas mais baixas favorecem modelos mais estáveis e exigem
#'   mais árvores; faixas altas aceleram a busca, mas podem sobreajustar.
#' @param tree_depth Vetor numérico/inteiro de tamanho 2. Limites da profundidade
#'   máxima das árvores. Valores são arredondados. Profundidades maiores capturam
#'   interações mais complexas; profundidades menores regularizam e reduzem
#'   tempo.
#' @param min_n Vetor numérico de tamanho 2. Limites do tamanho/peso mínimo de
#'   nó/folha em nomenclatura parsnip. Mapeia para `min_child_weight` no XGBoost
#'   e `min_sum_hessian_in_leaf` no LightGBM. Valores maiores evitam folhas muito
#'   pequenas.
#' @param loss_reduction Vetor numérico de tamanho 2, não negativo. Limites da
#'   redução mínima de perda exigida para split. Mapeia para `gamma` no XGBoost e
#'   `min_gain_to_split` no LightGBM. Valores maiores reduzem complexidade.
#' @param sample_size Vetor numérico de tamanho 2 em `(0, 1]`. Limites da fração
#'   de linhas usada por iteração. Valores abaixo de 1 introduzem amostragem
#'   estocástica e podem melhorar generalização.
#' @param mtry `NULL` ou vetor numérico de tamanho 2 em `(0, 1]`. Limites da
#'   fração de features considerada. Use `NULL` para não tunar. Para fixar o
#'   padrão de 80%, use `mtry = "default"` em [TuneBoostTreeBoostParams()].
#' @param max_bin `NULL` ou vetor numérico/inteiro de tamanho 2. Limites do número
#'   de bins de histograma. Valores maiores preservam detalhes em variáveis
#'   contínuas; valores menores reduzem memória e tempo.
#' @param lambda `NULL` ou vetor numérico de tamanho 2, não negativo. Limites da
#'   regularização L2. Mapeia para `lambda` no XGBoost e `lambda_l2` no LightGBM.
#'   Valores maiores reduzem magnitude dos pesos das folhas.
#' @param alpha `NULL` ou vetor numérico de tamanho 2, não negativo. Limites da
#'   regularização L1. Mapeia para `alpha` no XGBoost e `lambda_l1` no LightGBM.
#'   Valores maiores podem induzir pesos de folhas mais esparsos.
#' @param max_delta_step `NULL` ou vetor numérico de tamanho 2, não negativo.
#'   Limites do estabilizador logístico do XGBoost, útil em desbalanceamento
#'   severo. É ignorado pelo LightGBM.
#' @param colsample_bytree `NULL` ou vetor numérico de tamanho 2 em `(0, 1]`.
#'   Limites da amostragem de features por árvore. Mapeia para
#'   `colsample_bytree` no XGBoost e `feature_fraction` no LightGBM.
#' @param colsample_bylevel `NULL` ou vetor numérico de tamanho 2 em `(0, 1]`.
#'   Limites da amostragem de features por nível no XGBoost. É ignorado pelo
#'   LightGBM.
#' @param num_leaves `NULL` ou vetor numérico/inteiro de tamanho 2. Limites do
#'   número máximo de folhas no LightGBM. Aumenta capacidade e risco de
#'   sobreajuste. É ignorado pelo XGBoost.
#' @param min_data_in_leaf `NULL` ou vetor numérico/inteiro de tamanho 2. Limites
#'   do mínimo de linhas por folha no LightGBM. Valores maiores regularizam folhas
#'   pequenas. É ignorado pelo XGBoost.
#' @param scale_pos_weight `NULL` ou vetor numérico positivo de tamanho 2.
#'   Limites do peso da classe positiva. Preferencialmente use
#'   [TuneBoostTreeImbalance()] com `scale_pos_weight = "auto"`; tune este peso
#'   apenas quando houver dados suficientes e validação robusta.
#'
#' @return Lista validada com classe `tbtb_search_space`. Cada elemento é um
#'   vetor numérico `c(lower, upper)` finito e crescente. Elementos `NULL` são
#'   removidos e não são otimizados.
#' @export

TuneBoostTreeSearchSpace <- function(
  learn_rate = c(0.01, 0.2),
  tree_depth = c(2L, 12L),
  min_n = c(1, 80),
  loss_reduction = c(0, 8),
  sample_size = c(0.55, 1),
  mtry = NULL,
  max_bin = NULL,
  lambda = NULL,
  alpha = NULL,
  max_delta_step = NULL,
  colsample_bytree = NULL,
  colsample_bylevel = NULL,
  num_leaves = NULL,
  min_data_in_leaf = NULL,
  scale_pos_weight = NULL
) {

  out <- list(
    learn_rate        = learn_rate,
    tree_depth        = tree_depth,
    min_n             = min_n,
    sample_size       = sample_size,
    mtry              = mtry,
    loss_reduction    = loss_reduction,
    max_bin           = max_bin,
    lambda            = lambda,
    alpha             = alpha,
    max_delta_step    = max_delta_step,
    colsample_bytree  = colsample_bytree,
    colsample_bylevel = colsample_bylevel,
    num_leaves        = num_leaves,
    min_data_in_leaf  = min_data_in_leaf,
    scale_pos_weight  = scale_pos_weight
  )
  out <- out[!vapply(out, is.null, logical(1L))]
  for(parameterName in names(out)) {
    value <- as.numeric(out[[parameterName]])
    if(length(value) != 2L || anyNA(value) || any(!is.finite(value)) || value[1L] >= value[2L]) {
      cli::cli_abort("Search-space entry `{parameterName}` must contain finite increasing lower/upper bounds.")
    }
    out[[parameterName]] <- value
  }
  for(fractionName in intersect(c("mtry", "sample_size", "colsample_bytree", "colsample_bylevel"), names(out))) {
    if(out[[fractionName]][1L] <= 0 || out[[fractionName]][2L] > 1) {
      cli::cli_abort("`{fractionName}` search bounds must be fractions in `(0, 1]`.")
    }
  }
  if("scale_pos_weight" %in% names(out) && out$scale_pos_weight[1L] <= 0) {
    cli::cli_abort("`scale_pos_weight` search bounds must be positive.")
  }
  class(out) <- c("tbtb_search_space", "list")
  out
}
####
## Fim
#


