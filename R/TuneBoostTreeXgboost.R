#' Montar configuração da engine XGBoost
#'
#' @description
#' Cria o bloco de engine XGBoost usado por [TuneBoostTree()]. Os
#' hiperparâmetros do modelo continuam sendo informados com nomes de
#' `parsnip::boost_tree()` em `boost` e `searchSpace`; este construtor controla
#' apenas opções nativas da engine.
#'
#' @param eval_metric Texto escalar, `"aucpr"` ou `"auc"`. `"aucpr"` é alinhado
#'   ao objetivo PR-AUC do pacote e costuma ser preferível em dados
#'   desbalanceados. `"auc"` usa ROC-AUC para o `early stopping` nativo e pode ser
#'   útil quando ROC-AUC for a referência operacional externa.
#' @param tree_method Texto escalar repassado ao XGBoost. `"hist"` é o padrão por
#'   ser rápido e eficiente em memória. Outros valores aceitos dependem da versão
#'   instalada do XGBoost.
#' @param feature_types `NULL` ou vetor de textos alinhado aos preditores,
#'   repassado ao `xgb.DMatrix`. Use apenas quando precisar informar tipos de
#'   features ao XGBoost; `NULL` deixa a engine inferir o tratamento padrão das
#'   matrizes numéricas criadas pelo pacote.
#'
#' @return Lista validada com classe `tbtb_engine`, contendo `name = "xgboost"`,
#'   `eval_metric`, `tree_method` e `feature_types`.
#' @export

TuneBoostTreeXgboost <- function(eval_metric = "aucpr", tree_method = "hist", feature_types = NULL) {

  out <- list(
    name = "xgboost",
    eval_metric = as.character(eval_metric)[1L],
    tree_method = as.character(tree_method)[1L],
    feature_types = feature_types
  )
  if(!(out$eval_metric %in% c("aucpr", "auc"))) {
    cli::cli_abort("`eval_metric` must be 'aucpr' or 'auc' for XGBoost.")
  }
  class(out) <- c("tbtb_engine", "list")
  out
}
####
## Fim
#


