#' Montar configuração de validação cruzada
#'
#' @description
#' Define como [TuneBoostTree()] cria folds para estimar a métrica de validação.
#' A implementação pública atual é deliberadamente restrita a validação cruzada
#' estratificada para classificação binária, porque o objetivo padrão é PR-AUC e
#' folds sem a classe positiva tornam essa métrica instável ou indefinida.
#'
#' @param folds Inteiro maior ou igual a 2. Número desejado de folds. Mais folds
#'   usam maior proporção dos dados para treino em cada repetição, mas aumentam o
#'   número de modelos treinados. Se a classe minoritária tiver menos observações
#'   que `folds`, [TuneBoostTree()] reduz internamente o número efetivo de folds
#'   para manter as duas classes presentes nas validações sempre que possível.
#' @param stratified Lógico escalar. Deve ser `TRUE`. `TRUE` significa que os
#'   índices da classe negativa e da classe positiva são embaralhados
#'   separadamente e distribuídos entre folds, preservando a proporção de classes
#'   de forma aproximada. `FALSE` gera erro imediato, para evitar uma
#'   configuração que poderia gerar folds sem positivos e distorcer PR-AUC,
#'   threshold e `scale_pos_weight` por fold.
#'
#' @return Lista validada com classe `tbtb_cv`, contendo `folds` e `stratified`.
#'   O valor de `folds` pode ser ajustado no objeto `config$cv` retornado por
#'   [TuneBoostTree()] quando a contagem da classe minoritária exigir redução.
#' @export

TuneBoostTreeCv <- function(folds = 10L, stratified = TRUE) {

  out <- list(folds = as.integer(folds), stratified = isTRUE(stratified))
  if(length(out$folds) != 1L || is.na(out$folds) || out$folds < 2L) {
    cli::cli_abort("`folds` must be an integer greater than or equal to 2.")
  }
  if(!out$stratified) {
    cli::cli_abort("Only stratified binary folds are currently supported; use `stratified = TRUE`.")
  }
  class(out) <- c("tbtb_cv", "list")
  out
}
####
## Fim
#


