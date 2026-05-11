#' Montar configuração de desbalanceamento de classes
#'
#' @description
#' Controla duas estratégias independentes para classificação binária
#' desbalanceada: balanceamento físico do treino de cada fold por uma função do
#' usuário (`balanceFn`) e ponderação da classe positiva (`scale_pos_weight`). O
#' balanceamento nunca é aplicado ao fold de validação, preservando uma estimativa
#' honesta da distribuição original.
#'
#' @param balanceFn `NULL` ou função. Quando não é `NULL`, a função é chamada uma
#'   vez para a partição de treino de cada fold com a assinatura
#'   `balanceFn(data, formula, ...)`. O argumento `data` é um `data.frame` com as
#'   linhas de treino daquele fold antes da criação das matrizes de engine;
#'   `formula` é exatamente a fórmula passada a [TuneBoostTree()]; `...` recebe
#'   somente os argumentos extras informados em [TuneBoostTreeImbalance()]. A
#'   função deve retornar `data.frame`, tibble ou data.table contendo a variável
#'   resposta e todos os preditores exigidos por `formula`. O retorno pode ter
#'   mais ou menos linhas que a entrada, permitindo oversampling, undersampling ou
#'   geração sintética. Não deve retornar matriz, `xgb.DMatrix`, `lgb.Dataset`,
#'   lista de parâmetros nem objeto já modelado.
#' @param scale_pos_weight `"auto"`, `NULL` ou número positivo. `"auto"` calcula
#'   a razão `negativos / positivos` no treino de cada fold e a repassa para a
#'   engine. `NULL` desativa essa ponderação. Um número positivo fixa o mesmo
#'   peso em todos os folds e no ajuste final. Quando `balanceFn` altera a
#'   distribuição de classes, o peso automático é calculado depois do
#'   balanceamento.
#' @param ... Argumentos nomeados extras repassados exclusivamente a `balanceFn`
#'   depois de `data` e `formula`. Eles não são enviados para XGBoost, LightGBM,
#'   otimizador, métrica ou função principal. Use este ponto para parâmetros como
#'   taxa de oversampling, seed local do método de balanceamento ou nome da classe
#'   alvo esperada pela sua função.
#'
#' @return Lista validada com classe `tbtb_imbalance`, contendo `balanceFn`,
#'   `scale_pos_weight` e `balance_args`.
#' @export

TuneBoostTreeImbalance <- function(balanceFn = NULL, scale_pos_weight = "auto", ...) {

  if(!is.null(balanceFn) && !is.function(balanceFn)) {
    cli::cli_abort("`balanceFn` must be a function or `NULL`.")
  }
  if(is.character(scale_pos_weight)) {
    if(!identical(scale_pos_weight, "auto")) {
      cli::cli_abort("`scale_pos_weight` must be `\"auto\"`, `NULL`, or a positive numeric scalar.")
    }
  } else if(!is.null(scale_pos_weight)) {
    scale_pos_weight <- as.numeric(scale_pos_weight)[1L]
    if(!is.finite(scale_pos_weight) || scale_pos_weight <= 0) {
      cli::cli_abort("Numeric `scale_pos_weight` must be positive and finite.")
    }
  }
  out <- list(
    balanceFn = balanceFn,
    scale_pos_weight = scale_pos_weight,
    balance_args = list(...)
  )
  class(out) <- c("tbtb_imbalance", "list")
  out
}
####
## Fim
#


