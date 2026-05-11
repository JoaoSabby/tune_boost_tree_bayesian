#' Montar configuração do otimizador rBayesianOptimization
#'
#' @description
#' Configura [rBayesianOptimization::BayesianOptimization()] como backend de
#' otimização de [TuneBoostTree()]. Este backend recebe o espaço de busca,
#' avalia candidatos por validação cruzada e usa uma função de aquisição para
#' decidir quais hiperparâmetros testar depois dos pontos iniciais.
#'
#' @param acquisition Texto com uma das opções `"ucb"`, `"ei"` ou `"poi"`.
#'   `"ucb"` (`upper confidence bound`) combina valor previsto alto e incerteza
#'   alta; é uma opção robusta quando se deseja equilibrar exploração e
#'   intensificação. `"ei"` (`expected improvement`) escolhe pontos com maior
#'   ganho esperado sobre o melhor score atual; tende a ser eficiente quando a
#'   superfície é relativamente suave. `"poi"` (`probability of improvement`)
#'   prioriza a probabilidade de superar o melhor score; pode ser mais ganancioso
#'   e sensível a pequenas diferenças quando `eps` é baixo.
#' @param kappa Número finito usado por `acquisition = "ucb"`. Valores maiores
#'   aumentam exploração em regiões incertas; valores menores concentram a busca
#'   próximo de regiões já promissoras. É mantido no objeto mesmo quando a
#'   aquisição escolhida não usa `kappa`.
#' @param eps Número finito usado por `acquisition = "ei"` e `"poi"`. Representa
#'   uma margem mínima de melhoria sobre o melhor resultado atual. Aumentar `eps`
#'   reduz o interesse por ganhos pequenos e pode tornar a busca mais
#'   conservadora diante de ruído de validação cruzada.
#' @param fallback Lógico escalar. `TRUE` permite usar o otimizador interno do
#'   pacote se `rBayesianOptimization` não estiver disponível ou falhar no
#'   ambiente. `FALSE` torna esse backend obrigatório e falha cedo quando ele não
#'   puder ser usado.
#'
#' @return Lista validada com classe `tbtb_optimizer`, contendo `type =
#'   "rBayesianOptimization"`, `command`, `fallback`, `acquisition`, `kappa` e
#'   `eps`.
#' @export

TuneBoostTreeOptimizerRBayesianOptimization <- function(
  acquisition = c("ucb", "ei", "poi"),
  kappa = 2.576,
  eps = 0,
  fallback = TRUE
) {

  acquisition <- match.arg(acquisition)
  out <- list(
    type = "rBayesianOptimization",
    command = NA_character_,
    fallback = isTRUE(fallback),
    acquisition = acquisition,
    kappa = as.numeric(kappa)[1L],
    eps = as.numeric(eps)[1L]
  )
  if(!is.finite(out$kappa) || !is.finite(out$eps)) {
    cli::cli_abort("`kappa` and `eps` must be finite numerics.")
  }
  class(out) <- c("tbtb_optimizer", "list")
  out
}
####
## Fim
#


