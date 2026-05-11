#' Montar configuração do otimizador interno
#'
#' @description
#' Cria uma configuração de otimizador sem dependências externas. Este backend é
#' usado como fallback seguro quando Limbo ou rBayesianOptimization não estão
#' disponíveis, e também pode ser solicitado explicitamente para ambientes onde a
#' reprodutibilidade e a ausência de dependências opcionais são mais importantes
#' que recursos avançados do backend externo.
#'
#' @return Lista com classe `tbtb_optimizer`, `type = "internal"`, `fallback =
#'   TRUE`, `acquisition = "internal"`, `kappa = 0` e `eps = 0`. Esses campos
#'   mantêm a mesma estrutura dos demais otimizadores para simplificar o fluxo da
#'   função principal.
#' @export

TuneBoostTreeInternalOptimizer <- function() {

  out <- list(
    type = "internal",
    command = NA_character_,
    fallback = TRUE,
    acquisition = "internal",
    kappa = 0,
    eps = 0
  )
  class(out) <- c("tbtb_optimizer", "list")
  out
}
####
## Fim
#


