#' Montar configuração do otimizador Limbo
#'
#' @description
#' Configura o otimizador externo Limbo em protocolo ask/tell para uso opcional
#' em [TuneBoostTree()]. Quando o executável não estiver disponível e `fallback =
#' TRUE`, a execução usa o otimizador interno do pacote; quando `fallback = FALSE`,
#' a ausência do Limbo interrompe o tuning.
#'
#' @param command `NULL` ou texto escalar. Caminho absoluto, caminho relativo ou
#'   nome no `PATH` para o executável ask/tell do Limbo. `NULL` procura primeiro a
#'   variável de ambiente `TBTB_LIMBO_COMMAND` e depois o diretório `bin` do
#'   pacote. Use caminho explícito em ambientes HPC para evitar depender do
#'   `PATH` da sessão.
#' @param fallback Lógico escalar. `TRUE` permite continuar com o otimizador
#'   interno se o comando não existir, não for executável ou falhar. `FALSE` torna
#'   o Limbo obrigatório e é adequado para testes de integração ou pipelines que
#'   exigem esse backend específico.
#' @param acquisition Uma das opções `"ucb"`, `"ei"` ou `"poi"`. `"ucb"`
#'   (`upper confidence bound`) soma performance prevista e incerteza, favorecendo
#'   exploração quando `kappa` é alto. `"ei"` (`expected improvement`) favorece
#'   maior ganho esperado sobre o melhor PR-AUC observado. `"poi"` (`probability
#'   of improvement`) favorece maior probabilidade de superar o melhor valor e
#'   pode se comportar de forma mais gananciosa.
#' @param kappa Número finito usado por `"ucb"`. Valores maiores exploram regiões
#'   incertas com mais força; valores menores focam regiões já promissoras.
#' @param eps Número finito usado por `"ei"` e `"poi"`. Define margem mínima de
#'   melhoria sobre o melhor valor atual; aumentar `eps` reduz a atratividade de
#'   pequenas melhorias potencialmente ruidosas.
#'
#' @return Lista validada com classe `tbtb_optimizer`, contendo `type = "limbo"`,
#'   `command`, `fallback`, `acquisition`, `kappa` e `eps`.
#' @export

TuneBoostTreeOptimizerLimbo <- function(
  command = NULL,
  fallback = TRUE,
  acquisition = c("ucb", "ei", "poi"),
  kappa = 2.576,
  eps = 0
) {

  command <- TuneBoostTree_ResolveLimboCommand(command)
  acquisition <- match.arg(acquisition)
  out <- list(
    type = "limbo",
    command = command,
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


