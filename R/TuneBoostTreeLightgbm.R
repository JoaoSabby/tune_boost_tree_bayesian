#' Montar configuração da engine LightGBM
#'
#' @description
#' Cria o bloco de engine LightGBM usado por [TuneBoostTree()]. LightGBM é a
#' engine padrão da função principal. Os hiperparâmetros do modelo continuam nos
#' construtores [TuneBoostTreeBoostParams()] e [TuneBoostTreeSearchSpace()].
#'
#' @param metric Texto escalar repassado ao LightGBM como métrica de avaliação
#'   nativa para `early stopping`. O padrão `"average_precision"` é coerente com
#'   o objetivo PR-AUC do pacote. Outras métricas dependem da versão do LightGBM e
#'   podem alterar a iteração escolhida por `early stopping`, ainda que o score do
#'   tuner continue sendo calculado pela configuração de performance do pacote.
#'
#' @return Lista validada com classe `tbtb_engine`, contendo `name = "lightgbm"`,
#'   `metric` e `feature_types = NULL`.
#' @export

TuneBoostTreeLightgbm <- function(metric = "average_precision") {

  out <- list(name = "lightgbm", metric = as.character(metric)[1L], feature_types = NULL)
  class(out) <- c("tbtb_engine", "list")
  out
}
####
## Fim
#


