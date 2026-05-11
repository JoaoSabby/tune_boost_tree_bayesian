#' Montar configuração de performance
#'
#' @description
#' Define a métrica otimizada por [TuneBoostTree()] e o backend usado para
#' calculá-la nas avaliações de validação cruzada.
#'
#' @param metric Texto escalar. Atualmente apenas `"pr_auc"` é suportado. PR-AUC
#'   prioriza qualidade de ranking para a classe positiva e é mais informativa do
#'   que acurácia em cenários desbalanceados.
#' @param backend Texto com uma das opções `"auto"`, `"c"`, `"fortran"`,
#'   `"rfast"` ou `"r"`. `"auto"` tenta escolher a implementação mais rápida
#'   disponível e cai para R puro quando necessário. `"c"` e `"fortran"` usam
#'   rotinas nativas do pacote quando compiladas. `"rfast"` usa o pacote Rfast se
#'   instalado. `"r"` usa implementação portátil em R, mais simples e geralmente
#'   mais lenta.
#'
#' @return Lista validada com classe `tbtb_performance`, contendo `metric` e
#'   `backend`.
#' @export

TuneBoostTreePerformance <- function(metric = "pr_auc", backend = "auto") {

  metric <- match.arg(as.character(metric)[1L], c("pr_auc"))
  backend <- match.arg(
    as.character(backend)[1L],
    c("auto", "c", "fortran", "rfast", "r")
  )
  out <- list(metric = metric, backend = backend)
  class(out) <- c("tbtb_performance", "list")
  out
}
####
## Fim
#


