#' Montar configuração de controle de execução
#'
#' @description
#' Controla reprodutibilidade, paralelismo, verbosidade e fallback do número de
#' árvores final em [TuneBoostTree()]. Este objeto não define hiperparâmetros do
#' modelo; ele define como o tuning é executado.
#'
#' @param seed Inteiro escalar. Semente usada para criar folds, amostrar pontos
#'   iniciais/candidatos, inicializar otimizadores e repassar seeds às engines.
#'   Usar o mesmo `seed` com os mesmos dados e dependências tende a reproduzir a
#'   mesma sequência de candidatos e folds.
#' @param parallel `"auto"`, `FALSE`, `"sequential"` ou lista criada por
#'   [TuneBoostTreeParallel()]. `"auto"` decide entre paralelizar folds ou usar
#'   threads internas da engine conforme dados, folds e CPUs. `FALSE` e
#'   `"sequential"` desativam paralelismo entre folds e deixam as threads para a
#'   engine. Uma lista de [TuneBoostTreeParallel()] dá controle explícito sobre
#'   workers e threads.
#' @param verbose Lógico escalar (`TRUE` ou `FALSE`). `TRUE` exibe mensagens de
#'   alto nível do pacote, como início/fim do tuning, engine usada, número de
#'   árvores, `stop_iter`, workers de fold e progresso do otimizador quando o
#'   backend suporta. `FALSE` silencia essas mensagens do pacote. Em ambos os
#'   casos, a saída detalhada de XGBoost/LightGBM durante a validação cruzada é
#'   mantida compacta/suprimida para evitar logs enormes.
#' @param fallback_trees Inteiro positivo. Número de árvores usado somente se o
#'   pacote não conseguir recuperar uma melhor iteração válida do `early
#'   stopping` para o melhor candidato. Em execuções normais, o valor final de
#'   `trees` vem do melhor fold/iterações observadas, não deste fallback.
#'
#' @return Lista validada com classe `tbtb_control`, contendo `seed`, `parallel`,
#'   `verbose` e `fallback_trees`.
#' @export

TuneBoostTreeControl <- function(seed = 42L, parallel = "auto", verbose = TRUE, fallback_trees = 100L) {

  out <- list(
    seed           = as.integer(seed),
    parallel       = parallel,
    verbose        = verbose,
    fallback_trees = as.integer(fallback_trees)
  )
  if(length(out$seed) != 1L || is.na(out$seed)) {
    cli::cli_abort("`seed` must be a scalar integer.")
  }
  if(length(out$fallback_trees) != 1L || is.na(out$fallback_trees) || out$fallback_trees < 1L) {
    cli::cli_abort("`fallback_trees` must be a positive integer.")
  }
  class(out) <- c("tbtb_control", "list")
  out
}
####
## Fim
#


