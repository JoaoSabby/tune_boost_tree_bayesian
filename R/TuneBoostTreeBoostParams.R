#' Montar parâmetros fixos de boosted trees
#'
#' @description
#' Constrói o bloco `boost` consumido pelo argumento `boost` de
#' [TuneBoostTree()]. Este bloco define o que fica fixo no treinamento e também
#' guarda o teto de árvores e a paciência de `early stopping`. Os nomes seguem,
#' quando há equivalência direta, a API de `parsnip::boost_tree()` para facilitar
#' a leitura: `trees`, `learn_rate`, `tree_depth`, `min_n`, `loss_reduction`,
#' `sample_size`, `mtry` e `stop_iter`.
#'
#' @details
#' Há uma separação intencional entre `boost` e `searchSpace`:
#'
#' * valores diferentes de `NULL` neste objeto são tratados como escolhas fixas e
#'   prevalecem sobre qualquer limite com o mesmo nome em
#'   [TuneBoostTreeSearchSpace()];
#' * valores `NULL` deixam o parâmetro livre para ser otimizado somente quando o
#'   mesmo nome existir em `searchSpace`;
#' * `trees` e `stop_iter` não são otimizados diretamente. `trees` é o teto de
#'   rodadas usado durante a validação cruzada e o número final salvo em
#'   `bestHyperparameters$trees` vem do `early stopping`; `stop_iter` é a
#'   paciência usada para encontrar esse ponto.
#'
#' No ajuste, o pacote traduz esses nomes para parâmetros nativos. Exemplos:
#' `learn_rate` vira `eta` no XGBoost e `learning_rate` no LightGBM;
#' `tree_depth` vira `max_depth`; `min_n` vira `min_child_weight` no XGBoost e
#' `min_sum_hessian_in_leaf` no LightGBM; `loss_reduction` vira `gamma` no
#' XGBoost e `min_gain_to_split` no LightGBM; `sample_size` vira
#' `subsample`/`bagging_fraction`; `mtry` vira a fração de colunas por nó/split;
#' `max_bin` controla a discretização histogram-based.
#'
#' @param trees Inteiro positivo. Número máximo de rodadas/árvores avaliadas em
#'   cada treino de validação cruzada. É um limite superior de custo: valores
#'   maiores permitem que o `early stopping` encontre modelos mais longos, mas
#'   aumentam tempo e memória. O modelo final normalmente usa menos rodadas,
#'   copiadas para `bestHyperparameters$trees`.
#' @param stop_iter Inteiro positivo. Paciência do `early stopping`, isto é, o
#'   número de rodadas sem melhora na métrica nativa da engine antes de parar o
#'   fold. Valores pequenos economizam tempo, mas podem parar cedo demais;
#'   valores grandes reduzem esse risco e aumentam o custo.
#' @param learn_rate `NULL` ou escalar numérico em `(0, 1]`. Taxa de aprendizado
#'   fixa. Valores menores tornam cada árvore mais conservadora e geralmente
#'   exigem mais `trees`; valores maiores aceleram o ajuste e aumentam risco de
#'   sobreajuste. Use `NULL` para permitir otimização via `searchSpace`.
#' @param tree_depth `NULL` ou inteiro positivo. Profundidade máxima fixa das
#'   árvores. Profundidades maiores capturam interações mais complexas; valores
#'   menores regularizam e reduzem custo. Use `NULL` para otimizar.
#' @param min_n `NULL` ou número positivo. Tamanho/peso mínimo do nó ou folha na
#'   nomenclatura parsnip. Valores maiores tornam splits mais conservadores e são
#'   úteis contra sobreajuste em dados ruidosos. Use `NULL` para otimizar.
#' @param loss_reduction `NULL` ou número não negativo. Ganho/redução mínima de
#'   perda exigida para criar um split. Aumentar esse valor simplifica as árvores
#'   e pode melhorar generalização. Use `NULL` para otimizar.
#' @param sample_size `NULL` ou número em `(0, 1]`. Fração de linhas usada por
#'   iteração. `1` usa todas as linhas; valores menores introduzem amostragem
#'   estocástica, reduzem custo por árvore e podem atuar como regularização. Use
#'   `NULL` para otimizar.
#' @param mtry `"default"`, `NULL` ou número em `(0, 1]`. Fração de features
#'   considerada por split/nó. `"default"` fixa `0.8`, uma escolha segura para
#'   iniciar; um número fixa outra fração; `NULL` permite otimizar quando `mtry`
#'   estiver em `searchSpace`.
#' @param max_bin `NULL` ou inteiro positivo. Número de bins para engines
#'   histogram-based. Valores maiores preservam mais detalhe de variáveis
#'   contínuas e podem aumentar memória/tempo; valores menores aceleram e podem
#'   regularizar. Use `NULL` para manter o padrão interno/otimizar quando houver
#'   limite correspondente.
#'
#' @return Lista validada com classe `tbtb_boost_params`, contendo `trees`,
#'   `stop_iter`, `learn_rate`, `tree_depth`, `min_n`, `loss_reduction`,
#'   `sample_size`, `mtry` e `max_bin`.
#' @export

TuneBoostTreeBoostParams <- function(
  trees = 500L,
  stop_iter = 20L,
  learn_rate = NULL,
  tree_depth = NULL,
  min_n = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  mtry = "default",
  max_bin = NULL
) {

  if(is.character(mtry) && identical(mtry[1L], "default")) {
    mtryValue <- "default"
  } else {
    mtryValue <- mtry
  }
  out <- list(
    trees          = as.integer(trees),
    stop_iter      = as.integer(stop_iter),
    learn_rate     = learn_rate,
    tree_depth     = tree_depth,
    min_n          = min_n,
    loss_reduction = loss_reduction,
    sample_size    = sample_size,
    mtry           = mtryValue,
    max_bin        = max_bin
  )
  if(length(out$trees) != 1L || is.na(out$trees) || out$trees < 1L) {
    cli::cli_abort("`trees` must be a positive integer.")
  }
  if(length(out$stop_iter) != 1L || is.na(out$stop_iter) || out$stop_iter < 1L) {
    cli::cli_abort("`stop_iter` must be a positive integer.")
  }
  if(is.character(out$mtry) && !identical(out$mtry, "default")) {
    cli::cli_abort("`mtry` as character must be exactly 'default'.")
  }
  class(out) <- c("tbtb_boost_params", "list")
  out
}
####
## Fim
#


