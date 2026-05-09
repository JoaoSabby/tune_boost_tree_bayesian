#' Montar parâmetros fixos de boosted trees
#'
#' @description
#' Constrói o bloco `boost` consumido por [TuneBoostTree()]. Este bloco guarda
#' valores fixos do modelo, usando a nomenclatura de `parsnip::boost_tree()`
#' sempre que há equivalência (`trees`, `learn_rate`, `tree_depth`, `min_n`,
#' `loss_reduction`, `sample_size`, `mtry` e `stop_iter`). Valores diferentes de
#' `NULL` são mantidos fixos e prevalecem sobre qualquer candidato gerado pelo
#' otimizador. Valores `NULL` só serão otimizados quando o mesmo nome também
#' existir em [TuneBoostTreeSearchSpace()].
#'
#' @details
#' O pacote traduz esses nomes para os parâmetros nativos da engine no momento do
#' ajuste. Por exemplo, `learn_rate` vira `eta` no XGBoost e `learning_rate` no
#' LightGBM; `sample_size` vira `subsample`/`bagging_fraction`; `mtry` vira
#' fração de colunas por split/nó ou recurso equivalente disponível.
#'
#' @param trees Inteiro positivo. Número máximo de rodadas/árvores avaliadas em
#'   cada treino de validação cruzada. É o teto usado durante o tuning; o valor
#'   final gravado em `bestHyperparameters$trees` pode ser menor porque é
#'   escolhido por `early stopping`.
#' @param stop_iter Inteiro positivo. Paciência de `early stopping`, no mesmo
#'   sentido usado por tidymodels/parsnip. O treino de cada fold interrompe a
#'   avaliação após esse número de rodadas sem melhora na métrica de validação.
#'   Valores pequenos economizam tempo, mas podem interromper modelos que melhoram
#'   lentamente; valores grandes dão mais margem ao modelo e aumentam o custo.
#' @param learn_rate `NULL` ou escalar numérico em `(0, 1]`. Taxa de aprendizado
#'   aplicada à contribuição de cada árvore. Valores menores costumam exigir mais
#'   árvores e produzir busca mais estável; valores maiores aceleram o ajuste e
#'   podem aumentar sobreajuste. `NULL` deixa o parâmetro livre apenas se houver
#'   limites em [TuneBoostTreeSearchSpace()].
#' @param tree_depth `NULL` ou inteiro positivo. Profundidade máxima fixa das
#'   árvores. Valores maiores permitem interações mais complexas; valores menores
#'   regularizam e reduzem custo.
#' @param min_n `NULL` ou número positivo. Tamanho/peso mínimo de nó ou folha no
#'   vocabulário de parsnip. Internamente é convertido para `min_child_weight` no
#'   XGBoost e `min_sum_hessian_in_leaf` no LightGBM. Valores maiores tornam
#'   splits mais conservadores.
#' @param loss_reduction `NULL` ou número não negativo. Redução mínima de perda
#'   exigida para criar um split. Mapeia para `gamma` no XGBoost e
#'   `min_gain_to_split` no LightGBM. Aumentar este valor simplifica árvores e
#'   reduz sobreajuste.
#' @param sample_size `NULL` ou número em `(0, 1]`. Fração de linhas amostrada em
#'   cada iteração. `1` usa todas as linhas; valores menores adicionam
#'   regularização estocástica e podem melhorar generalização em dados ruidosos.
#' @param mtry `"default"`, `NULL` ou número em `(0, 1]`. `"default"` fixa o uso
#'   de aproximadamente 80% das features (`0.8`) em cada split/nó, escolha segura
#'   para iniciar a busca. Um número fixa outra fração. `NULL` não fixa o
#'   parâmetro e permite tuning somente se `mtry` aparecer em
#'   [TuneBoostTreeSearchSpace()].
#' @param max_bin `NULL` ou inteiro positivo. Número de bins usados por algoritmos
#'   histogram-based. Valores maiores preservam mais detalhe em variáveis
#'   contínuas e podem aumentar memória/tempo; valores menores são mais rápidos e
#'   podem regularizar.
#'
#' @return Lista validada com classe `tbtb_boost_params`. O objeto é uma lista
#'   comum com campos `trees`, `stop_iter`, `learn_rate`, `tree_depth`, `min_n`,
#'   `loss_reduction`, `sample_size`, `mtry` e `max_bin`, pronta para ser passada
#'   ao argumento `boost` de [TuneBoostTree()].
#' @export
TuneBoostTreeBoostParams <- function(trees = 500L, stop_iter = 20L, learn_rate = NULL, tree_depth = NULL, min_n = NULL, loss_reduction = NULL, sample_size = NULL, mtry = "default", max_bin = NULL) {

  if(is.character(mtry) && identical(mtry[1L], "default")){
    mtryValue <- "default"
  } else {
    mtryValue <- mtry
  }
  out <- list(trees = as.integer(trees), stop_iter = as.integer(stop_iter), learn_rate = learn_rate, tree_depth = tree_depth, min_n = min_n, loss_reduction = loss_reduction, sample_size = sample_size, mtry = mtryValue, max_bin = max_bin)
  if(length(out$trees) != 1L || is.na(out$trees) || out$trees < 1L) cli::cli_abort("`trees` must be a positive integer.")
  if(length(out$stop_iter) != 1L || is.na(out$stop_iter) || out$stop_iter < 1L) cli::cli_abort("`stop_iter` must be a positive integer.")
  if(is.character(out$mtry) && !identical(out$mtry, "default")) cli::cli_abort("`mtry` as character must be exactly 'default'.")
  class(out) <- c("tbtb_boost_params", "list")
  out
}
####
## Fim
#


#' Montar espaço de busca Bayesiano
#'
#' @description
#' Define os limites inferiores e superiores dos hiperparâmetros que serão
#' explorados pelo otimizador de [TuneBoostTree()]. O espaço de busca controla o
#' que varia; [TuneBoostTreeBoostParams()] controla o que fica fixo. Sempre que
#' possível, os nomes seguem `parsnip::boost_tree()` para facilitar migração de
#' fluxos tidymodels.
#'
#' @details
#' Cada argumento deve ser `NULL` ou um vetor de tamanho 2 no formato
#' `c(limite_inferior, limite_superior)`. O limite inferior precisa ser menor que
#' o superior. Parâmetros com `NULL` não são tunados. Parâmetros inteiros, como
#' `tree_depth`, `max_bin`, `num_leaves` e `min_data_in_leaf`, são amostrados em
#' escala numérica e arredondados antes do ajuste.
#'
#' Parâmetros com o mesmo nome fixados em [TuneBoostTreeBoostParams()] prevalecem
#' sobre o espaço de busca. Por exemplo, se `boost = TuneBoostTreeBoostParams(mtry
#' = "default")`, `mtry` fica em 0.8 mesmo que exista um limite de `mtry` aqui.
#'
#' @param learn_rate Vetor numérico de tamanho 2 em `(0, 1]`. Limites da taxa de
#'   aprendizado. Faixas mais baixas favorecem modelos mais estáveis e exigem
#'   mais árvores; faixas altas aceleram a busca, mas podem sobreajustar.
#' @param tree_depth Vetor numérico/inteiro de tamanho 2. Limites da profundidade
#'   máxima das árvores. Valores são arredondados. Profundidades maiores capturam
#'   interações mais complexas; profundidades menores regularizam e reduzem
#'   tempo.
#' @param min_n Vetor numérico de tamanho 2. Limites do tamanho/peso mínimo de
#'   nó/folha em nomenclatura parsnip. Mapeia para `min_child_weight` no XGBoost
#'   e `min_sum_hessian_in_leaf` no LightGBM. Valores maiores evitam folhas muito
#'   pequenas.
#' @param loss_reduction Vetor numérico de tamanho 2, não negativo. Limites da
#'   redução mínima de perda exigida para split. Mapeia para `gamma` no XGBoost e
#'   `min_gain_to_split` no LightGBM. Valores maiores reduzem complexidade.
#' @param sample_size Vetor numérico de tamanho 2 em `(0, 1]`. Limites da fração
#'   de linhas usada por iteração. Valores abaixo de 1 introduzem amostragem
#'   estocástica e podem melhorar generalização.
#' @param mtry `NULL` ou vetor numérico de tamanho 2 em `(0, 1]`. Limites da
#'   fração de features considerada. Use `NULL` para não tunar. Para fixar o
#'   padrão de 80%, use `mtry = "default"` em [TuneBoostTreeBoostParams()].
#' @param max_bin `NULL` ou vetor numérico/inteiro de tamanho 2. Limites do número
#'   de bins de histograma. Valores maiores preservam detalhes em variáveis
#'   contínuas; valores menores reduzem memória e tempo.
#' @param lambda `NULL` ou vetor numérico de tamanho 2, não negativo. Limites da
#'   regularização L2. Mapeia para `lambda` no XGBoost e `lambda_l2` no LightGBM.
#'   Valores maiores reduzem magnitude dos pesos das folhas.
#' @param alpha `NULL` ou vetor numérico de tamanho 2, não negativo. Limites da
#'   regularização L1. Mapeia para `alpha` no XGBoost e `lambda_l1` no LightGBM.
#'   Valores maiores podem induzir pesos de folhas mais esparsos.
#' @param max_delta_step `NULL` ou vetor numérico de tamanho 2, não negativo.
#'   Limites do estabilizador logístico do XGBoost, útil em desbalanceamento
#'   severo. É ignorado pelo LightGBM.
#' @param colsample_bytree `NULL` ou vetor numérico de tamanho 2 em `(0, 1]`.
#'   Limites da amostragem de features por árvore. Mapeia para
#'   `colsample_bytree` no XGBoost e `feature_fraction` no LightGBM.
#' @param colsample_bylevel `NULL` ou vetor numérico de tamanho 2 em `(0, 1]`.
#'   Limites da amostragem de features por nível no XGBoost. É ignorado pelo
#'   LightGBM.
#' @param num_leaves `NULL` ou vetor numérico/inteiro de tamanho 2. Limites do
#'   número máximo de folhas no LightGBM. Aumenta capacidade e risco de
#'   sobreajuste. É ignorado pelo XGBoost.
#' @param min_data_in_leaf `NULL` ou vetor numérico/inteiro de tamanho 2. Limites
#'   do mínimo de linhas por folha no LightGBM. Valores maiores regularizam folhas
#'   pequenas. É ignorado pelo XGBoost.
#' @param scale_pos_weight `NULL` ou vetor numérico positivo de tamanho 2.
#'   Limites do peso da classe positiva. Preferencialmente use
#'   [TuneBoostTreeImbalance()] com `scale_pos_weight = "auto"`; tune este peso
#'   apenas quando houver dados suficientes e validação robusta.
#'
#' @return Lista validada com classe `tbtb_search_space`. Cada elemento é um
#'   vetor numérico `c(lower, upper)` finito e crescente. Elementos `NULL` são
#'   removidos e não são otimizados.
#' @export
TuneBoostTreeSearchSpace <- function(learn_rate = c(0.01, 0.2), tree_depth = c(2L, 12L), min_n = c(1, 80), loss_reduction = c(0, 8), sample_size = c(0.55, 1), mtry = NULL, max_bin = NULL, lambda = NULL, alpha = NULL, max_delta_step = NULL, colsample_bytree = NULL, colsample_bylevel = NULL, num_leaves = NULL, min_data_in_leaf = NULL, scale_pos_weight = NULL) {

  out <- list(learn_rate = learn_rate, tree_depth = tree_depth, min_n = min_n, sample_size = sample_size, mtry = mtry, loss_reduction = loss_reduction, max_bin = max_bin, lambda = lambda, alpha = alpha, max_delta_step = max_delta_step, colsample_bytree = colsample_bytree, colsample_bylevel = colsample_bylevel, num_leaves = num_leaves, min_data_in_leaf = min_data_in_leaf, scale_pos_weight = scale_pos_weight)
  out <- out[!vapply(out, is.null, logical(1L))]
  for(parameterName in names(out)){
    value <- as.numeric(out[[parameterName]])
    if(length(value) != 2L || anyNA(value) || any(!is.finite(value)) || value[1L] >= value[2L]) cli::cli_abort("Search-space entry `{parameterName}` must contain finite increasing lower/upper bounds.")
    out[[parameterName]] <- value
  }
  for(fractionName in intersect(c("mtry", "sample_size", "colsample_bytree", "colsample_bylevel"), names(out))){
    if(out[[fractionName]][1L] <= 0 || out[[fractionName]][2L] > 1) cli::cli_abort("`{fractionName}` search bounds must be fractions in `(0, 1]`.")
  }
  if("scale_pos_weight" %in% names(out) && out$scale_pos_weight[1L] <= 0) cli::cli_abort("`scale_pos_weight` search bounds must be positive.")
  class(out) <- c("tbtb_search_space", "list")
  out
}
####
## Fim
#


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
#'   de forma aproximada. `FALSE` é rejeitado, não ignorado, para evitar uma
#'   configuração que poderia gerar folds sem positivos e distorcer PR-AUC,
#'   threshold e `scale_pos_weight` por fold.
#'
#' @return Lista validada com classe `tbtb_cv`, contendo `folds` e `stratified`.
#'   O valor de `folds` pode ser ajustado no objeto `config$cv` retornado por
#'   [TuneBoostTree()] quando a contagem da classe minoritária exigir redução.
#' @export
TuneBoostTreeCv <- function(folds = 10L, stratified = TRUE) {

  out <- list(folds = as.integer(folds), stratified = isTRUE(stratified))
  if(length(out$folds) != 1L || is.na(out$folds) || out$folds < 2L) cli::cli_abort("`folds` must be an integer greater than or equal to 2.")
  if(!out$stratified) cli::cli_abort("Only stratified binary folds are currently supported; use `stratified = TRUE`.")
  class(out) <- c("tbtb_cv", "list")
  out
}
####
## Fim
#


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
TuneBoostTreeOptimizerLimbo <- function(command = NULL, fallback = TRUE, acquisition = c("ucb", "ei", "poi"), kappa = 2.576, eps = 0) {

  command <- TuneBoostTree_ResolveLimboCommand(command)
  acquisition <- match.arg(acquisition)
  out <- list(type = "limbo", command = command, fallback = isTRUE(fallback), acquisition = acquisition, kappa = as.numeric(kappa)[1L], eps = as.numeric(eps)[1L])
  if(!is.finite(out$kappa) || !is.finite(out$eps)) cli::cli_abort("`kappa` and `eps` must be finite numerics.")
  class(out) <- c("tbtb_optimizer", "list")
  out
}
####
## Fim
#


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

  out <- list(type = "internal", command = NA_character_, fallback = TRUE, acquisition = "internal", kappa = 0, eps = 0)
  class(out) <- c("tbtb_optimizer", "list")
  out
}
####
## Fim
#


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
TuneBoostTreeOptimizerRBayesianOptimization <- function(acquisition = c("ucb", "ei", "poi"), kappa = 2.576, eps = 0, fallback = TRUE) {

  acquisition <- match.arg(acquisition)
  out <- list(type = "rBayesianOptimization", command = NA_character_, fallback = isTRUE(fallback), acquisition = acquisition, kappa = as.numeric(kappa)[1L], eps = as.numeric(eps)[1L])
  if(!is.finite(out$kappa) || !is.finite(out$eps)) cli::cli_abort("`kappa` and `eps` must be finite numerics.")
  class(out) <- c("tbtb_optimizer", "list")
  out
}
####
## Fim
#


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

  if(!is.null(balanceFn) && !is.function(balanceFn)) cli::cli_abort("`balanceFn` must be a function or `NULL`.")
  if(is.character(scale_pos_weight)){
    if(!identical(scale_pos_weight, "auto")) cli::cli_abort("`scale_pos_weight` must be `\"auto\"`, `NULL`, or a positive numeric scalar.")
  } else if(!is.null(scale_pos_weight)){
    scale_pos_weight <- as.numeric(scale_pos_weight)[1L]
    if(!is.finite(scale_pos_weight) || scale_pos_weight <= 0) cli::cli_abort("Numeric `scale_pos_weight` must be positive and finite.")
  }
  out <- list(balanceFn = balanceFn, scale_pos_weight = scale_pos_weight, balance_args = list(...))
  class(out) <- c("tbtb_imbalance", "list")
  out
}
####
## Fim
#


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
  backend <- match.arg(as.character(backend)[1L], c("auto", "c", "fortran", "rfast", "r"))
  out <- list(metric = metric, backend = backend)
  class(out) <- c("tbtb_performance", "list")
  out
}
####
## Fim
#


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

  out <- list(seed = as.integer(seed), parallel = parallel, verbose = verbose, fallback_trees = as.integer(fallback_trees))
  if(length(out$seed) != 1L || is.na(out$seed)) cli::cli_abort("`seed` must be a scalar integer.")
  if(length(out$fallback_trees) != 1L || is.na(out$fallback_trees) || out$fallback_trees < 1L) cli::cli_abort("`fallback_trees` must be a positive integer.")
  class(out) <- c("tbtb_control", "list")
  out
}
####
## Fim
#


#' Montar configuração explícita de paralelismo
#'
#' @description
#' Descreve como [TuneBoostTree()] divide CPU entre folds avaliados em paralelo e
#' threads internas da engine. Use este construtor quando `parallel = "auto"` em
#' [TuneBoostTreeControl()] não for específico o suficiente para o ambiente.
#'
#' @param workers Inteiro positivo ou `"auto"`. Número de folds avaliados
#'   simultaneamente. Valores maiores podem acelerar validação cruzada, mas cada
#'   worker treina um modelo e consome memória. Valores acima do número efetivo de
#'   folds são limitados automaticamente.
#' @param threads_per_worker Inteiro positivo ou `"auto"`. Número de threads de
#'   XGBoost/LightGBM alocado para cada worker. Aumentar este valor acelera cada
#'   fit individual, mas pode reduzir eficiência quando há muitos workers.
#' @param strategy Uma de `"auto"`, `"folds"`, `"engine"` ou `"sequential"`.
#'   `"folds"` prioriza vários folds simultâneos. `"engine"` prioriza um fold por
#'   vez com mais threads na engine. `"sequential"` desativa paralelismo entre
#'   folds. `"auto"` escolhe uma divisão balanceada.
#'
#' @return Lista validada com classe `tbtb_parallel`, contendo `workers`,
#'   `threads_per_worker` e `strategy`.
#' @export
TuneBoostTreeParallel <- function(workers = "auto", threads_per_worker = "auto", strategy = c("auto", "folds", "engine", "sequential")) {

  out <- list(workers = workers, threads_per_worker = threads_per_worker, strategy = match.arg(strategy))
  class(out) <- c("tbtb_parallel", "list")
  out
}
####
## Fim
#


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

  out <- list(name = "xgboost", eval_metric = as.character(eval_metric)[1L], tree_method = as.character(tree_method)[1L], feature_types = feature_types)
  if(!(out$eval_metric %in% c("aucpr", "auc"))) cli::cli_abort("`eval_metric` must be 'aucpr' or 'auc' for XGBoost.")
  class(out) <- c("tbtb_engine", "list")
  out
}
####
## Fim
#


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


#' Montar configuração ultraotimizada
#'
#' @description
#' Monta um conjunto opinativo de configurações de alto desempenho para
#' [TuneBoostTree()]. O perfil aumenta o orçamento de árvores, usa Limbo como
#' otimizador preferencial, mantém PR-AUC como métrica e ativa paralelismo
#' automático.
#'
#' @param command `NULL` ou caminho/comando do executável ask/tell do Limbo. O
#'   valor é repassado a [TuneBoostTreeOptimizerLimbo()].
#' @param strict_limbo Lógico escalar. `TRUE` define `fallback = FALSE` no
#'   otimizador Limbo, exigindo executável funcional. `FALSE` permite fallback
#'   para o otimizador interno quando Limbo não estiver disponível.
#'
#' @return Lista nomeada com os blocos `boost`, `searchSpace`, `cv`, `optimizer`,
#'   `imbalance`, `performance` e `control`, todos prontos para uso em
#'   [TuneBoostTree()].
#' @export
TuneBoostTreeUltraConfig <- function(command = NULL, strict_limbo = TRUE) {

  list(boost = TuneBoostTreeBoostParams(trees = 1000L, stop_iter = 30L, mtry = 1, max_bin = 256L), searchSpace = TuneBoostTreeSearchSpace(learn_rate = c(0.005, 0.2), tree_depth = c(2L, 12L), min_n = c(1, 80), loss_reduction = c(0, 8), sample_size = c(0.55, 1)), cv = TuneBoostTreeCv(folds = 10L), optimizer = TuneBoostTreeOptimizerLimbo(command = command, fallback = !isTRUE(strict_limbo), acquisition = "ucb", kappa = 2.576, eps = 0), imbalance = TuneBoostTreeImbalance(scale_pos_weight = "auto"), performance = TuneBoostTreePerformance(metric = "pr_auc", backend = "auto"), control = TuneBoostTreeControl(parallel = "auto", verbose = TRUE))
}
####
## Fim
#


#' Executar tuning ultraotimizado de boosted trees
#'
#' @description
#' Atalho que cria [TuneBoostTreeUltraConfig()] e chama [TuneBoostTree()] com essa
#' configuração. Use quando quiser o perfil de alto desempenho sem montar cada
#' bloco manualmente.
#'
#' @param formula Fórmula de duas faces para classificação binária, repassada a
#'   [TuneBoostTree()].
#' @param data data.frame, tibble ou data.table com as linhas de treino,
#'   repassado a [TuneBoostTree()].
#' @param initial Inteiro, `NULL` ou tibble/data.frame de warm-start, repassado a
#'   [TuneBoostTree()].
#' @param nIter Inteiro com número de iterações do otimizador após a
#'   inicialização.
#' @param engine `"xgboost"`, `"lightgbm"` ou configuração de engine criada por
#'   [TuneBoostTreeXgboost()] ou [TuneBoostTreeLightgbm()].
#' @param command Caminho/comando do executável ask/tell do Limbo, repassado a
#'   [TuneBoostTreeUltraConfig()].
#' @param strict_limbo Lógico escalar. `TRUE` exige Limbo funcional; `FALSE`
#'   permite fallback interno.
#'
#' @return O mesmo objeto `tbtb_tune_result` retornado por [TuneBoostTree()], com
#'   `bestHyperparameters`, `bestScore`, `bestThreshold`, `initial` (tibble),
#'   `evaluationLog` (tibble) e `config`.
#' @export
TuneBoostTreeBayesianUltra <- function(formula, data, initial = 20L, nIter = 60L, engine = "lightgbm", command = NULL, strict_limbo = TRUE) {

  ultra <- TuneBoostTreeUltraConfig(command = command, strict_limbo = strict_limbo)
  TuneBoostTree(formula = formula, data = data, initial = initial, nIter = nIter, engine = engine, boost = ultra$boost, searchSpace = ultra$searchSpace, cv = ultra$cv, optimizer = ultra$optimizer, imbalance = ultra$imbalance, performance = ultra$performance, control = ultra$control)
}
####
## Fim
#


#' Tunar hiperparâmetros de gradient boosted trees
#'
#' @description
#' Função principal do pacote. Executa tuning de boosted trees para classificação
#' binária com validação cruzada estratificada, `early stopping`, otimização
#' Bayesiana, tratamento opcional de desbalanceamento e engines XGBoost ou
#' LightGBM. A função atual e recomendada é `TuneBoostTree()`; o nome histórico
#' `TuneBoostTreeBayesian()` permanece como alias de compatibilidade.
#'
#' @details
#' A variável resposta deve ser fator com exatamente dois níveis. A classe
#' positiva é definida de forma determinística: o nível menos frequente é tratado
#' como positivo; em caso de empate, o primeiro nível em `levels(response)` é
#' tratado como positivo. Essa ordenação é guardada no resultado e reaproveitada
#' por funções de ajuste e predição.
#'
#' Todos os hiperparâmetros expostos seguem, quando possível, nomes de
#' `parsnip::boost_tree()`/tidymodels. Parâmetros específicos de engine, como
#' `lambda`, `alpha`, `num_leaves` e `colsample_bylevel`, aparecem somente onde
#' não há nome parsnip equivalente claro.
#'
#' @param formula Fórmula de duas faces com uma variável resposta binária e
#'   preditores numéricos, por exemplo `classe ~ x1 + x2`. A resposta precisa ser
#'   fator com dois níveis e sem valores ausentes.
#' @param data data.frame, tibble ou data.table não vazio contendo todas as linhas
#'   de treino e as colunas referenciadas por `formula`. Internamente a entrada é
#'   padronizada para `data.frame` antes da criação das matrizes de engine.
#' @param initial `NULL`, inteiro não negativo ou tabela de warm-start. Um inteiro
#'   solicita esse número de pontos iniciais aleatórios antes das iterações
#'   Bayesianas. Uma tabela deve conter colunas dos hiperparâmetros ativos e a
#'   coluna `Value`; o componente `initial` retornado por uma execução anterior é
#'   uma tibble pronta para reutilização. `NULL` equivale a nenhum ponto inicial
#'   adicional.
#' @param nIter Inteiro não negativo. Número de iterações de otimização após os
#'   pontos iniciais. `0` avalia apenas `initial`/grade inicial quando fornecida.
#' @param engine Texto `"lightgbm"`/`"xgboost"` ou configuração criada por
#'   [TuneBoostTreeLightgbm()] ou [TuneBoostTreeXgboost()]. Use os construtores
#'   quando precisar alterar métrica nativa, método de árvore ou metadados de
#'   features.
#' @param boost Configuração criada por [TuneBoostTreeBoostParams()]. Define
#'   valores fixos como `trees`, `stop_iter`, `mtry = "default"` e qualquer
#'   hiperparâmetro que não deva ser tunado.
#' @param searchSpace Espaço de busca criado por [TuneBoostTreeSearchSpace()].
#'   Define limites inferiores/superiores dos parâmetros que serão otimizados. O
#'   nome antigo `search_space` ainda é aceito em `...` apenas para
#'   compatibilidade; prefira `searchSpace`.
#' @param cv Configuração de validação cruzada criada por [TuneBoostTreeCv()].
#'   Controla `folds` e exige `stratified = TRUE`.
#' @param optimizer Configuração de otimizador criada por
#'   [TuneBoostTreeOptimizerRBayesianOptimization()],
#'   [TuneBoostTreeOptimizerLimbo()] ou [TuneBoostTreeInternalOptimizer()].
#'   Controla backend, função de aquisição (`"ucb"`, `"ei"`, `"poi"`), `kappa`,
#'   `eps` e política de fallback.
#' @param imbalance Configuração de desbalanceamento criada por
#'   [TuneBoostTreeImbalance()]. Controla a assinatura de `balanceFn`, argumentos
#'   repassados por `...` a essa função e `scale_pos_weight`.
#' @param performance Configuração de métrica criada por
#'   [TuneBoostTreePerformance()]. Atualmente otimiza `"pr_auc"` e permite
#'   escolher o backend de cálculo.
#' @param control Controles de execução criados por [TuneBoostTreeControl()].
#'   Define `seed`, `parallel`, `verbose` e `fallback_trees`.
#' @param ... Argumentos de compatibilidade. `search_space` é mapeado para
#'   `searchSpace`; qualquer outro nome é rejeitado para evitar erros silenciosos.
#'
#' @return Objeto de classe `tbtb_tune_result` (também uma lista) com:
#'
#'   - `bestHyperparameters`: lista com o melhor conjunto encontrado. Inclui
#'     parâmetros tunados, parâmetros fixos de `boost`, `trees` escolhido por
#'     `early stopping`, `stop_iter`, `eval_metric`, `scale_pos_weight` quando
#'     aplicável e `threshold` otimizado. É o principal objeto a passar para
#'     [FitBoostTreeModel()].
#'   - `bestScore`: número com o melhor PR-AUC médio de validação cruzada.
#'     Valores maiores indicam melhor desempenho no objetivo otimizado.
#'   - `bestThreshold`: lista com `threshold`, `metric` e `score`, calculada a
#'     partir de probabilidades out-of-fold após a escolha dos hiperparâmetros. O
#'     `threshold` também é copiado para `bestHyperparameters$threshold`.
#'   - `initial`: tibble de warm-start com os hiperparâmetros ativos e `Value`.
#'     Combina histórico recebido com avaliações novas, deduplica candidatos e
#'     pode ser reutilizada em outra chamada de `TuneBoostTree()`.
#'   - `evaluationLog`: tibble de auditoria da execução atual. Cada linha contém
#'     um candidato avaliado, `Value` como PR-AUC de validação cruzada e
#'     `bestIteration` como rodada efetiva selecionada por `early stopping`.
#'   - `config`: lista com as configurações resolvidas: `engine`, `boost`,
#'     `searchSpace`, `cv`, `optimizer`, `imbalance`, `performance`, `control` e
#'     `parallel`.
#'
#'   Todas as tabelas retornadas diretamente pela função (`initial` e
#'   `evaluationLog`) são tibbles.
#' @export
TuneBoostTree <- function(formula, data, initial = 10L, nIter = 30L, engine = "lightgbm", boost = TuneBoostTreeBoostParams(), searchSpace = TuneBoostTreeSearchSpace(), cv = TuneBoostTreeCv(), optimizer = TuneBoostTreeOptimizerRBayesianOptimization(), imbalance = TuneBoostTreeImbalance(), performance = TuneBoostTreePerformance(), control = TuneBoostTreeControl(), ...) {

  dots <- list(...)
  if("search_space" %in% names(dots)){
    if(!identical(searchSpace, TuneBoostTreeSearchSpace())) cli::cli_abort("Use only one of `searchSpace` or deprecated `search_space`.")
    searchSpace <- dots$search_space
  }
  unknownDots <- setdiff(names(dots), "search_space")
  if(length(unknownDots) > 0L) cli::cli_abort("Unknown argument(s): {paste(unknownDots, collapse = ', ')}")
  if(!inherits(formula, "formula") || length(formula) != 3L) cli::cli_abort("`formula` must be a two-sided formula.")
  if(!is.data.frame(data) || nrow(data) == 0L) cli::cli_abort("`data` must be a non-empty data.frame, tibble, or data.table.")
  data <- as.data.frame(data) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  engine <- TuneBoostTree_ResolveEngine(engine)
  boost <- TuneBoostTree_ResolveBoost(boost)
  bounds <- TuneBoostTree_ResolveSearchSpace(searchSpace, boost)
  cv <- TuneBoostTree_ResolveCv(cv)
  optimizer <- TuneBoostTree_ResolveOptimizer(optimizer)
  imbalance <- TuneBoostTree_ResolveImbalance(imbalance)
  performance <- TuneBoostTree_ResolvePerformance(performance)
  control <- TuneBoostTree_ResolveControl(control)
  TuneBoostTree_SetPassiveOpenMp()
  initialState <- TuneBoostTree_ResolveInitial(initial, bounds)
  initGridDt <- initialState$initGridDt
  initPoints <- initialState$initPoints
  nIter <- as.integer(nIter)
  if(length(nIter) != 1L || is.na(nIter) || nIter < 0L) cli::cli_abort("`nIter` must be a non-negative integer.")

  timerStart <- proc.time()[["elapsed"]]
  parameterNames <- names(bounds)
  nFolds <- cv$folds
  formulaInfoForTarget <- TuneBoostTree_ExtractFormulaInfo(formula, data)
  preparedTargetForCv <- TuneBoostTree_PrepareTarget(data[[formulaInfoForTarget$targetName]], NULL)
  nFolds <- TuneBoostTree_ValidateCvClassCounts(preparedTargetForCv$yData, nFolds)
  cv$folds <- nFolds
  nRoundsTuning <- boost$trees
  earlyStoppingRounds <- boost$stop_iter
  seed <- control$seed
  runtime <- TuneBoostTree_ResolveParallel(control$parallel, nrow(data), nFolds)
  nWorkersFolds <- runtime$workers
  workerThreads <- runtime$threads_per_worker
  prAucBackend <- TuneBoostTree_SelectPrAucBackend(performance$backend)
  engine_boost_tree <- engine$name
  evalMetric <- if(engine_boost_tree == "xgboost") engine$eval_metric else "average_precision"
  featureTypes <- engine$feature_types

  balanceFn <- imbalance$balanceFn
  useBalancedCv <- !is.null(balanceFn)
  if(useBalancedCv){
    balancedFolds <- TuneBoostTree_PrepareBalancedFolds(formula, data, nFolds, balanceFn, imbalance$balance_args, imbalance$scale_pos_weight, workerThreads, seed, engine_boost_tree, preparedTargetForCv$targetLevels)
    scalePosWeightValue <- NULL
  } else {
    formulaInfo <- formulaInfoForTarget
    preparedTrain <- TuneBoostTree_PrepareMatrix(formula, data, featureTypes, preparedTargetForCv$targetLevels, formulaInfo)
    classCounts <- table(preparedTrain$yData)
    if(length(classCounts) != 2L || any(classCounts == 0L)) cli::cli_abort("`data` must contain both binary classes.")
    scalePosWeightValue <- TuneBoostTree_ResolveScalePosWeight(preparedTrain$yData, imbalance$scale_pos_weight)
    folds <- TuneBoostTree_CreateStratifiedFolds(preparedTrain$yData, nFolds, seed)
    balancedFolds <- vector("list", length(folds))
    for(foldId in seq_along(folds)){
      testIndex <- folds[[foldId]]
      trainIndex <- setdiff(seq_len(nrow(data)), testIndex)
      trainMatrix <- preparedTrain$xMatrix[trainIndex, , drop = FALSE]
      testMatrix <- preparedTrain$xMatrix[testIndex, , drop = FALSE]
      dtrain <- TuneBoostTree_CreateDataObject(trainMatrix, preparedTrain$yData[trainIndex], preparedTrain$featureTypes, workerThreads, engine_boost_tree)
      dtest <- TuneBoostTree_CreateDataObject(testMatrix, preparedTrain$yData[testIndex], preparedTrain$featureTypes, workerThreads, engine_boost_tree)
      foldScalePosWeight <- TuneBoostTree_ResolveScalePosWeight(preparedTrain$yData[trainIndex], imbalance$scale_pos_weight)
      if(engine_boost_tree == "xgboost"){
        balancedFolds[[foldId]] <- list(dtrain = dtrain, dtest = dtest, yTest = preparedTrain$yData[testIndex], scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels)
      } else {
        balancedFolds[[foldId]] <- list(dstrain = dtrain, dstest = dtest, xTest = TuneBoostTree_AsPredictionMatrix(testMatrix), yTest = preparedTrain$yData[testIndex], scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels)
      }
    }
  }

  cacheEnv <- new.env(parent = emptyenv())
  on.exit({
    if(exists("cacheEnv", inherits = FALSE) && is.environment(cacheEnv)){
      rm(list = ls(envir = cacheEnv, all.names = TRUE), envir = cacheEnv)
    }
  }, add = TRUE)
  evaluationLogList <- vector("list", max(64L, as.integer(initPoints) + as.integer(nIter) + 32L))
  logIndex <- 0L
  objective <- TuneBoostTree_EvaluateCv
  environment(objective) <- environment()

  if(isTRUE(control$verbose)) cli::cli_inform("Starting {.val {engine_boost_tree}} Bayesian tuning with {.val {nRoundsTuning}} trees, {.val {earlyStoppingRounds}} stop_iter, and {.val {nWorkersFolds}} fold worker(s).")
  set.seed(seed)
  tuningResult <- TuneBoostTree_RunOptimizer(objective = objective, bounds = bounds, initGridDt = initGridDt, initPoints = initPoints, nIter = nIter, acq = optimizer$acquisition, kappa = optimizer$kappa, eps = optimizer$eps, verbose = control$verbose, seed = seed, optimizerBackend = optimizer$type, limboCommand = optimizer$command, limboFallback = optimizer$fallback)

  evaluationLog <- if(logIndex > 0L) TuneBoostTree_AsTibble(data.table::rbindlist(evaluationLogList[seq_len(logIndex)], fill = TRUE)) else tibble::tibble()
  bestHyperparameters <- as.list(tuningResult$Best_Par)
  fixedBoostNames <- setdiff(names(boost)[!vapply(boost, is.null, logical(1L))], c("trees", "stop_iter"))
  for(fixedName in setdiff(fixedBoostNames, names(bestHyperparameters))) bestHyperparameters[[fixedName]] <- boost[[fixedName]]
  bestScore <- as.numeric(tuningResult$Best_Value)
  bestIteration <- TuneBoostTree_FindBestIteration(evaluationLog, bestHyperparameters, bestScore, bounds)
  if(is.null(bestIteration)){
    bestSummary <- TuneBoostTree_RunCvManual(balancedFolds, bestHyperparameters, nRoundsTuning, earlyStoppingRounds, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend)
    bestIteration <- as.integer(bestSummary$bestIteration)
  }
  if(is.null(bestIteration) || is.na(bestIteration) || bestIteration < 1L) bestIteration <- as.integer(control$fallback_trees)
  bestHyperparameters$trees <- as.integer(bestIteration)
  bestHyperparameters$stop_iter <- as.integer(earlyStoppingRounds)
  bestHyperparameters$eval_metric <- evalMetric
  if(!useBalancedCv && is.null(bestHyperparameters$scale_pos_weight)) bestHyperparameters$scale_pos_weight <- scalePosWeightValue
  bestThresholdSummary <- TuneBoostTree_OptimizeThresholdCv(balancedFolds, bestHyperparameters, bestHyperparameters$trees, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend)
  bestHyperparameters$threshold <- as.numeric(bestThresholdSummary$threshold)

  newInitGridDt <- TuneBoostTree_CreateInitGrid(evaluationLog, bounds)
  returnedInitGridDt <- TuneBoostTree_AsTibble(TuneBoostTree_CombineInitGrid(initGridDt, newInitGridDt, bounds))
  if(isTRUE(control$verbose)) cli::cli_inform("Finished Bayesian tuning in {.val {round(proc.time()[['elapsed']] - timerStart, 2)}} seconds.")

  out <- list(bestHyperparameters = bestHyperparameters, bestScore = bestScore, bestThreshold = bestThresholdSummary, initial = returnedInitGridDt, evaluationLog = evaluationLog, config = list(engine = engine, boost = boost, searchSpace = bounds, cv = cv, optimizer = optimizer, imbalance = imbalance, performance = performance, control = control, parallel = runtime))
  class(out) <- c("tbtb_tune_result", "list")
  out
}
####
## Fim
#

#' @rdname TuneBoostTree
#' @export
TuneBoostTreeBayesian <- TuneBoostTree
####
## Fim
#

#' @rdname TuneBoostTreeOptimizerLimbo
#' @export
TuneBoostTreeLimbo <- TuneBoostTreeOptimizerLimbo

#' @rdname TuneBoostTreeOptimizerRBayesianOptimization
#' @export
TuneBoostTreeRBayesianOptimization <- TuneBoostTreeOptimizerRBayesianOptimization

#' Resolver configuração da engine
#' @noRd
TuneBoostTree_ResolveEngine <- function(engine) {

  if(is.character(engine)){
    engineName <- match.arg(as.character(engine)[1L], c("xgboost", "lightgbm"))
    return(if(engineName == "xgboost") TuneBoostTreeXgboost() else TuneBoostTreeLightgbm())
  }
  if(!is.list(engine) || is.null(engine$name) || !(engine$name %in% c("xgboost", "lightgbm"))) cli::cli_abort("`engine` must be 'xgboost', 'lightgbm', or a TuneBoostTree engine configuration.")
  engine
}
####
## Fim
#

#' Resolver configuração de boosting
#' @noRd
TuneBoostTree_ResolveBoost <- function(boost) {

  if(is.null(boost)) boost <- TuneBoostTreeBoostParams()
  if(!is.list(boost)) cli::cli_abort("`boost` must be created by `TuneBoostTreeBoostParams()` or be a compatible list.")
  defaults <- TuneBoostTreeBoostParams()
  defaults[names(boost)] <- boost
  TuneBoostTreeBoostParams(trees = defaults$trees, stop_iter = defaults$stop_iter, learn_rate = defaults$learn_rate, tree_depth = defaults$tree_depth, min_n = defaults$min_n, loss_reduction = defaults$loss_reduction, sample_size = defaults$sample_size, mtry = defaults$mtry, max_bin = defaults$max_bin)
}
####
## Fim
#

#' Resolver espaço de busca
#' @noRd
TuneBoostTree_ResolveSearchSpace <- function(search_space, boost) {

  if(is.null(search_space)) search_space <- TuneBoostTreeSearchSpace()
  if(!is.list(search_space)) cli::cli_abort("`searchSpace` must be created by `TuneBoostTreeSearchSpace()` or be a compatible list.")
  defaults <- TuneBoostTreeSearchSpace()
  defaults[names(search_space)] <- search_space
  bounds <- do.call(TuneBoostTreeSearchSpace, defaults)
  fixedNames <- intersect(names(boost), names(bounds))[!vapply(boost[intersect(names(boost), names(bounds))], is.null, logical(1L))]
  for(parameterName in fixedNames){
    if(identical(boost[[parameterName]], "default")) next
    value <- as.numeric(boost[[parameterName]])[1L]
    if(!is.finite(value)) cli::cli_abort("Fixed boost parameter `{parameterName}` must be finite.")
    bounds[[parameterName]] <- c(value, value)
  }
  bounds
}
####
## Fim
#

#' Resolver configuração de validação cruzada
#' @noRd
TuneBoostTree_ResolveCv <- function(cv) {

  if(is.null(cv)) cv <- TuneBoostTreeCv()
  if(!is.list(cv)) cli::cli_abort("`cv` must be created by `TuneBoostTreeCv()` or be a compatible list.")
  defaults <- TuneBoostTreeCv()
  defaults[names(cv)] <- cv
  TuneBoostTreeCv(folds = defaults$folds, stratified = defaults$stratified)
}
####
## Fim
#

#' Resolver localização do comando Limbo
#' @noRd
TuneBoostTree_ResolveLimboCommand <- function(command = NULL) {

  if(!is.null(command)){
    command <- as.character(command)
    if(length(command) != 1L) cli::cli_abort("`command` must be `NULL` or a single executable path/command.")
    return(if(is.na(command) || !nzchar(command)) NA_character_ else command)
  }
  envCommand <- Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_)
  if(!is.na(envCommand) && nzchar(envCommand)) return(envCommand)
  executableName <- if(.Platform$OS.type == "windows") "tbtb-limbo-ask.exe" else "tbtb-limbo-ask"
  pkgCommand <- system.file("bin", executableName, package = "TuneBoostTreeBayesian")
  if(nzchar(pkgCommand)) return(pkgCommand)
  NA_character_
}
####
## Fim
#

#' Verificar comando executável
#' @noRd
TuneBoostTree_IsExecutableCommand <- function(command) {

  command <- as.character(command)[1L]
  if(is.na(command) || !nzchar(command)) return(FALSE)
  command <- path.expand(command)
  if(grepl(.Platform$file.sep, command, fixed = TRUE) || grepl("/", command, fixed = TRUE)) return(file.exists(command) && file.access(command, mode = 1L) == 0L)
  nzchar(Sys.which(command))
}
####
## Fim
#

#' Resolver configuração do otimizador
#' @noRd
TuneBoostTree_ResolveOptimizer <- function(optimizer) {

  if(is.null(optimizer)) optimizer <- TuneBoostTreeOptimizerRBayesianOptimization()
  if(is.character(optimizer)){
    optimizerName <- as.character(optimizer[1L])
    if(identical(optimizerName, "internal")){
      optimizer <- TuneBoostTreeInternalOptimizer()
    } else if(identical(optimizerName, "rBayesianOptimization")){
      optimizer <- TuneBoostTreeOptimizerRBayesianOptimization()
    } else if(identical(optimizerName, "limbo")){
      optimizer <- TuneBoostTreeOptimizerLimbo()
    } else {
      cli::cli_abort("`optimizer` as character must be one of 'internal', 'rBayesianOptimization', or 'limbo'.")
    }
  }
  if(!is.list(optimizer) || is.null(optimizer$type)) cli::cli_abort("`optimizer` must be created by `TuneBoostTreeOptimizerLimbo()`, `TuneBoostTreeOptimizerRBayesianOptimization()`, or `TuneBoostTreeInternalOptimizer()`.")
  if(!(optimizer$type %in% c("limbo", "internal", "rBayesianOptimization"))) cli::cli_abort("Unsupported optimizer type: {optimizer$type}")
  optimizer
}
####
## Fim
#

#' Resolver configuração de desbalanceamento
#' @noRd
TuneBoostTree_ResolveImbalance <- function(imbalance) {

  if(is.null(imbalance)) imbalance <- TuneBoostTreeImbalance()
  if(!is.list(imbalance)) cli::cli_abort("`imbalance` must be created by `TuneBoostTreeImbalance()` or be a compatible list.")
  args <- if(is.null(imbalance$balance_args)) list() else imbalance$balance_args
  if(is.null(imbalance$balanceFn) && !is.null(imbalance$balance_fn)){
    cli::cli_warn("`balance_fn` is deprecated; use `balanceFn` instead.")
    imbalance$balanceFn <- imbalance$balance_fn
  }
  do.call(TuneBoostTreeImbalance, c(list(balanceFn = imbalance$balanceFn, scale_pos_weight = imbalance$scale_pos_weight), args))
}
####
## Fim
#

#' Resolver configuração de performance
#' @noRd
TuneBoostTree_ResolvePerformance <- function(performance) {

  if(is.null(performance)) performance <- TuneBoostTreePerformance()
  if(!is.list(performance)) cli::cli_abort("`performance` must be created by `TuneBoostTreePerformance()` or be a compatible list.")
  defaults <- TuneBoostTreePerformance()
  defaults[names(performance)] <- performance
  TuneBoostTreePerformance(metric = defaults$metric, backend = defaults$backend)
}
####
## Fim
#

#' Resolver controle de execução
#' @noRd
TuneBoostTree_ResolveControl <- function(control) {

  if(is.null(control)) control <- TuneBoostTreeControl()
  if(!is.list(control)) cli::cli_abort("`control` must be created by `TuneBoostTreeControl()` or be a compatible list.")
  defaults <- TuneBoostTreeControl()
  defaults[names(control)] <- control
  TuneBoostTreeControl(seed = defaults$seed, parallel = defaults$parallel, verbose = defaults$verbose, fallback_trees = defaults$fallback_trees)
}
####
## Fim
#

#' Resolver estado inicial do otimizador
#' @noRd
TuneBoostTree_ResolveInitial <- function(initial, bounds) {

  if(is.null(initial)) return(list(initGridDt = NULL, initPoints = 0L))
  if(is.list(initial) && !is.data.frame(initial) && !is.null(initial$initial)) initial <- initial$initial
  if(is.data.frame(initial)) return(list(initGridDt = TuneBoostTree_DeduplicateInitGrid(initial, bounds), initPoints = 0L))
  if(is.numeric(initial) && length(initial) == 1L && is.finite(initial) && initial >= 0) return(list(initGridDt = NULL, initPoints = as.integer(initial)))
  cli::cli_abort("`initial` must be `NULL`, a non-negative integer, or a data.frame/tibble/data.table warm-start grid.")
}
####
## Fim
#

#' Detectar orçamento físico de CPU
#' @noRd
TuneBoostTree_DetectCpuBudget <- function() {

  physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  logical <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if(is.na(physical) || physical < 1L) physical <- logical
  if(is.na(physical) || physical < 1L) physical <- 1L
  reserve <- min(2L, max(0L, as.integer(physical) - 1L))
  as.integer(max(1L, as.integer(physical) - reserve))
}
####
## Fim
#

#' Configurar espera passiva para OpenMP quando o usuário não definiu política
#' @noRd
TuneBoostTree_SetPassiveOpenMp <- function() {

  if(!nzchar(Sys.getenv("OMP_WAIT_POLICY", unset = ""))) Sys.setenv(OMP_WAIT_POLICY = "passive")
  if(!nzchar(Sys.getenv("GOMP_SPINCOUNT", unset = ""))) Sys.setenv(GOMP_SPINCOUNT = "0")
  invisible(TRUE)
}
####
## Fim
#

#' Preparar matriz de predição preservando representação esparsa
#' @noRd
TuneBoostTree_AsPredictionMatrix <- function(xMatrix) {

  if(inherits(xMatrix, "sparseMatrix")) return(xMatrix)
  as.matrix(xMatrix)
}
####
## Fim
#

#' Resolver execução paralela
#' @noRd
TuneBoostTree_ResolveParallel <- function(parallel, nRows, nFolds) {

  totalCores <- TuneBoostTree_DetectCpuBudget()
  if(isFALSE(parallel) || identical(parallel, "sequential")) return(list(workers = 1L, threads_per_worker = totalCores))
  if(is.character(parallel) && identical(parallel[1L], "auto")){
    workers <- if(nRows < 1000L) 1L else min(as.integer(nFolds), max(1L, floor(totalCores / 2L)))
    threads <- max(1L, floor(totalCores / workers))
    return(TuneBoostTree_FinalizeParallel(workers, threads, nFolds, totalCores))
  }
  if(is.list(parallel)){
    strategy <- if(is.null(parallel$strategy)) "auto" else as.character(parallel$strategy)[1L]
    if(strategy == "sequential") return(TuneBoostTree_FinalizeParallel(1L, totalCores, nFolds, totalCores))
    if(strategy == "engine") return(TuneBoostTree_FinalizeParallel(1L, totalCores, nFolds, totalCores))
    workers <- if(identical(parallel$workers, "auto")) min(as.integer(nFolds), max(1L, floor(totalCores / 2L))) else as.integer(parallel$workers)
    if(length(workers) != 1L || is.na(workers) || workers < 1L) cli::cli_abort("Parallel `workers` must be positive or 'auto'.")
    threads <- if(identical(parallel$threads_per_worker, "auto")) max(1L, floor(totalCores / workers)) else as.integer(parallel$threads_per_worker)
    if(length(threads) != 1L || is.na(threads) || threads < 1L) cli::cli_abort("Parallel `threads_per_worker` must be positive or 'auto'.")
    return(TuneBoostTree_FinalizeParallel(workers, threads, nFolds, totalCores))
  }
  cli::cli_abort("`parallel` must be 'auto', FALSE, 'sequential', or `TuneBoostTreeParallel()`.")
}
####
## Fim
#

#' Finalizar configuração paralela e avisar sobre oversubscription manual
#' @noRd
TuneBoostTree_FinalizeParallel <- function(workers, threads, nFolds, totalCores) {

  workers <- min(as.integer(workers), as.integer(nFolds))
  threads <- as.integer(threads)
  requestedThreads <- as.numeric(workers) * as.numeric(threads)
  oversubscriptionLimit <- as.numeric(totalCores) * 2
  if(is.finite(requestedThreads) && requestedThreads > oversubscriptionLimit){
    cli::cli_warn("workers ({workers}) * threads_per_worker ({threads}) = {requestedThreads} exceeds 2x the detected CPU budget ({totalCores}); consider reducing them to avoid oversubscription.")
  }
  list(workers = as.integer(workers), threads_per_worker = threads)
}
####
## Fim
#

#' Resolver peso da classe positiva
#' @noRd
TuneBoostTree_ResolveScalePosWeight <- function(yData, scale_pos_weight) {

  if(is.null(scale_pos_weight)) return(NULL)
  if(is.character(scale_pos_weight) && identical(scale_pos_weight, "auto")){
    classCounts <- table(as.integer(yData))
    if(length(classCounts) != 2L || any(classCounts == 0L)) return(NULL)
    return(as.numeric(classCounts[["0"]] / classCounts[["1"]]))
  }
  as.numeric(scale_pos_weight)[1L]
}
####
## Fim
#



#' Validar contagens de classes da validação cruzada
#' @noRd
TuneBoostTree_ValidateCvClassCounts <- function(yData, nFolds) {

  classCounts <- table(as.integer(yData))
  if(length(classCounts) != 2L || any(classCounts == 0L)) cli::cli_abort("`data` must contain both binary classes.")
  minClassCount <- min(as.integer(classCounts))
  if(minClassCount < 2L) cli::cli_abort("The minority class must contain at least two observations for cross-validation.")
  if(minClassCount < as.integer(nFolds)){
    cli::cli_warn("Minority class has {.val {minClassCount}} observation(s), fewer than requested {.val {nFolds}} fold(s); using {.val {minClassCount}} fold(s) so every validation fold contains both classes.")
    return(as.integer(minClassCount))
  }
  as.integer(nFolds)
}
####
## Fim
#

#' Extrair metadados da fórmula
#'
#' @param formula Fórmula de modelo de duas faces.
#' @param data data.frame contendo todas as colunas referenciadas.
#'
#' @details Auxiliar interno que centraliza a análise da fórmula para que preparação de matriz e predição usem a mesma ordem de features.
#'
#' @return Lista com nome do alvo, nomes dos preditores e objeto de termos.
#' @noRd
TuneBoostTree_ExtractFormulaInfo <- function(formula, data) {

  targetName <- all.vars(formula[[2L]])[1L] # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  termsValue <- terms(formula, data = data) # Objetivo: centralizar a leitura da fórmula para manter a mesma ordem de preditores em todo o fluxo.
  predictorNames <- attr(termsValue, "term.labels") # Objetivo: centralizar a leitura da fórmula para manter a mesma ordem de preditores em todo o fluxo.
  list(targetName = targetName, predictorNames = predictorNames, termsValue = termsValue) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
}
####
## Fim
#

#' Preparar alvo binário
#'
#' @param targetData Vetor de desfecho dos dados de treino.
#' @param targetLevels Ordenação opcional de dois níveis, na qual o segundo nível é positivo.
#'
#' @details Auxiliar interno que mapeia rótulos binários para 0/1 preservando os nomes das classes para a predição.
#'
#' @return Lista com alvo numérico, níveis, classe negativa e classe positiva.
#' @noRd
TuneBoostTree_PrepareTarget <- function(targetData, targetLevels = NULL) {

  if(!is.factor(targetData)) cli::cli_abort("The dependent/target variable must be a factor with exactly two levels.")
  observedLevels <- levels(targetData)
  if(length(observedLevels) != 2L) cli::cli_abort("The dependent/target factor must have exactly two levels.")
  if(anyNA(targetData)) cli::cli_abort("The dependent/target factor must not contain missing values.")
  if(is.null(targetLevels)){
    classCounts <- table(targetData)
    levelCounts <- as.integer(classCounts[observedLevels])
    if(any(levelCounts == 0L)) cli::cli_abort("The dependent/target factor must contain observations for both levels.")
    positiveClass <- if(levelCounts[1L] <= levelCounts[2L]) observedLevels[1L] else observedLevels[2L]
    negativeClass <- setdiff(observedLevels, positiveClass)[1L]
    targetLevels <- c(negativeClass, positiveClass)
  }
  targetLevels <- as.character(targetLevels)
  if(length(targetLevels) != 2L || anyNA(targetLevels) || !setequal(targetLevels, observedLevels)) cli::cli_abort("`targetLevels` must contain the two factor levels of the target.")
  positiveClass <- targetLevels[2L]
  negativeClass <- targetLevels[1L]
  yData <- as.integer(as.character(targetData) == positiveClass)
  list(yData = yData, targetLevels = targetLevels, negativeClass = negativeClass, positiveClass = positiveClass)
}
####
## Fim
#

#' Preparar matriz numérica de preditores
#'
#' @param formula Fórmula de duas faces.
#' @param data data.frame contendo desfecho e preditores.
#' @param featureTypes Vetor opcional de tipos de features do XGBoost.
#' @param targetLevels Ordenação opcional dos níveis do alvo binário.
#' @param formulaInfo Metadados da fórmula já analisados por `TuneBoostTree_ExtractFormulaInfo`.
#'
#' @details Converte preditores numéricos para matriz double e usa armazenamento esparso apenas quando a entrada é altamente esparsa.
#'
#' @return Lista com matriz, alvo, metadados de features, classes e fórmula.
#' @noRd
TuneBoostTree_PrepareMatrix <- function(formula, data, featureTypes = NULL, targetLevels = NULL, formulaInfo = NULL) {

  if(is.null(formulaInfo)) formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, data) # Objetivo: centralizar a leitura da fórmula para manter a mesma ordem de preditores em todo o fluxo.
  featureNames <- formulaInfo$predictorNames # Objetivo: centralizar a leitura da fórmula para manter a mesma ordem de preditores em todo o fluxo.
  dataFrame <- as.data.frame(data) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  xData <- dataFrame[, featureNames, drop = FALSE] # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  sparseLike <- vapply(xData, TuneBoostTree_IsSparseLikeColumn, logical(1L)) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
  numericMatrix <- data.matrix(xData) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
  storage.mode(numericMatrix) <- "double" # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
  colnames(numericMatrix) <- featureNames # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  xMatrix <- if(any(sparseLike) || mean(numericMatrix == 0) > 0.7) Matrix::Matrix(numericMatrix, sparse = TRUE) else numericMatrix # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
  preparedTarget <- TuneBoostTree_PrepareTarget(dataFrame[[formulaInfo$targetName]], targetLevels) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  if(!is.null(featureTypes)) names(featureTypes) <- featureNames # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  list(xMatrix = xMatrix, yData = preparedTarget$yData, featureNames = featureNames, featureTypes = featureTypes, targetLevels = preparedTarget$targetLevels, targetName = formulaInfo$targetName, negativeClass = preparedTarget$negativeClass, positiveClass = preparedTarget$positiveClass, formulaInfo = formulaInfo) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
}
####
## Fim
#

#' Detectar colunas com perfil esparso
#' @noRd
TuneBoostTree_IsSparseLikeColumn <- function(column) {

  any(c("sparsevctrs_vctr", "sparse_vector", "sparse_double", "sparse_integer") %in% class(column))
}
####
## Fim
#

#' Criar objeto de dados da engine
#'
#' @param xMatrix Matriz numérica ou `dgCMatrix` esparsa de preditores.
#' @param yData Vetor numérico opcional do alvo.
#' @param featureTypes Vetor opcional de tipos de features do XGBoost.
#' @param nThreads Inteiro com threads da engine para construção dos dados.
#' @param engine_boost_tree Nome da engine, `"xgboost"` ou `"lightgbm"`.
#'
#' @details Esta é a única função interna que constrói objetos de dados nativos das engines.
#'
#' @return Um `xgb.DMatrix` para XGBoost ou `lgb.Dataset` para LightGBM.
#' @noRd
TuneBoostTree_CreateDataObject <- function(xMatrix, yData = NULL, featureTypes = NULL, nThreads = 1L, engine_boost_tree = "xgboost") {

  if(engine_boost_tree == "xgboost"){
    args <- list(data = xMatrix, nthread = as.integer(nThreads)) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
    if(!is.null(yData)) args$label <- yData # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
    if(!is.null(featureTypes)) args$feature_types <- unname(featureTypes) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
    return(do.call(xgboost::xgb.DMatrix, args)) # Objetivo: isolar a conversão para objetos nativos das engines e reutilizar o mesmo contrato no treino, CV e predição.
  }
  lightgbm::lgb.Dataset(data = xMatrix, label = yData) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
}
####
## Fim
#

#' Criar folds estratificados
#'
#' @param yData Vetor alvo inteiro 0/1.
#' @param nFolds Inteiro com número de folds.
#' @param seed Inteiro usado como semente aleatória.
#'
#' @details Divisor interno de folds que preserva a lógica original de alocação alternada por classe.
#'
#' @return Lista de vetores inteiros com índices de teste.
#' @noRd
TuneBoostTree_CreateStratifiedFolds <- function(yData, nFolds = 10L, seed = 42L) {

  set.seed(seed) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  negativeIndex <- sample(which(as.integer(yData) == 0L)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  positiveIndex <- sample(which(as.integer(yData) == 1L)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  folds <- vector("list", as.integer(nFolds)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  for(foldId in seq_len(as.integer(nFolds))){
    folds[[foldId]] <- c(negativeIndex[seq(foldId, length(negativeIndex), by = nFolds)], positiveIndex[seq(foldId, length(positiveIndex), by = nFolds)]) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  }
  folds # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
}
####
## Fim
#

#' Obter limites Bayesianos padrão
#'
#' @details Auxiliar interno que retorna o espaço de busca canônico compartilhado pelas engines.
#'
#' @return Lista nomeada de limites numéricos.
#' @noRd
TuneBoostTree_GetDefaultBounds <- function() {

  TuneBoostTreeSearchSpace() # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
}
####
## Fim
#


#' Ler hiperparâmetro opcional
#' @noRd
TuneBoostTree_GetHyperparameter <- function(hyperparameters, parameterName, default = NULL) {

  if(!parameterName %in% names(hyperparameters) || is.null(hyperparameters[[parameterName]])) return(default)
  value <- as.numeric(hyperparameters[[parameterName]])[1L]
  if(!is.finite(value)) return(default)
  value
}
####
## Fim
#

#' Converter tabelas retornadas em tibbles
#' @noRd
TuneBoostTree_AsTibble <- function(x) {

  if(is.null(x)) return(NULL)
  tibble::as_tibble(x)
}
####
## Fim
#

#' Montar parâmetros da engine
#'
#' @param hyperparameters Lista nomeada de hiperparâmetros canônicos do tuner.
#' @param nThreads Inteiro com threads atribuídas ao ajuste do modelo.
#' @param scalePosWeight Numeric positive-class weight.
#' @param seed Inteiro usado como semente aleatória.
#' @param evalMetric Nome da métrica de avaliação do XGBoost.
#' @param engine_boost_tree Nome da engine, `"xgboost"` ou `"lightgbm"`.
#'
#' @details Traduz nomes canônicos de parâmetros para listas específicas de cada engine.
#'
#' @return Lista nomeada pronta para `xgb.train` ou `lgb.train`.
#' @noRd
TuneBoostTree_BuildParams <- function(hyperparameters, nThreads = 1L, scalePosWeight = NULL, seed = 42L, evalMetric = "aucpr", engine_boost_tree = "xgboost") {

  learnRateValue <- as.numeric(hyperparameters[["learn_rate"]]) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  treeDepthValue <- as.integer(round(as.numeric(hyperparameters[["tree_depth"]]))) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  minNValue <- as.numeric(hyperparameters[["min_n"]]) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  sampleSizeValue <- as.numeric(hyperparameters[["sample_size"]]) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  mtryRaw <- hyperparameters[["mtry"]]
  mtryValue <- if(is.null(mtryRaw) || (is.character(mtryRaw) && identical(mtryRaw[1L], "default"))) 0.8 else as.numeric(mtryRaw)[1L] # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  lossReductionValue <- as.numeric(hyperparameters[["loss_reduction"]]) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  maxBinValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "max_bin", 255L)
  maxBinValue <- as.integer(round(maxBinValue)) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  lambdaValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "lambda", NULL)
  alphaValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "alpha", NULL)
  maxDeltaStepValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "max_delta_step", NULL)
  colsampleBytreeValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "colsample_bytree", NULL)
  colsampleBylevelValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "colsample_bylevel", NULL)
  numLeavesValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "num_leaves", NULL)
  minDataInLeafValue <- TuneBoostTree_GetHyperparameter(hyperparameters, "min_data_in_leaf", NULL)
  tunedScalePosWeight <- TuneBoostTree_GetHyperparameter(hyperparameters, "scale_pos_weight", NULL)
  scalePosWeight <- if(!is.null(tunedScalePosWeight)) tunedScalePosWeight else scalePosWeight
  scalePosWeight <- if(is.null(scalePosWeight)) NULL else as.numeric(scalePosWeight)[1L]
  if(engine_boost_tree == "xgboost"){
    params <- list(objective = "binary:logistic", eval_metric = evalMetric, grow_policy = "depthwise", tree_method = "hist", max_bin = maxBinValue, max_depth = treeDepthValue, eta = learnRateValue, gamma = lossReductionValue, subsample = sampleSizeValue, min_child_weight = minNValue, colsample_bynode = mtryValue, nthread = as.integer(nThreads), seed = as.integer(seed))
    if(!is.null(lambdaValue)) params$lambda <- as.numeric(lambdaValue)
    if(!is.null(alphaValue)) params$alpha <- as.numeric(alphaValue)
    if(!is.null(maxDeltaStepValue)) params$max_delta_step <- as.numeric(maxDeltaStepValue)
    if(!is.null(colsampleBytreeValue)) params$colsample_bytree <- as.numeric(colsampleBytreeValue)
    if(!is.null(colsampleBylevelValue)) params$colsample_bylevel <- as.numeric(colsampleBylevelValue)
    if(!is.null(scalePosWeight)) params$scale_pos_weight <- scalePosWeight
    return(params)
  }
  params <- list(objective = "binary", boosting = "gbdt", metric = "average_precision", max_bin = maxBinValue, max_depth = treeDepthValue, learning_rate = learnRateValue, min_gain_to_split = lossReductionValue, bagging_fraction = sampleSizeValue, bagging_freq = 1L, min_sum_hessian_in_leaf = minNValue, feature_fraction_bynode = mtryValue, num_threads = as.integer(nThreads), seed = as.integer(seed), verbosity = -1L, verbose = -1L)
  if(!is.null(lambdaValue)) params$lambda_l2 <- as.numeric(lambdaValue)
  if(!is.null(alphaValue)) params$lambda_l1 <- as.numeric(alphaValue)
  if(!is.null(colsampleBytreeValue)) params$feature_fraction <- as.numeric(colsampleBytreeValue)
  if(!is.null(numLeavesValue)) params$num_leaves <- as.integer(round(as.numeric(numLeavesValue)))
  if(!is.null(minDataInLeafValue)) params$min_data_in_leaf <- as.integer(round(as.numeric(minDataInLeafValue)))
  if(!is.null(scalePosWeight)) params$scale_pos_weight <- scalePosWeight
  params
}
####
## Fim
#

#' Preparar folds balanceados
#'
#' @param formula Fórmula de duas faces.
#' @param data Full training data.frame.
#' @param nFolds Inteiro com número de folds.
#' @param balanceFn Função aplicada uma vez a cada partição de treino.
#' @param balanceArgs Argumentos extras repassados apenas para `balanceFn`.
#' @param scalePosWeightSetting Política de peso do fold: numérica, `"auto"` ou `NULL`.
#' @param nThreads Inteiro com threads para construção dos dados da engine.
#' @param seed Inteiro usado como semente aleatória.
#' @param engine_boost_tree Nome da engine, `"xgboost"` ou `"lightgbm"`.
#'
#' @details Aplica balanceamento uma vez por fold e armazena datasets de treino e teste para todas as avaliações do objetivo.
#'
#' @return Lista de objetos de fold com datasets em cache, rótulos, pesos e metadados.
#' @noRd
TuneBoostTree_PrepareBalancedFolds <- function(formula, data, nFolds, balanceFn, balanceArgs = list(), scalePosWeightSetting = "auto", nThreads = 1L, seed = 42L, engine_boost_tree = "xgboost", targetLevels = NULL) {

  formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, data) # Objetivo: centralizar a leitura da fórmula para manter a mesma ordem de preditores em todo o fluxo.
  preparedFull <- TuneBoostTree_PrepareMatrix(formula, data, NULL, targetLevels, formulaInfo) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  folds <- TuneBoostTree_CreateStratifiedFolds(preparedFull$yData, nFolds, seed) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  balancedFolds <- vector("list", length(folds)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  for(foldId in seq_along(folds)){
    testIndex <- folds[[foldId]] # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    trainData <- data[setdiff(seq_len(nrow(data)), testIndex), , drop = FALSE] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
    testData <- data[testIndex, , drop = FALSE] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
    balancedTrain <- do.call(balanceFn, c(list(data = trainData, formula = formula), balanceArgs)) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
    preparedTrain <- TuneBoostTree_PrepareMatrix(formula, balancedTrain, NULL, preparedFull$targetLevels, formulaInfo) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
    preparedTest <- TuneBoostTree_PrepareMatrix(formula, testData, NULL, preparedTrain$targetLevels, formulaInfo) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
    trainObject <- TuneBoostTree_CreateDataObject(preparedTrain$xMatrix, preparedTrain$yData, preparedTrain$featureTypes, nThreads, engine_boost_tree) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
    testObject <- TuneBoostTree_CreateDataObject(preparedTest$xMatrix, preparedTest$yData, preparedTest$featureTypes, nThreads, engine_boost_tree) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
    foldScalePosWeight <- TuneBoostTree_ResolveScalePosWeight(preparedTrain$yData, scalePosWeightSetting) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    if(engine_boost_tree == "xgboost"){
      balancedFolds[[foldId]] <- list(dtrain = trainObject, dtest = testObject, yTest = preparedTest$yData, scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
    } else {
      balancedFolds[[foldId]] <- list(dstrain = trainObject, dstest = testObject, xTest = TuneBoostTree_AsPredictionMatrix(preparedTest$xMatrix), yTest = preparedTest$yData, scalePosWeight = foldScalePosWeight, featureNames = preparedTrain$featureNames, targetLevels = preparedTrain$targetLevels) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
    }
  }
  balancedFolds # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
}
####
## Fim
#

#' Executar validação cruzada manual
#'
#' @param balancedFolds List of cached fold objects.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Inteiro com limite de rodadas da tunagem.
#' @param earlyStoppingRounds Inteiro com paciência da parada antecipada.
#' @param seed Inteiro usado como semente aleatória.
#' @param nThreads Inteiro com threads por worker.
#' @param nWorkersFolds Inteiro com número de workers de folds.
#' @param evalMetric Nome da métrica do XGBoost.
#' @param engine_boost_tree Nome da engine, `"xgboost"` ou `"lightgbm"`.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Executa folds em cache sequencialmente ou com `parallel` base, limitando threads da engine para evitar sobrecarga de CPU.
#'
#' @return Lista com score médio, melhor iteração média e scores por fold.
#' @noRd
TuneBoostTree_RunCvManual <- function(balancedFolds, hyperparameters, nRounds, earlyStoppingRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend = "auto") {

  nWorkers <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  workerThreads <- max(1L, as.integer(nThreads)) # Objetivo: reutilizar o orçamento de threads já resolvido por TuneBoostTree_FinalizeParallel.
  foldIds <- seq_along(balancedFolds) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  TuneBoostTree_SetPassiveOpenMp()
  if(nWorkers == 1L){
    foldResults <- vector("list", length(foldIds)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    for(i in foldIds) foldResults[[i]] <- TuneBoostTree_RunOneFold(balancedFolds[[i]], hyperparameters, nRounds, earlyStoppingRounds, seed + i, workerThreads, evalMetric, engine_boost_tree, prAucBackend) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  } else if(.Platform$OS.type == "windows"){
    cluster <- parallel::makeCluster(nWorkers) # Objetivo: limitar o paralelismo para acelerar folds sem exceder o orçamento de CPU disponível.
    on.exit(parallel::stopCluster(cluster), add = TRUE) # Objetivo: limitar o paralelismo para acelerar folds sem exceder o orçamento de CPU disponível.
    foldResults <- parallel::parLapply(cluster, foldIds, TuneBoostTree_RunFoldById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, earlyStoppingRounds = earlyStoppingRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree, prAucBackend = prAucBackend) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  } else {
    foldResults <- parallel::mclapply(foldIds, TuneBoostTree_RunFoldById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, earlyStoppingRounds = earlyStoppingRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree, prAucBackend = prAucBackend, mc.cores = nWorkers, mc.set.seed = FALSE) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  }
  foldScores <- vapply(foldResults, `[[`, numeric(1L), "score") # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  foldBestIter <- vapply(foldResults, `[[`, integer(1L), "bestIteration") # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  if(anyNA(foldScores)) cli::cli_abort("At least one validation fold produced undefined PR-AUC; reduce `folds` or provide more positive-class observations.")
  list(score = as.numeric(mean(foldScores)), bestIteration = as.integer(round(mean(foldBestIter))), foldScores = foldScores) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
}
####
## Fim
#

#' Avaliar um conjunto Bayesiano de parâmetros
#'
#' @param learn_rate Learning-rate candidate.
#' @param tree_depth Depth candidate.
#' @param min_n Minimum node-size candidate.
#' @param sample_size Row-sampling candidate.
#' @param mtry Predictor-sampling fraction candidate.
#' @param loss_reduction Split-gain candidate.
#' @param max_bin Histogram-bin candidate.
#'
#' @details Objetivo de topo usado pelo otimizador; seu ambiente é religado por `TuneBoostTreeBayesian` ao estado local da chamada.
#'
#' @return Lista com `Score` e `Pred` para adaptadores de otimizador.
#' @noRd
TuneBoostTree_EvaluateCv <- function(...) {

  hyperparameters <- list(...)
  hyperparameters <- hyperparameters[parameterNames]
  fixedBoostNames <- setdiff(names(boost)[!vapply(boost, is.null, logical(1L))], c("trees", "stop_iter"))
  for(fixedName in setdiff(fixedBoostNames, names(hyperparameters))) hyperparameters[[fixedName]] <- boost[[fixedName]]
  normalizedData <- TuneBoostTree_NormalizeParams(as.data.frame(hyperparameters[parameterNames], stringsAsFactors = FALSE), parameterNames) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  hyperparameters <- as.list(normalizedData[1L, parameterNames, drop = FALSE])
  for(fixedName in setdiff(fixedBoostNames, names(hyperparameters))) hyperparameters[[fixedName]] <- boost[[fixedName]]
  cacheKey <- paste(paste(parameterNames, format(unlist(normalizedData[1L, parameterNames, drop = FALSE], use.names = FALSE), digits = 17L), sep = "="), collapse = "|") # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  if(exists(cacheKey, envir = cacheEnv, inherits = FALSE)){
    cachedResult <- get(cacheKey, envir = cacheEnv, inherits = FALSE) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
    return(list(Score = as.numeric(cachedResult$score), Pred = 0)) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  }
  cvSummary <- TuneBoostTree_RunCvManual(balancedFolds, hyperparameters, nRoundsTuning, earlyStoppingRounds, seed, workerThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  scoreValue <- as.numeric(cvSummary$score) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  bestIteration <- as.integer(cvSummary$bestIteration) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  logIndex <<- logIndex + 1L # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  evaluationLogList[[logIndex]] <<- data.frame(normalizedData[1L, parameterNames, drop = FALSE], Value = scoreValue, bestIteration = bestIteration, stringsAsFactors = FALSE) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  assign(cacheKey, list(score = scoreValue, bestIteration = bestIteration), envir = cacheEnv) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  list(Score = scoreValue, Pred = 0) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
}
####
## Fim
#

#' Executar um fold por identificador
#'
#' @param foldId Inteiro identificador do fold.
#' @param balancedFolds List of cached folds.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Inteiro com limite de rodadas da tunagem.
#' @param earlyStoppingRounds Inteiro com paciência da parada antecipada.
#' @param seed Inteiro usado como semente aleatória.
#' @param nThreads Inteiro com threads deste worker de fold.
#' @param evalMetric Nome da métrica do XGBoost.
#' @param engine_boost_tree Nome da engine.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Adaptador pequeno de topo que mantém workers paralelos autocontidos e evita closures sobre o estado dos folds.
#'
#' @return Lista com resultado do fold.
#' @noRd
TuneBoostTree_RunFoldById <- function(foldId, balancedFolds, hyperparameters, nRounds, earlyStoppingRounds, seed, nThreads, evalMetric, engine_boost_tree, prAucBackend = "auto") {

  TuneBoostTree_RunOneFold(balancedFolds[[foldId]], hyperparameters, nRounds, earlyStoppingRounds, seed + foldId, nThreads, evalMetric, engine_boost_tree, prAucBackend) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
}
####
## Fim
#

#' Executar um fold em cache
#'
#' @param foldData Cached fold object.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Inteiro com limite de rodadas da tunagem.
#' @param earlyStoppingRounds Inteiro com paciência da parada antecipada.
#' @param seed Inteiro usado como semente aleatória.
#' @param nThreads Inteiro com threads deste worker de fold.
#' @param evalMetric Nome da métrica do XGBoost.
#' @param engine_boost_tree Nome da engine.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details O treino e a predição específicos de cada engine ficam isolados aqui para a CV manual.
#'
#' @return Lista com score do fold e melhor iteração.
#' @noRd
TuneBoostTree_RunOneFold <- function(foldData, hyperparameters, nRounds, earlyStoppingRounds, seed, nThreads, evalMetric, engine_boost_tree, prAucBackend = "auto") {

  paramsValue <- TuneBoostTree_BuildParams(hyperparameters, nThreads, foldData$scalePosWeight, seed, evalMetric, engine_boost_tree) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  if(engine_boost_tree == "xgboost"){
    foldModel <- xgboost::xgb.train(params = paramsValue, data = foldData$dtrain, nrounds = as.integer(nRounds), watchlist = list(train = foldData$dtrain, eval = foldData$dtest), early_stopping_rounds = as.integer(earlyStoppingRounds), maximize = TRUE, verbose = 0L) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    bestIterFold <- as.integer(foldModel$best_iteration) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    if(is.null(bestIterFold) || is.na(bestIterFold) || bestIterFold < 1L) bestIterFold <- as.integer(nRounds) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    predictedProbability <- as.numeric(stats::predict(foldModel, newdata = foldData$dtest)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  } else {
    foldModel <- lightgbm::lgb.train(params = paramsValue, data = foldData$dstrain, nrounds = as.integer(nRounds), valids = list(eval = foldData$dstest), early_stopping_rounds = as.integer(earlyStoppingRounds), verbose = -1L) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    bestIterFold <- as.integer(foldModel$best_iter) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    if(is.null(bestIterFold) || is.na(bestIterFold) || bestIterFold < 1L) bestIterFold <- as.integer(nRounds) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
    predictedProbability <- as.numeric(stats::predict(foldModel, data = foldData$xTest)) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  }
  list(score = TuneBoostTree_CalculatePrAuc(foldData$yTest, predictedProbability, backend = prAucBackend), bestIteration = bestIterFold) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
}
####
## Fim
#


#' Executar otimizador de hiperparâmetros
#' @noRd
TuneBoostTree_RunOptimizer <- function(objective, bounds, initGridDt = NULL, initPoints = 10L, nIter = 30L, acq = "ucb", kappa = 2.576, eps = 0, verbose = TRUE, seed = 42L, optimizerBackend = "internal", limboCommand = NA_character_, limboFallback = TRUE) {

  if(identical(optimizerBackend, "limbo")){
    if(TuneBoostTree_IsExecutableCommand(limboCommand)){
      limboResult <- tryCatch(TuneBoostTree_RunLimboOptimizer(objective, bounds, initGridDt, initPoints, nIter, acq, kappa, eps, seed, limboCommand), error = function(e) e)
      if(!inherits(limboResult, "error")) return(limboResult)
      if(!isTRUE(limboFallback)) cli::cli_abort("Limbo optimizer failed and `fallback = FALSE`: {conditionMessage(limboResult)}")
      cli::cli_warn("Limbo optimizer failed; using the package-native Bayesian optimizer fallback. Cause: {conditionMessage(limboResult)}")
      return(TuneBoostTree_RunInternalOptimizer(objective, bounds, initGridDt, initPoints, nIter, seed, acq, kappa, eps))
    }
    if(!isTRUE(limboFallback)) cli::cli_abort("Limbo optimizer command is not available or executable and `fallback = FALSE`.")
    if(!is.na(limboCommand) && nzchar(limboCommand)) cli::cli_warn("Limbo optimizer command is not available; using the package-native Bayesian optimizer fallback.")
    return(TuneBoostTree_RunInternalOptimizer(objective, bounds, initGridDt, initPoints, nIter, seed, acq, kappa, eps))
  }
  if(identical(optimizerBackend, "rBayesianOptimization") && requireNamespace("rBayesianOptimization", quietly = TRUE)){
    return(TuneBoostTree_RunRBayesianOptimization(objective, bounds, initGridDt, initPoints, nIter, acq, kappa, eps, verbose, seed))
  }
  if(identical(optimizerBackend, "rBayesianOptimization")){
    if(!isTRUE(limboFallback)) cli::cli_abort("Package {.pkg rBayesianOptimization} is not available and `fallback = FALSE`.")
    cli::cli_warn("Package {.pkg rBayesianOptimization} is not available; using the package-native Bayesian optimizer fallback.")
  }
  TuneBoostTree_RunInternalOptimizer(objective, bounds, initGridDt, initPoints, nIter, seed, acq, kappa, eps)
}
####
## Fim
#

#' Executar otimizador Limbo ask/tell
#' @noRd
TuneBoostTree_RunLimboOptimizer <- function(objective, bounds, initGridDt = NULL, initPoints = 10L, nIter = 30L, acq = "ucb", kappa = 2.576, eps = 0, seed = 42L, limboCommand = NA_character_) {

  set.seed(as.integer(seed))
  parameterNames <- names(bounds)
  history <- TuneBoostTree_EvaluateInitialCandidates(objective, bounds, initGridDt, initPoints)
  best <- TuneBoostTree_BestHistoryRow(history, parameterNames)
  for(iteration in seq_len(as.integer(nIter))){
    candidate <- TuneBoostTree_RequestLimboCandidate(limboCommand, bounds, history, acq, kappa, eps, seed, iteration)
    candidate <- TuneBoostTree_ValidateCandidate(candidate, bounds)
    value <- as.numeric(do.call(objective, as.list(candidate[1L, parameterNames, drop = FALSE]))$Score)[1L]
    if(is.finite(value)){
      history <- rbind(history, data.frame(candidate[1L, parameterNames, drop = FALSE], Value = value, stringsAsFactors = FALSE))
      if(value > best$Best_Value) best <- list(Best_Par = as.list(candidate[1L, parameterNames, drop = FALSE]), Best_Value = value)
    }
  }
  if(!is.finite(best$Best_Value)) cli::cli_abort("Limbo did not produce any finite optimizer score.")
  list(Best_Par = best$Best_Par, Best_Value = best$Best_Value, History = history)
}
####
## Fim
#

#' Executar otimizador interno seguro
#' @noRd
TuneBoostTree_RunInternalOptimizer <- function(objective, bounds, initGridDt = NULL, initPoints = 10L, nIter = 30L, seed = 42L, acq = "ucb", kappa = 2.576, eps = 0) {

  set.seed(as.integer(seed))
  parameterNames <- names(bounds)
  history <- TuneBoostTree_EvaluateInitialCandidates(objective, bounds, initGridDt, initPoints)
  best <- TuneBoostTree_BestHistoryRow(history, parameterNames)
  if(!is.finite(best$Best_Value)) cli::cli_abort("All initial optimizer candidate evaluations failed or returned non-finite scores.")
  for(iteration in seq_len(as.integer(nIter))){
    candidate <- TuneBoostTree_ProposeInternalBayesianCandidate(history, bounds, acq, kappa, eps, seed + iteration)
    value <- as.numeric(do.call(objective, as.list(candidate[1L, parameterNames, drop = FALSE]))$Score)[1L]
    if(is.finite(value)){
      history <- rbind(history, data.frame(candidate[1L, parameterNames, drop = FALSE], Value = value, stringsAsFactors = FALSE))
      if(value > best$Best_Value) best <- list(Best_Par = as.list(candidate[1L, parameterNames, drop = FALSE]), Best_Value = value)
    }
  }
  list(Best_Par = best$Best_Par, Best_Value = best$Best_Value, History = history)
}
####
## Fim
#

#' Avaliar candidatos iniciais e de aquecimento
#' @noRd
TuneBoostTree_EvaluateInitialCandidates <- function(objective, bounds, initGridDt = NULL, initPoints = 10L) {

  parameterNames <- names(bounds)
  candidates <- TuneBoostTree_SampleCandidates(bounds, as.integer(initPoints))
  if(!is.null(initGridDt) && nrow(initGridDt) > 0L) candidates <- rbind(TuneBoostTree_ValidateCandidate(initGridDt[, parameterNames, drop = FALSE], bounds), candidates)
  if(nrow(candidates) == 0L) candidates <- TuneBoostTree_SampleCandidates(bounds, max(1L, 2L * length(parameterNames)))
  values <- rep(NA_real_, nrow(candidates))
  for(rowId in seq_len(nrow(candidates))) values[[rowId]] <- as.numeric(do.call(objective, as.list(candidates[rowId, parameterNames, drop = FALSE]))$Score)[1L]
  out <- data.frame(candidates[, parameterNames, drop = FALSE], Value = values, stringsAsFactors = FALSE)
  out[is.finite(out$Value), , drop = FALSE]
}
####
## Fim
#

#' Selecionar melhor linha do histórico
#' @noRd
TuneBoostTree_BestHistoryRow <- function(history, parameterNames) {

  if(is.null(history) || nrow(history) == 0L || !any(is.finite(history$Value))) return(list(Best_Par = NULL, Best_Value = -Inf))
  bestId <- which.max(history$Value)
  list(Best_Par = as.list(history[bestId, parameterNames, drop = FALSE]), Best_Value = as.numeric(history$Value[[bestId]]))
}
####
## Fim
#

#' Propor candidato com aquisição gaussiana leve
#' @noRd
TuneBoostTree_ProposeInternalBayesianCandidate <- function(history, bounds, acq = "ucb", kappa = 2.576, eps = 0, seed = 42L) {

  set.seed(as.integer(seed))
  parameterNames <- names(bounds)
  poolSize <- max(512L, min(8192L, 1024L * length(parameterNames)))
  pool <- TuneBoostTree_SampleCandidates(bounds, poolSize)
  pool <- TuneBoostTree_RemoveKnownCandidates(pool, history, parameterNames)
  if(nrow(pool) == 0L) pool <- TuneBoostTree_SampleCandidates(bounds, poolSize)
  if(nrow(history) < max(4L, length(parameterNames) + 1L)) return(pool[1L, parameterNames, drop = FALSE])
  score <- TuneBoostTree_AcquisitionScores(history, pool, bounds, acq, kappa, eps)
  pool[which.max(score), parameterNames, drop = FALSE]
}
####
## Fim
#

#' Pontuar conjunto de candidatos com posterior gaussiano
#' @noRd
TuneBoostTree_AcquisitionScores <- function(history, pool, bounds, acq = "ucb", kappa = 2.576, eps = 0) {

  parameterNames <- names(bounds)
  xTrain <- TuneBoostTree_ScaleUnit(history[, parameterNames, drop = FALSE], bounds)
  xPool <- TuneBoostTree_ScaleUnit(pool[, parameterNames, drop = FALSE], bounds)
  y <- as.numeric(history$Value)
  yMean <- mean(y)
  ySd <- stats::sd(y)
  if(!is.finite(ySd) || ySd <= 1e-12) ySd <- 1
  yScaled <- (y - yMean) / ySd
  lengthScale <- rep(0.35, length(parameterNames))
  kTrain <- TuneBoostTree_RbfKernel(xTrain, xTrain, lengthScale) + diag(1e-6, nrow(xTrain))
  cholK <- tryCatch(chol(kTrain), error = function(e) NULL)
  if(is.null(cholK)) return(stats::runif(nrow(pool)))
  alpha <- backsolve(cholK, forwardsolve(t(cholK), yScaled))
  kPool <- TuneBoostTree_RbfKernel(xPool, xTrain, lengthScale)
  mu <- as.numeric(kPool %*% alpha) * ySd + yMean
  v <- forwardsolve(t(cholK), t(kPool))
  sigma <- sqrt(pmax(1 - colSums(v * v), 1e-12)) * ySd
  acq <- tolower(as.character(acq)[1L])
  if(identical(acq, "ucb")) return(mu + as.numeric(kappa)[1L] * sigma)
  z <- (mu - max(y) - as.numeric(eps)[1L]) / pmax(sigma, 1e-12)
  if(identical(acq, "poi")) return(stats::pnorm(z))
  improvement <- (mu - max(y) - as.numeric(eps)[1L]) * stats::pnorm(z) + sigma * stats::dnorm(z)
  pmax(improvement, 0)
}
####
## Fim
#

#' Núcleo exponencial quadrático
#' @noRd
TuneBoostTree_RbfKernel <- function(xA, xB, lengthScale) {

  xA <- as.matrix(xA)
  xB <- as.matrix(xB)
  scaledA <- sweep(xA, 2L, lengthScale, "/")
  scaledB <- sweep(xB, 2L, lengthScale, "/")
  dist2 <- outer(rowSums(scaledA^2), rowSums(scaledB^2), "+") - 2 * tcrossprod(scaledA, scaledB)
  exp(-0.5 * pmax(dist2, 0))
}
####
## Fim
#

#' Escalar parâmetros para hipercubo unitário
#' @noRd
TuneBoostTree_ScaleUnit <- function(parameterData, bounds) {

  parameterNames <- names(bounds)
  out <- as.data.frame(parameterData[, parameterNames, drop = FALSE], stringsAsFactors = FALSE)
  for(parameterName in parameterNames){
    lower <- as.numeric(bounds[[parameterName]][1L])
    upper <- as.numeric(bounds[[parameterName]][2L])
    out[[parameterName]] <- (as.numeric(out[[parameterName]]) - lower) / max(upper - lower, .Machine$double.eps)
  }
  out
}
####
## Fim
#

#' Remover candidatos já avaliados
#' @noRd
TuneBoostTree_RemoveKnownCandidates <- function(pool, history, parameterNames) {

  if(is.null(history) || nrow(history) == 0L) return(pool)
  poolKey <- do.call(paste, c(TuneBoostTree_NormalizeParams(pool, parameterNames), sep = "|"))
  historyKey <- do.call(paste, c(TuneBoostTree_NormalizeParams(history, parameterNames), sep = "|"))
  pool[!(poolKey %in% historyKey), parameterNames, drop = FALSE]
}
####
## Fim
#

#' Solicitar candidato a executável Limbo ask/tell externo
#' @noRd
TuneBoostTree_RequestLimboCandidate <- function(limboCommand, bounds, history, acq = "ucb", kappa = 2.576, eps = 0, seed = 42L, iteration = 1L) {

  workDir <- tempfile("tbtb_limbo_")
  dir.create(workDir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(workDir, recursive = TRUE, force = TRUE), add = TRUE)
  boundsFile <- file.path(workDir, "bounds.csv")
  observationsFile <- file.path(workDir, "observations.csv")
  configFile <- file.path(workDir, "config.csv")
  candidateFile <- file.path(workDir, "candidate.csv")
  boundsData <- data.frame(parameter = names(bounds), lower = vapply(bounds, `[[`, numeric(1L), 1L), upper = vapply(bounds, `[[`, numeric(1L), 2L), type = ifelse(names(bounds) %in% c("tree_depth", "min_n", "max_bin", "num_leaves", "min_data_in_leaf"), "integer", "double"), stringsAsFactors = FALSE)
  utils::write.csv(boundsData, boundsFile, row.names = FALSE)
  utils::write.csv(history, observationsFile, row.names = FALSE)
  utils::write.csv(data.frame(acq = as.character(acq)[1L], kappa = as.numeric(kappa)[1L], eps = as.numeric(eps)[1L], seed = as.integer(seed), iteration = as.integer(iteration)), configFile, row.names = FALSE)
  status <- suppressWarnings(system2(limboCommand, args = c(boundsFile, observationsFile, configFile, candidateFile), stdout = TRUE, stderr = TRUE, timeout = as.integer(Sys.getenv("TBTB_LIMBO_TIMEOUT", "600"))))
  exitStatus <- attr(status, "status")
  if(!is.null(exitStatus) && !identical(as.integer(exitStatus), 0L)) cli::cli_abort("Limbo command failed with exit status {exitStatus}: {paste(status, collapse = '\n')}")
  if(!file.exists(candidateFile)) cli::cli_abort("Limbo command did not create `candidate.csv`.")
  candidate <- utils::read.csv(candidateFile, stringsAsFactors = FALSE, check.names = FALSE)
  if(nrow(candidate) != 1L) cli::cli_abort("Limbo `candidate.csv` must contain exactly one candidate row.")
  TuneBoostTree_ValidateCandidate(candidate, bounds)
}
####
## Fim
#

#' Validar e limitar candidato do otimizador
#' @noRd
TuneBoostTree_ValidateCandidate <- function(candidate, bounds) {

  parameterNames <- names(bounds)
  candidate <- as.data.frame(candidate, stringsAsFactors = FALSE)
  missingNames <- setdiff(parameterNames, names(candidate))
  if(length(missingNames) > 0L) cli::cli_abort("Optimizer candidate is missing required column(s): {paste(missingNames, collapse = ', ')}.")
  candidate <- candidate[, parameterNames, drop = FALSE]
  for(parameterName in parameterNames){
    value <- as.numeric(candidate[[parameterName]])
    if(anyNA(value) || any(!is.finite(value))) cli::cli_abort("Optimizer candidate column `{parameterName}` contains non-finite value(s).")
    lower <- as.numeric(bounds[[parameterName]][1L])
    upper <- as.numeric(bounds[[parameterName]][2L])
    candidate[[parameterName]] <- pmin(pmax(value, lower), upper)
  }
  TuneBoostTree_NormalizeParams(candidate, parameterNames)
}
####
## Fim
#

#' Executar backend rBayesianOptimization
#' @noRd
TuneBoostTree_RunRBayesianOptimization <- function(objective, bounds, initGridDt = NULL, initPoints = 10L, nIter = 30L, acq = "ucb", kappa = 2.576, eps = 0, verbose = TRUE, seed = 42L) {

  set.seed(as.integer(seed))
  normalizedBounds <- lapply(bounds, function(x) c(as.numeric(x[1L]), as.numeric(x[2L])))
  result <- rBayesianOptimization::BayesianOptimization(FUN = objective, bounds = normalizedBounds, init_grid_dt = initGridDt, init_points = as.integer(initPoints), n_iter = as.integer(nIter), acq = acq, kappa = kappa, eps = eps, verbose = isTRUE(verbose))
  list(Best_Par = as.list(result$Best_Par), Best_Value = as.numeric(result$Best_Value), History = result$History)
}
####
## Fim
#

#' Amostrar candidatos do otimizador interno
#' @noRd
TuneBoostTree_SampleCandidates <- function(bounds, n) {

  parameterNames <- names(bounds)
  n <- as.integer(n)
  out <- as.data.frame(setNames(rep(list(numeric(n)), length(parameterNames)), parameterNames))
  if(n <= 0L) return(out)
  for(parameterName in parameterNames){
    lower <- as.numeric(bounds[[parameterName]][1L])
    upper <- as.numeric(bounds[[parameterName]][2L])
    out[[parameterName]] <- stats::runif(n, lower, upper)
  }
  TuneBoostTree_NormalizeParams(out, parameterNames)
}
####
## Fim
#

#' Otimizar limiar de decisão a partir de predições de validação cruzada
#' @noRd
TuneBoostTree_OptimizeThresholdCv <- function(balancedFolds, hyperparameters, nRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree, prAucBackend = "auto") {

  predictionSummary <- TuneBoostTree_RunCvPredictions(balancedFolds, hyperparameters, nRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree)
  TuneBoostTree_OptimizeThreshold(predictionSummary$actual, predictionSummary$predicted)
}
####
## Fim
#

#' Executar predições de CV sem parada antecipada
#' @noRd
TuneBoostTree_RunCvPredictions <- function(balancedFolds, hyperparameters, nRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree) {

  nWorkers <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds))
  workerThreads <- max(1L, as.integer(nThreads))
  foldIds <- seq_along(balancedFolds)
  TuneBoostTree_SetPassiveOpenMp()
  if(nWorkers == 1L){
    foldResults <- lapply(foldIds, function(i) TuneBoostTree_RunOneFoldPrediction(balancedFolds[[i]], hyperparameters, nRounds, seed + i, workerThreads, evalMetric, engine_boost_tree))
  } else if(.Platform$OS.type == "windows"){
    cluster <- parallel::makeCluster(nWorkers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    foldResults <- parallel::parLapply(cluster, foldIds, TuneBoostTree_RunFoldPredictionById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree)
  } else {
    foldResults <- parallel::mclapply(foldIds, TuneBoostTree_RunFoldPredictionById, balancedFolds = balancedFolds, hyperparameters = hyperparameters, nRounds = nRounds, seed = seed, nThreads = workerThreads, evalMetric = evalMetric, engine_boost_tree = engine_boost_tree, mc.cores = nWorkers, mc.set.seed = FALSE)
  }
  list(actual = unlist(lapply(foldResults, `[[`, "actual"), use.names = FALSE), predicted = unlist(lapply(foldResults, `[[`, "predicted"), use.names = FALSE))
}
####
## Fim
#

#' Executar um fold de predição por identificador
#' @noRd
TuneBoostTree_RunFoldPredictionById <- function(foldId, balancedFolds, hyperparameters, nRounds, seed, nThreads, evalMetric, engine_boost_tree) {

  TuneBoostTree_RunOneFoldPrediction(balancedFolds[[foldId]], hyperparameters, nRounds, seed + foldId, nThreads, evalMetric, engine_boost_tree)
}
####
## Fim
#

#' Executar um fold de predição
#' @noRd
TuneBoostTree_RunOneFoldPrediction <- function(foldData, hyperparameters, nRounds, seed, nThreads, evalMetric, engine_boost_tree) {

  paramsValue <- TuneBoostTree_BuildParams(hyperparameters, nThreads, foldData$scalePosWeight, seed, evalMetric, engine_boost_tree)
  if(engine_boost_tree == "xgboost"){
    foldModel <- xgboost::xgb.train(params = paramsValue, data = foldData$dtrain, nrounds = as.integer(nRounds), verbose = 0L)
    predictedProbability <- as.numeric(stats::predict(foldModel, newdata = foldData$dtest))
  } else {
    foldModel <- lightgbm::lgb.train(params = paramsValue, data = foldData$dstrain, nrounds = as.integer(nRounds), verbose = -1L)
    predictedProbability <- as.numeric(stats::predict(foldModel, data = foldData$xTest))
  }
  list(actual = foldData$yTest, predicted = predictedProbability)
}
####
## Fim
#

#' Otimizar limiar de classificação binária
#' @noRd
TuneBoostTree_OptimizeThreshold <- function(actual, predicted) {

  actual <- as.integer(actual)
  predicted <- as.numeric(predicted)
  valid <- is.finite(predicted) & !is.na(actual)
  actual <- actual[valid]
  predicted <- predicted[valid]
  if(length(actual) == 0L || length(unique(actual)) < 2L) return(list(threshold = 0.5, metric = "f1", score = NA_real_))
  thresholds <- sort(unique(predicted))
  candidates <- unique(pmin(pmax(c(0.5, thresholds), .Machine$double.eps), 1 - .Machine$double.eps))
  scores <- vapply(candidates, function(threshold) TuneBoostTree_F1Score(actual, predicted >= threshold), numeric(1L))
  bestIndex <- which.max(scores)
  list(threshold = as.numeric(candidates[bestIndex]), metric = "f1", score = as.numeric(scores[bestIndex]))
}
####
## Fim
#

#' Calculate F1 Score
#' @noRd
TuneBoostTree_F1Score <- function(actual, predictedClass) {

  tp <- sum(actual == 1L & predictedClass)
  fp <- sum(actual == 0L & predictedClass)
  fn <- sum(actual == 1L & !predictedClass)
  precision <- if((tp + fp) == 0L) 0 else tp / (tp + fp)
  recall <- if((tp + fn) == 0L) 0 else tp / (tp + fn)
  if((precision + recall) == 0) return(0)
  2 * precision * recall / (precision + recall)
}
####
## Fim
#

#' Calculate PR AUC
#'
#' @param actual Rótulos inteiros 0/1.
#' @param predicted Numeric positive-class probabilities.
#'
#' @details AUC precisão-revocação trapezoidal interna compatível com a implementação atual.
#'
#' @return Numeric PR-AUC value.
#' @noRd
TuneBoostTree_CalculatePrAuc <- function(actual, predicted, backend = "auto") {

  backend <- TuneBoostTree_SelectPrAucBackend(backend) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  actual <- as.integer(actual) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  predicted <- as.numeric(predicted) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(length(actual) != length(predicted) || length(actual) == 0L) return(NA_real_) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  if(anyNA(actual) || anyNA(predicted) || any(!is.finite(predicted))) return(NA_real_) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  positiveCount <- sum(actual == 1L) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(positiveCount == 0L) return(NA_real_) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  if(identical(backend, "c")) return(TuneBoostTree_CalculatePrAucC(actual, predicted)) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  if(identical(backend, "fortran")) return(TuneBoostTree_CalculatePrAucFortran(actual, predicted)) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  if(identical(backend, "rfast")) return(TuneBoostTree_CalculatePrAucRfast(actual, predicted, positiveCount)) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  TuneBoostTree_CalculatePrAucR(actual, predicted, positiveCount) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
}
####
## Fim
#


#' Selecionar backend de PR-AUC
#'
#' @param backend Nome do backend solicitado.
#'
#' @details `auto` prefere C compilado, depois Fortran compilado, depois Rfast e por fim R base. Solicitações explícitas indisponíveis retornam para R base para não abortar uma tunagem longa.
#'
#' @return Nome do backend resolvido.
#' @noRd
TuneBoostTree_SelectPrAucBackend <- function(backend = "auto") {

  backend <- match.arg(as.character(backend)[1L], c("auto", "c", "fortran", "rfast", "r")) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  if(identical(backend, "auto")){
    if(TuneBoostTree_LoadNativeBackend("c")) return("c") # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
    if(TuneBoostTree_LoadNativeBackend("fortran")) return("fortran") # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
    if(requireNamespace("Rfast", quietly = TRUE)) return("rfast") # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
    return("r") # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  }
  if(identical(backend, "c") && !TuneBoostTree_LoadNativeBackend("c")) return("r") # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  if(identical(backend, "fortran") && !TuneBoostTree_LoadNativeBackend("fortran")) return("r") # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  if(identical(backend, "rfast") && !requireNamespace("Rfast", quietly = TRUE)) return("r") # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  backend # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
}
####
## Fim
#

#' Carregar backend nativo opcional
#'
#' @param backend `"c"` ou `"fortran"`.
#'
#' @details Verifica se a DLL instalada do pacote, contendo as rotinas C e Fortran registradas, está carregada.
#'
#' @return Lógico indicando se o símbolo está disponível.
#' @noRd
TuneBoostTree_LoadNativeBackend <- function(backend) {

  packageDll <- "TuneBoostTreeBayesian" # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  packageDll %in% names(getLoadedDLLs()) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
}
####
## Fim
#

#' Calcular PR-AUC com C compilado
#'
#' @noRd
TuneBoostTree_CalculatePrAucC <- function(actual, predicted) {

  as.numeric(.Call("tbtb_pr_auc_c", actual, predicted, PACKAGE = "TuneBoostTreeBayesian")) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
}
####
## Fim
#

#' Calcular PR-AUC com Fortran compilado
#' @noRd
TuneBoostTree_CalculatePrAucFortran <- function(actual, predicted) {

  out <- .Fortran("tbtb_pr_auc_f", n = as.integer(length(actual)), actual = as.integer(actual), predicted = as.double(predicted), score = as.double(NA_real_), PACKAGE = "TuneBoostTreeBayesian") # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  as.numeric(out$score) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
}
####
## Fim
#

#' Calcular PR-AUC com ordenação Rfast
#' @noRd
TuneBoostTree_CalculatePrAucRfast <- function(actual, predicted, positiveCount = sum(actual == 1L)) {

  orderIndex <- Rfast::Order(as.numeric(predicted), stable = TRUE, descending = TRUE) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  TuneBoostTree_CalculatePrAucOrdered(actual[orderIndex], positiveCount) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
}
####
## Fim
#

#' Calcular PR-AUC com ordenação em R base
#' @noRd
TuneBoostTree_CalculatePrAucR <- function(actual, predicted, positiveCount = sum(actual == 1L)) {

  orderIndex <- order(predicted, decreasing = TRUE) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  TuneBoostTree_CalculatePrAucOrdered(actual[orderIndex], positiveCount) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
}
####
## Fim
#

#' Acumular PR-AUC ordenada
#' @noRd
TuneBoostTree_CalculatePrAucOrdered <- function(actualOrd, positiveCount) {

  tp <- cumsum(actualOrd == 1L) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  fp <- cumsum(actualOrd == 0L) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  precision <- c(1, tp / pmax(tp + fp, 1)) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  recall <- c(0, tp / positiveCount) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  sum((recall[-1L] - recall[-length(recall)]) * precision[-1L]) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
}
####
## Fim
#

#' Normalizar parâmetros
#'
#' @param parameterData data.frame of candidate parameters.
#' @param parameterNames Nomes ordenados dos parâmetros a normalizar.
#'
#' @details Arredonda parâmetros inteiros e estabiliza valores contínuos para chaves de cache e comparação.
#'
#' @return data.frame normalizado.
#' @noRd
TuneBoostTree_NormalizeParams <- function(parameterData, parameterNames) {

  parameterData <- as.data.frame(parameterData, stringsAsFactors = FALSE) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  parameterData <- parameterData[, parameterNames, drop = FALSE] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  for(parameterName in parameterNames) parameterData[[parameterName]] <- as.numeric(parameterData[[parameterName]]) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  integerParameters <- intersect(c("tree_depth", "min_n", "max_bin", "num_leaves", "min_data_in_leaf"), parameterNames)
  for(parameterName in integerParameters) parameterData[[parameterName]] <- as.integer(round(parameterData[[parameterName]])) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  for(parameterName in setdiff(parameterNames, integerParameters)) parameterData[[parameterName]] <- round(parameterData[[parameterName]], digits = 12L) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  parameterData # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
}
####
## Fim
#

#' Compare Scores
#'
#' @param scoreA First numeric score.
#' @param scoreB Second numeric score.
#' @param tolerance Relative tolerance for equality.
#'
#' @details Auxiliar interno para relacionar melhores valores do otimizador ao log de avaliação.
#'
#' @return Logical scalar.
#' @noRd
TuneBoostTree_IsScoreMatch <- function(scoreA, scoreB, tolerance = 1e-6) {

  scoreA <- as.numeric(scoreA) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  scoreB <- as.numeric(scoreB) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  is.finite(scoreA) && is.finite(scoreB) && abs(scoreA - scoreB) <= tolerance * max(1, abs(scoreA), abs(scoreB)) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
}
####
## Fim
#


#' Complete Parameter Grid Columns
#' @noRd
TuneBoostTree_CompleteParameterGrid <- function(gridData, bounds) {

  gridData <- as.data.frame(gridData, stringsAsFactors = FALSE)
  for(parameterName in names(bounds)){
    if(!parameterName %in% names(gridData)) gridData[[parameterName]] <- mean(as.numeric(bounds[[parameterName]]))
  }
  gridData
}
####
## Fim
#

#' Create Initialization Grid
#'
#' @param historyData data.frame ou data.table com log de avaliações.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Converte avaliações registradas para o esquema de aquecimento esperado pela otimização Bayesiana.
#'
#' @return Um data.frame ou `NULL`.
#' @noRd
TuneBoostTree_CreateInitGrid <- function(historyData, bounds) {

  if(is.null(historyData) || nrow(historyData) == 0L) return(NULL) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  requiredNames <- c(names(bounds), "Value") # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  historyData <- TuneBoostTree_CompleteParameterGrid(historyData, bounds) # Objetivo: validar a comunicação com o otimizador para impedir candidatos fora dos limites definidos.
  out <- as.data.frame(historyData[, requiredNames, drop = FALSE], stringsAsFactors = FALSE) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  out <- out[stats::complete.cases(out), , drop = FALSE] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(nrow(out) == 0L) return(NULL) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  out # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
}
####
## Fim
#

#' Combine Initialization Grids
#'
#' @param initGridDt Existing warm-start grid.
#' @param newInitGridDt Newly created warm-start grid.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Combina avaliações anteriores e atuais, mantendo apenas o melhor score duplicado por chave de parâmetros.
#'
#' @return data.frame deduplicado ou `NULL`.
#' @noRd
TuneBoostTree_CombineInitGrid <- function(initGridDt, newInitGridDt, bounds) {

  if(is.null(initGridDt)) return(newInitGridDt) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  if(is.null(newInitGridDt)) return(initGridDt) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  TuneBoostTree_DeduplicateInitGrid(rbind(initGridDt, newInitGridDt), bounds) # Objetivo: validar a comunicação com o otimizador para impedir candidatos fora dos limites definidos.
}
####
## Fim
#

#' Deduplicate Initialization Grid
#'
#' @param gridData Warm-start grid data.frame.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Normaliza hiperparâmetros e mantém a linha com maior `Value` para cada chave de parâmetros.
#'
#' @return data.frame deduplicado ou `NULL`.
#' @noRd
TuneBoostTree_DeduplicateInitGrid <- function(gridData, bounds) {

  if(is.null(gridData)) return(NULL) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  if(nrow(gridData) == 0L) return(gridData) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  parameterNames <- names(bounds) # Objetivo: validar a comunicação com o otimizador para impedir candidatos fora dos limites definidos.
  requiredNames <- c(parameterNames, "Value") # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  gridData <- TuneBoostTree_CompleteParameterGrid(gridData, bounds) # Objetivo: validar a comunicação com o otimizador para impedir candidatos fora dos limites definidos.
  gridData <- as.data.frame(gridData[, requiredNames, drop = FALSE], stringsAsFactors = FALSE) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  gridData <- gridData[stats::complete.cases(gridData), , drop = FALSE] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(nrow(gridData) == 0L) return(gridData) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  normalizedData <- TuneBoostTree_NormalizeParams(gridData, parameterNames) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  for(parameterName in parameterNames) gridData[[parameterName]] <- normalizedData[[parameterName]] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  key <- do.call(paste, c(gridData[, parameterNames, drop = FALSE], sep = "|")) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  gridData$key__ <- key # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  gridData$order__ <- seq_len(nrow(gridData)) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  gridData <- gridData[order(gridData$key__, -gridData$Value, gridData$order__), , drop = FALSE] # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  gridData <- gridData[!duplicated(gridData$key__), , drop = FALSE] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  gridData[, setdiff(names(gridData), c("key__", "order__")), drop = FALSE] # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
}
####
## Fim
#

#' Find Best Iteration
#'
#' @param evaluationLog data.table com log de avaliações.
#' @param bestHyperparameters Named best-parameter list.
#' @param bestScore Numeric best score.
#' @param bounds Named bounds list defining parameter columns.
#'
#' @details Encontra a iteração com parada antecipada correspondente ao melhor par parâmetro/score do otimizador.
#'
#' @return Melhor iteração inteira ou `NULL`.
#' @noRd
TuneBoostTree_FindBestIteration <- function(evaluationLog, bestHyperparameters, bestScore, bounds) {

  if(is.null(evaluationLog) || nrow(evaluationLog) == 0L) return(NULL) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  parameterNames <- names(bounds) # Objetivo: validar a comunicação com o otimizador para impedir candidatos fora dos limites definidos.
  normalizedBest <- TuneBoostTree_NormalizeParams(as.data.frame(bestHyperparameters, stringsAsFactors = FALSE), parameterNames) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  normalizedLog <- TuneBoostTree_NormalizeParams(evaluationLog, parameterNames) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  matched <- rep(TRUE, nrow(normalizedLog)) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  for(parameterName in parameterNames) matched <- matched & (normalizedLog[[parameterName]] == normalizedBest[[parameterName]][1L]) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  matched <- matched & vapply(evaluationLog$Value, TuneBoostTree_IsScoreMatch, logical(1L), scoreB = bestScore) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
  if(!any(matched)) return(NULL) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
  as.integer(evaluationLog$bestIteration[[which(matched)[1L]]]) # Objetivo: registrar avaliações e reaproveitar resultados para tornar a otimização auditável e evitar trabalho duplicado.
}
####
## Fim
#

#' Dividir dados em folds estratificados para boost-tree
#'
#' @param yData Vetor binário inteiro ou lógico da variável resposta.
#' @param nFolds Inteiro com número de folds.
#' @param seed Inteiro usado como semente aleatória.
#'
#' @details Wrapper público para validação cruzada externa ou fluxos que precisam
#'   da mesma estratificação usada pelo tuner.
#'
#' @return Lista de vetores inteiros. Cada elemento contém os índices de teste de
#'   um fold; os índices de treino são obtidos pelo complemento.
#' @export
SplitDataBoostTreeFolds <- function(yData, nFolds = 10L, seed = 42L) {

  if(length(yData) == 0L || anyNA(yData)) cli::cli_abort("`yData` must be a non-empty binary vector without NA.") # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  yData <- as.integer(yData) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(!all(yData %in% c(0L, 1L))) cli::cli_abort("`yData` must contain only 0 and 1 values.") # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(any(table(yData) < as.integer(nFolds))) cli::cli_abort("Each class must contain at least `nFolds` observations.") # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
  TuneBoostTree_CreateStratifiedFolds(yData, nFolds, seed) # Objetivo: manter a validação cruzada reprodutível e estratificada, sem vazamento entre treino e validação.
}
####
## Fim
#

#' Ajustar modelo boosted tree
#'
#' @param formula Fórmula de duas faces com uma variável resposta binária e
#'   preditores numéricos.
#' @param dataTrain data.frame de treino.
#' @param hyperparameters Lista nomeada retornada por `TuneBoostTree` ou lista
#'   equivalente com nomes canônicos de hiperparâmetros.
#' @param featureTypes Vetor opcional de tipos de features do XGBoost.
#' @param targetLevels Ordenação opcional de dois níveis da variável resposta.
#' @param scalePosWeight Peso opcional da classe positiva; é calculado quando
#'   `NULL`.
#' @param nThreads Inteiro com número de threads da engine.
#' @param seed Inteiro usado como semente aleatória.
#' @param verbose Verbosidade da engine.
#' @param engineBoostTree Nome da engine, `"xgboost"` ou `"lightgbm"`.
#' @param ... Argumentos legados. `engine_boost_tree` ainda é aceito com aviso
#'   de depreciação e será migrado para `engineBoostTree`.
#'
#' @details Ajusta o modelo final com hiperparâmetros canônicos e isola a
#'   tradução de parâmetros no limite da engine.
#'
#' @return Lista nomeada com o modelo nativo em `model`, parâmetros efetivamente
#'   usados em `params`, nomes/tipos de features, níveis e nomes das classes,
#'   metadados da fórmula, número de rodadas (`nRounds`), `threshold` e `engine`.
#' @export
FitBoostTreeModel <- function(formula, dataTrain, hyperparameters, featureTypes = NULL, targetLevels = NULL, scalePosWeight = NULL, nThreads = 8L, seed = 42L, verbose = 0L, engineBoostTree = "lightgbm", ...) {

  dots <- list(...)
  if("engine_boost_tree" %in% names(dots)){
    if(!missing(engineBoostTree)) cli::cli_abort("Use only one of `engineBoostTree` or deprecated `engine_boost_tree`.")
    cli::cli_warn("`engine_boost_tree` is deprecated; use `engineBoostTree` instead.")
    engineBoostTree <- dots$engine_boost_tree
  }
  unknownDots <- setdiff(names(dots), "engine_boost_tree")
  if(length(unknownDots) > 0L) cli::cli_abort("Unknown argument(s): {paste(unknownDots, collapse = ', ')}")
  engine_boost_tree <- engineBoostTree
  if(!(engine_boost_tree %in% c("xgboost", "lightgbm"))) cli::cli_abort("`engineBoostTree` must be 'xgboost' or 'lightgbm'.") # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  preparedTrain <- TuneBoostTree_PrepareMatrix(formula, dataTrain, featureTypes, targetLevels, NULL) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  classCounts <- table(preparedTrain$yData) # Objetivo: ajustar o peso da classe positiva de acordo com a distribuição realmente usada no treino.
  if(length(classCounts) != 2L || any(classCounts == 0L)) cli::cli_abort("`dataTrain` must contain both binary classes.") # Objetivo: ajustar o peso da classe positiva de acordo com a distribuição realmente usada no treino.
  scalePosWeight <- if(is.null(scalePosWeight)) as.numeric(classCounts[["0"]] / classCounts[["1"]]) else as.numeric(scalePosWeight) # Objetivo: ajustar o peso da classe positiva de acordo com a distribuição realmente usada no treino.
  evalMetric <- if(is.null(hyperparameters$eval_metric)) "aucpr" else as.character(hyperparameters$eval_metric) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  scalePosWeight <- if(!is.null(hyperparameters$scale_pos_weight)) hyperparameters$scale_pos_weight else scalePosWeight
  paramsValue <- TuneBoostTree_BuildParams(hyperparameters, nThreads, scalePosWeight, seed, evalMetric, engine_boost_tree) # Objetivo: ajustar o peso da classe positiva de acordo com a distribuição realmente usada no treino.
  nRounds <- if(is.null(hyperparameters$trees)) 100L else as.integer(hyperparameters$trees) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
  trainObject <- TuneBoostTree_CreateDataObject(preparedTrain$xMatrix, preparedTrain$yData, preparedTrain$featureTypes, nThreads, engine_boost_tree) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
  if(engine_boost_tree == "xgboost"){
    model <- xgboost::xgb.train(params = paramsValue, data = trainObject, nrounds = nRounds, verbose = as.integer(verbose)) # Objetivo: isolar a conversão para objetos nativos das engines e reutilizar o mesmo contrato no treino, CV e predição.
  } else {
    model <- lightgbm::lgb.train(params = paramsValue, data = trainObject, nrounds = nRounds, verbose = as.integer(verbose)) # Objetivo: isolar a conversão para objetos nativos das engines e reutilizar o mesmo contrato no treino, CV e predição.
  }
  threshold <- if(is.null(hyperparameters$threshold)) 0.5 else as.numeric(hyperparameters$threshold)[1L]
  list(model = model, params = paramsValue, featureNames = preparedTrain$featureNames, featureTypes = preparedTrain$featureTypes, targetLevels = preparedTrain$targetLevels, targetName = preparedTrain$targetName, negativeClass = preparedTrain$negativeClass, positiveClass = preparedTrain$positiveClass, formulaInfo = preparedTrain$formulaInfo, nRounds = nRounds, threshold = threshold, engine = engine_boost_tree) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
}
####
## Fim
#

#' Predizer com modelo boosted tree
#'
#' @param modelObj Objeto de modelo retornado por [FitBoostTreeModel()].
#' @param newdata Novo data.frame contendo todas as colunas preditoras.
#' @param threshold Limiar de probabilidade da classe positiva. Quando `NULL`,
#'   usa `modelObj$threshold` se existir; caso contrário, usa `0.5`.
#' @param engineBoostTree Sobrescrita opcional da engine; por padrão usa
#'   `modelObj$engine`.
#' @param ... Argumentos legados. `engine_boost_tree` ainda é aceito com aviso
#'   de depreciação e será migrado para `engineBoostTree`.
#'
#' @details Despacha a predição conforme a engine armazenada e retorna classes
#'   preditas e probabilidades das duas classes.
#'
#' @return Tibble com `predictedClass`, `probabilityFirstClass` e
#'   `probabilitySecondClass`. A segunda probabilidade corresponde à classe
#'   positiva armazenada no modelo.
#' @export
PredictBoostTreeModel <- function(modelObj, newdata, threshold = NULL, engineBoostTree = NULL, ...) {

  dots <- list(...)
  if("engine_boost_tree" %in% names(dots)){
    if(!is.null(engineBoostTree)) cli::cli_abort("Use only one of `engineBoostTree` or deprecated `engine_boost_tree`.")
    cli::cli_warn("`engine_boost_tree` is deprecated; use `engineBoostTree` instead.")
    engineBoostTree <- dots$engine_boost_tree
  }
  unknownDots <- setdiff(names(dots), "engine_boost_tree")
  if(length(unknownDots) > 0L) cli::cli_abort("Unknown argument(s): {paste(unknownDots, collapse = ', ')}")
  if(!is.data.frame(newdata) || nrow(newdata) == 0L) cli::cli_abort("`newdata` must be a non-empty data.frame.") # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(is.null(threshold)) threshold <- if(!is.null(modelObj$threshold)) modelObj$threshold else 0.5 # Objetivo: aplicar a mesma regra de decisão binária usada na seleção do modelo e nas métricas finais.
  threshold <- as.numeric(threshold) # Objetivo: aplicar a mesma regra de decisão binária usada na seleção do modelo e nas métricas finais.
  if(length(threshold) != 1L || is.na(threshold) || threshold <= 0 || threshold >= 1) cli::cli_abort("`threshold` must be between 0 and 1.") # Objetivo: aplicar a mesma regra de decisão binária usada na seleção do modelo e nas métricas finais.
  engine <- if(is.null(engineBoostTree)) modelObj$engine else engineBoostTree # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  if(!(engine %in% c("xgboost", "lightgbm"))) cli::cli_abort("Model engine must be 'xgboost' or 'lightgbm'.") # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  featureNames <- modelObj$featureNames # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  missingFeatureNames <- setdiff(featureNames, names(newdata)) # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  if(length(missingFeatureNames) > 0L) cli::cli_abort("`newdata` is missing required predictors: {paste(missingFeatureNames, collapse = ', ')}") # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  newdataFrame <- as.data.frame(newdata) # Objetivo: padronizar entradas tabulares para evitar diferenças entre data.frame, tibble e data.table nas etapas seguintes.
  numericMatrix <- data.matrix(newdataFrame[, featureNames, drop = FALSE]) # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  storage.mode(numericMatrix) <- "double" # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
  colnames(numericMatrix) <- featureNames # Objetivo: garantir alinhamento explícito das features e falhar cedo quando a base de predição estiver incompleta.
  if(engine == "xgboost"){
    nThreads <- if(is.null(modelObj$params$nthread)) 1L else as.integer(modelObj$params$nthread) # Objetivo: traduzir hiperparâmetros canônicos uma única vez para reduzir divergência entre XGBoost e LightGBM.
    predictionObject <- TuneBoostTree_CreateDataObject(numericMatrix, NULL, modelObj$featureTypes, nThreads, "xgboost") # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
    probabilitySecondClass <- as.numeric(stats::predict(modelObj$model, newdata = predictionObject)) # Objetivo: isolar a conversão para objetos nativos das engines e reutilizar o mesmo contrato no treino, CV e predição.
  } else {
    probabilitySecondClass <- as.numeric(stats::predict(modelObj$model, data = numericMatrix)) # Objetivo: entregar às engines uma matriz numérica estável, usando representação esparsa apenas quando isso reduz custo de memória.
  }
  probabilityFirstClass <- 1 - probabilitySecondClass # Objetivo: aplicar a mesma regra de decisão binária usada na seleção do modelo e nas métricas finais.
  predictedClass <- ifelse(probabilitySecondClass >= threshold, modelObj$targetLevels[2L], modelObj$targetLevels[1L]) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  out <- tibble::tibble(predictedClass = predictedClass, probabilityFirstClass = probabilityFirstClass, probabilitySecondClass = probabilitySecondClass) # Objetivo: aplicar a mesma regra de decisão binária usada na seleção do modelo e nas métricas finais.
  attr(out, "targetName") <- modelObj$targetName # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  attr(out, "targetLevels") <- modelObj$targetLevels # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  out # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
}
####
## Fim
#

#' Avaliar performance de modelo boosted tree
#'
#' @param modelObj Objeto de modelo retornado por [FitBoostTreeModel()].
#' @param testData data.frame de teste contendo preditores e variável resposta.
#' @param formula Fórmula de duas faces que identifica a variável resposta.
#'
#' @details Chama [PredictBoostTreeModel()] internamente e calcula PR-AUC e uma
#'   tabela de confusão resumida.
#'
#' @return Lista com `prAuc`, `confusionSummary` e `predictions`.
#'   `confusionSummary` é uma tibble com colunas `actual`, `predicted` e
#'   `count`; `predictions` é o tibble retornado por [PredictBoostTreeModel()].
#' @export
PerformanceBoostTreeModel <- function(modelObj, testData, formula) {

  predictions <- PredictBoostTreeModel(modelObj, testData) # Objetivo: explicitar a intenção desta etapa para facilitar manutenção e auditoria do fluxo de modelagem.
  formulaInfo <- TuneBoostTree_ExtractFormulaInfo(formula, testData) # Objetivo: centralizar a leitura da fórmula para manter a mesma ordem de preditores em todo o fluxo.
  preparedTarget <- TuneBoostTree_PrepareTarget(testData[[formulaInfo$targetName]], modelObj$targetLevels) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  prAuc <- TuneBoostTree_CalculatePrAuc(preparedTarget$yData, predictions$probabilitySecondClass) # Objetivo: manter o cálculo de PR-AUC consistente entre backends rápidos e o fallback portátil em R.
  confusionTable <- table(actual = testData[[formulaInfo$targetName]], predicted = predictions$predictedClass) # Objetivo: preservar a semântica das classes para que treino, validação e predição usem a mesma referência binária.
  confusionSummary <- tibble::as_tibble(as.data.frame(confusionTable, stringsAsFactors = FALSE)) # Objetivo: garantir que toda tabela retornada publicamente seja tibble.
  names(confusionSummary) <- c("actual", "predicted", "count") # Objetivo: expor nomes intuitivos e estáveis para a tabela de confusão.
  list(prAuc = prAuc, confusionSummary = confusionSummary, predictions = predictions) # Objetivo: retornar um objeto autocontido para facilitar auditoria, predição e uso em funções auxiliares.
}
####
## Fim
#
