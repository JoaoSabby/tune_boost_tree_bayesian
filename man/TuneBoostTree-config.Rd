\name{TuneBoostTreeBoostParams}
\alias{TuneBoostTreeBoostParams}
\alias{TuneBoostTreeSearchSpace}
\alias{TuneBoostTreeCv}
\alias{TuneBoostTreeOptimizerLimbo}
\alias{TuneBoostTreeLimbo}
\alias{TuneBoostTreeInternalOptimizer}
\alias{TuneBoostTreeOptimizerRBayesianOptimization}
\alias{TuneBoostTreeRBayesianOptimization}
\alias{TuneBoostTreeImbalance}
\alias{TuneBoostTreePerformance}
\alias{TuneBoostTreeControl}
\alias{TuneBoostTreeParallel}
\alias{TuneBoostTreeXgboost}
\alias{TuneBoostTreeLightgbm}
\alias{TuneBoostTreeUltraConfig}
\alias{TuneBoostTreeBayesianUltra}
\title{Funções auxiliares de configuração do TuneBoostTree}
\usage{
TuneBoostTreeBoostParams(trees = 500L, stop_iter = 20L, learn_rate = NULL,
  tree_depth = NULL, min_n = NULL, loss_reduction = NULL,
  sample_size = NULL, mtry = "default", max_bin = NULL)
TuneBoostTreeSearchSpace(learn_rate = c(0.01, 0.2), tree_depth = c(2L, 12L),
  min_n = c(1, 80), loss_reduction = c(0, 8), sample_size = c(0.55, 1),
  mtry = NULL, max_bin = NULL, lambda = NULL, alpha = NULL,
  max_delta_step = NULL, colsample_bytree = NULL, colsample_bylevel = NULL,
  num_leaves = NULL, min_data_in_leaf = NULL, scale_pos_weight = NULL)
TuneBoostTreeCv(folds = 10L, stratified = TRUE)
TuneBoostTreeOptimizerLimbo(command = NULL, fallback = TRUE, acquisition = c("ucb", "ei", "poi"), kappa = 2.576,
  eps = 0)
TuneBoostTreeLimbo(command = NULL, fallback = TRUE, acquisition = c("ucb", "ei", "poi"), kappa = 2.576,
  eps = 0)
TuneBoostTreeInternalOptimizer()
TuneBoostTreeOptimizerRBayesianOptimization(acquisition = c("ucb", "ei", "poi"), kappa = 2.576, eps = 0,
  fallback = TRUE)
TuneBoostTreeRBayesianOptimization(acquisition = c("ucb", "ei", "poi"), kappa = 2.576, eps = 0,
  fallback = TRUE)
TuneBoostTreeImbalance(balanceFn = NULL, scale_pos_weight = "auto", ...)
TuneBoostTreePerformance(metric = "pr_auc", backend = "auto")
TuneBoostTreeControl(seed = 42L, parallel = "auto", verbose = TRUE,
  fallback_trees = 100L)
TuneBoostTreeParallel(workers = "auto", threads_per_worker = "auto",
  strategy = c("auto", "folds", "engine", "sequential"))
TuneBoostTreeXgboost(eval_metric = "aucpr", tree_method = "hist", feature_types = NULL)
TuneBoostTreeLightgbm(metric = "average_precision")
TuneBoostTreeUltraConfig(command = NULL, strict_limbo = TRUE)
TuneBoostTreeBayesianUltra(formula, data, initial = 20L, nIter = 60L,
  engine = "lightgbm", command = NULL, strict_limbo = TRUE)
}
\arguments{
\item{trees, stop_iter, learn_rate, tree_depth, min_n, loss_reduction, sample_size, mtry}{Controles de boosted tree usando nomenclatura de parsnip. Um valor escalar fixa o parâmetro; \code{NULL} deixa o parâmetro disponível para tuning apenas se também aparecer no espaço de busca. Para \code{mtry}, \code{"default"} fixa a fração de features em \code{0.8}, um número fixa outra fração em \code{(0, 1]} e \code{NULL} permite tuning quando houver limites em \code{TuneBoostTreeSearchSpace()}.}
\item{max_bin}{Valor fixo opcional em \code{TuneBoostTreeBoostParams()} ou limites opcionais em \code{TuneBoostTreeSearchSpace()}. Controla o número de bins de histograma das engines.}
\item{lambda}{Limites de regularização L2. Mapeia para \code{lambda} no XGBoost e \code{lambda_l2} no LightGBM.}
\item{alpha}{Limites de regularização L1. Mapeia para \code{alpha} no XGBoost e \code{lambda_l1} no LightGBM.}
\item{max_delta_step}{Limites opcionais do estabilizador logístico do XGBoost, útil em desbalanceamento severo.}
\item{colsample_bytree}{Limites de amostragem de features por árvore. Mapeia para \code{colsample_bytree} no XGBoost e \code{feature_fraction} no LightGBM.}
\item{colsample_bylevel}{Limites opcionais de amostragem de features por nível no XGBoost.}
\item{num_leaves}{Limites opcionais do número de folhas no LightGBM; controla complexidade.}
\item{min_data_in_leaf}{Limites opcionais do mínimo de linhas por folha no LightGBM; controla sobreajuste.}
\item{scale_pos_weight}{\code{"auto"}, \code{NULL}, escalar numérico positivo ou limites opcionais no espaço de busca. \code{"auto"} calcula a razão negativos/positivos no treino de cada fold.}
\item{folds, stratified}{Configuração de validação cruzada. \code{stratified} deve ser \code{TRUE}; os folds preservam a distribuição de classes sempre que possível.}
\item{command, fallback, acquisition, kappa, eps}{Configuração do Limbo ou rBayesianOptimization. \code{acquisition} aceita \code{"ucb"} (upper confidence bound, equilibra score previsto e incerteza), \code{"ei"} (expected improvement) ou \code{"poi"} (probability of improvement). \code{kappa} controla exploração no UCB; \code{eps} controla a margem de melhoria em EI/POI.}
\item{balanceFn}{Função opcional chamada como \code{balanceFn(data, formula, ...)} uma vez por treino de fold. Deve retornar data.frame, tibble ou data.table com a resposta e todos os preditores exigidos por \code{formula}.}
\item{...}{Argumentos extras repassados apenas para \code{balanceFn}.}
\item{metric, backend}{Configuração de métrica e backend de PR-AUC. Atualmente a métrica suportada é \code{"pr_auc"}; \code{backend = "auto"} escolhe a implementação disponível.}
\item{seed, parallel, verbose, fallback_trees}{Controles de execução. \code{parallel} define workers de fold e threads por engine; \code{verbose} controla mensagens de progresso do pacote; \code{fallback_trees} é usado quando o melhor número de árvores não é recuperado via early stopping.}
\item{workers, threads_per_worker, strategy}{Controles explícitos de paralelismo. \code{workers} define folds simultâneos; \code{threads_per_worker} define threads por treino de engine; \code{strategy} escolhe a preferência entre folds, engine ou execução sequencial.}
\item{eval_metric, tree_method, feature_types}{Controles específicos de XGBoost.}
\item{formula, data, initial, nIter, engine}{Argumentos repassados a \code{TuneBoostTreeBayesianUltra()}. A resposta em \code{data} deve representar classificação binária.}
\item{strict_limbo}{Quando \code{TRUE}, a configuração ultra exige executável Limbo funcional.}
}
\value{Cada auxiliar retorna uma lista validada consumida por \code{TuneBoostTree()}. As classes retornadas incluem \code{tbtb_boost_params}, \code{tbtb_search_space}, \code{tbtb_cv}, \code{tbtb_optimizer}, \code{tbtb_imbalance}, \code{tbtb_performance}, \code{tbtb_control}, \code{tbtb_parallel} e \code{tbtb_engine}. Saídas tabulares de tuning e predição são tibbles.}
\description{Construtores pequenos e explícitos para documentar cenários de tuning e manter a função principal compacta.}
\details{O perfil padrão otimiza \code{learn_rate}, \code{tree_depth}, \code{min_n}, \code{loss_reduction} e \code{sample_size}. Entradas opcionais como \code{mtry}, \code{max_bin}, \code{lambda}, \code{alpha}, \code{colsample_bytree}, limites específicos de engine e \code{scale_pos_weight} continuam disponíveis quando informadas explicitamente, mas não são tunadas por padrão para manter a busca Bayesiana focada e rápida.}
