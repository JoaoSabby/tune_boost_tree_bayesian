#' Tunar hiperparâmetros de gradient boosted trees
#'
#' @description
#' Função principal do pacote. Executa tuning de boosted trees para classificação
#' binária com validação cruzada estratificada, `early stopping`, otimização
#' Bayesiana, tratamento opcional de desbalanceamento e engines XGBoost ou
#' LightGBM.
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
#'   de treino e as colunas referenciadas por `formula`; não passe `sparseMatrix`
#'   diretamente como tabela de entrada. Internamente a entrada tabular é
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
#'   Define limites inferiores/superiores dos parâmetros que serão otimizados.
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

TuneBoostTree <- function(
  formula,
  data,
  initial = 10L,
  nIter = 30L,
  engine = "lightgbm",
  boost = TuneBoostTreeBoostParams(),
  searchSpace = TuneBoostTreeSearchSpace(),
  cv = TuneBoostTreeCv(),
  optimizer = TuneBoostTreeOptimizerRBayesianOptimization(),
  imbalance = TuneBoostTreeImbalance(),
  performance = TuneBoostTreePerformance(),
  control = TuneBoostTreeControl()
) {

  if(!inherits(formula, "formula") || length(formula) != 3L) {
    cli::cli_abort("`formula` must be a two-sided formula.")
  }
  if(!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_abort("`data` must be a non-empty data.frame, tibble, or data.table.")
  }
  data <- as.data.frame(data)
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
  if(length(nIter) != 1L || is.na(nIter) || nIter < 0L) {
    cli::cli_abort("`nIter` must be a non-negative integer.")
  }

  timerStart <- proc.time()[["elapsed"]]
  parameterNames <- names(bounds)
  nFolds <- cv$folds
  formulaInfoForTarget <- TuneBoostTree_ExtractFormulaInfo(formula, data)
  preparedTargetForCv <- TuneBoostTree_PrepareTarget(
    data[[formulaInfoForTarget$targetName]],
    NULL
  )
  nFolds <- TuneBoostTree_ValidateCvClassCounts(preparedTargetForCv$yData, nFolds)
  cv$folds <- nFolds
  nRoundsTuning <- boost$trees
  earlyStoppingRounds <- boost$stop_iter
  seed <- control$seed
  runtime <- TuneBoostTree_ResolveParallel(control$parallel, nrow(data), nFolds)
  nWorkersFolds <- runtime$workers
  workerThreads <- runtime$threads_per_worker
  prAucBackend <- TuneBoostTree_SelectPrAucBackend(performance$backend)
  engineConfig <- engine
  engine <- engineConfig$name
  if(identical(engine, "lightgbm") && nWorkersFolds > 1L) {
    workerThreads <- max(1L, as.integer(workerThreads) * as.integer(nWorkersFolds))
    nWorkersFolds <- 1L
  }
  if(engine == "xgboost") {
    evalMetric <- engineConfig$eval_metric
  } else {
    evalMetric <- engineConfig$metric
  }
  featureTypes <- engineConfig$feature_types

  balanceFn <- imbalance$balanceFn
  useBalancedCv <- !is.null(balanceFn)
  if(useBalancedCv) {
    balancedFolds <- TuneBoostTree_PrepareBalancedFolds(
      formula,
      data,
      nFolds,
      balanceFn,
      imbalance$balance_args,
      imbalance$scale_pos_weight,
      workerThreads,
      seed,
      engine,
      preparedTargetForCv$targetLevels
    )
    scalePosWeightValue <- NULL
  } else {
    formulaInfo <- formulaInfoForTarget
    preparedTrain <- TuneBoostTree_PrepareMatrix(
      formula,
      data,
      featureTypes,
      preparedTargetForCv$targetLevels,
      formulaInfo
    )
    classCounts <- table(preparedTrain$yData)
    if(length(classCounts) != 2L || any(classCounts == 0L)) {
      cli::cli_abort("`data` must contain both binary classes.")
    }
    scalePosWeightValue <- TuneBoostTree_ResolveScalePosWeight(
      preparedTrain$yData,
      imbalance$scale_pos_weight
    )
    folds <- TuneBoostTree_CreateStratifiedFolds(preparedTrain$yData, nFolds, seed)
    balancedFolds <- vector("list", length(folds))
    for(foldId in seq_along(folds)) {
      testIndex <- folds[[foldId]]
      trainIndex <- setdiff(seq_len(nrow(data)), testIndex)
      trainMatrix <- preparedTrain$xMatrix[trainIndex, , drop = FALSE]
      testMatrix <- preparedTrain$xMatrix[testIndex, , drop = FALSE]
      foldScalePosWeight <- TuneBoostTree_ResolveScalePosWeight(
        preparedTrain$yData[trainIndex],
        imbalance$scale_pos_weight
      )

      balancedFolds[[foldId]] <- list(
        xTrain = trainMatrix,
        yTrain = preparedTrain$yData[trainIndex],
        xTest = testMatrix,
        yTest = preparedTrain$yData[testIndex],
        featureTypes = preparedTrain$featureTypes,
        scalePosWeight = foldScalePosWeight,
        featureNames = preparedTrain$featureNames,
        targetLevels = preparedTrain$targetLevels
      )
    }
  }

  cacheEnv <- new.env(parent = emptyenv())
  on.exit({
    if(exists("cacheEnv", inherits = FALSE) && is.environment(cacheEnv)) {
      rm(list = ls(envir = cacheEnv, all.names = TRUE), envir = cacheEnv)
    }
  }, add = TRUE)
  evaluationLogList <- vector(
    "list",
    max(64L, as.integer(initPoints) + as.integer(nIter) + 32L)
  )
  logIndex <- 0L
  objective <- TuneBoostTree_EvaluateCv
  environment(objective) <- environment()

  if(isTRUE(control$verbose)) {
    cli::cli_inform(paste0(
      "Starting {.val {engine}} Bayesian tuning with {.val {nRoundsTuning}} trees, ",
      "{.val {earlyStoppingRounds}} stop_iter, and {.val {nWorkersFolds}} fold worker(s)."
    ))
  }
  set.seed(seed)
  tuningResult <- TuneBoostTree_RunOptimizer(
    objective = objective,
    bounds = bounds,
    initGridDt = initGridDt,
    initPoints = initPoints,
    nIter = nIter,
    acq = optimizer$acquisition,
    kappa = optimizer$kappa,
    eps = optimizer$eps,
    verbose = control$verbose,
    seed = seed,
    optimizerBackend = optimizer$type,
    limboCommand = optimizer$command,
    limboFallback = optimizer$fallback
  )

  if(logIndex > 0L) {
    evaluationLog <- TuneBoostTree_AsTibble(
      data.table::rbindlist(evaluationLogList[seq_len(logIndex)], fill = TRUE)
    )
  } else {
    evaluationLog <- tibble::tibble()
  }
  bestHyperparameters <- as.list(tuningResult$Best_Par)
  fixedBoostNames <- setdiff(
    names(boost)[!vapply(boost, is.null, logical(1L))],
    c("trees", "stop_iter")
  )
  for(fixedName in setdiff(fixedBoostNames, names(bestHyperparameters))) {
    bestHyperparameters[[fixedName]] <- boost[[fixedName]]
  }
  bestScore <- as.numeric(tuningResult$Best_Value)
  bestIteration <- TuneBoostTree_FindBestIteration(
    evaluationLog,
    bestHyperparameters,
    bestScore,
    bounds
  )
  if(is.null(bestIteration)) {
    bestSummary <- TuneBoostTree_RunCvManual(
      balancedFolds,
      bestHyperparameters,
      nRoundsTuning,
      earlyStoppingRounds,
      seed,
      workerThreads,
      nWorkersFolds,
      evalMetric,
      engine,
      prAucBackend
    )
    bestIteration <- as.integer(bestSummary$bestIteration)
  }
  if(is.null(bestIteration) || is.na(bestIteration) || bestIteration < 1L) {
    bestIteration <- as.integer(control$fallback_trees)
  }
  bestHyperparameters$trees <- as.integer(bestIteration)
  bestHyperparameters$stop_iter <- as.integer(earlyStoppingRounds)
  bestHyperparameters$eval_metric <- evalMetric
  if(!useBalancedCv && is.null(bestHyperparameters$scale_pos_weight)) {
    bestHyperparameters$scale_pos_weight <- scalePosWeightValue
  }
  bestThresholdSummary <- TuneBoostTree_OptimizeThresholdCv(
    balancedFolds,
    bestHyperparameters,
    bestHyperparameters$trees,
    seed,
    workerThreads,
    nWorkersFolds,
    evalMetric,
    engine,
    prAucBackend
  )
  bestHyperparameters$threshold <- as.numeric(bestThresholdSummary$threshold)

  newInitGridDt <- TuneBoostTree_CreateInitGrid(evaluationLog, bounds)
  returnedInitGridDt <- TuneBoostTree_AsTibble(
    TuneBoostTree_CombineInitGrid(initGridDt, newInitGridDt, bounds)
  )
  if(isTRUE(control$verbose)) {
    cli::cli_inform("Finished Bayesian tuning in {.val {round(proc.time()[['elapsed']] - timerStart, 2)}} seconds.")
  }

  out <- list(
    bestHyperparameters = bestHyperparameters,
    bestScore = bestScore,
    bestThreshold = bestThresholdSummary,
    initial = returnedInitGridDt,
    evaluationLog = evaluationLog,
    config = list(
      engine = engineConfig,
      boost = boost,
      searchSpace = bounds,
      cv = cv,
      optimizer = optimizer,
      imbalance = imbalance,
      performance = performance,
      control = control,
      parallel = runtime
    )
  )
  class(out) <- c("tbtb_tune_result", "list")
  out
}
####
## Fim
#

