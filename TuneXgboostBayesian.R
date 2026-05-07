#' TuneXgboostBayesian
#'
#' @description
#' Tuning bayesiano para XGBoost com foco em robustez, reuso de objetos de dados,
#' reducao de copias e suporte direto a treinamento/predicao apos otimizacao.
#'
#' @details
#' Melhorias principais desta versao:
#' - Remove rbind incremental do log; usa lista e consolida ao final.
#' - Reusa DMatrix e folds no ramo sem balanceamento.
#' - Prepara folds balanceados com cache de teste no ramo com balanceamento.
#' - Exponibiliza funcoes diretas de treino e predicao em padrao Fit/Predict.
#' - Valida intensamente entradas para reduzir falhas tardias.
#'
#' @keywords internal
NULL

.TuneXgboostState <- new.env(parent = emptyenv())
.TuneXgboostState$packagesLoaded <- FALSE

#' Carregar pacotes necessarios
#' @export
TuneXgboostLoadPackages <- function(){
  if(isTRUE(.TuneXgboostState$packagesLoaded)){
    return(invisible(TRUE))
  }

  packageNames <- c(
    "xgboost",
    "rBayesianOptimization",
    "Matrix",
    "stringr",
    "data.table"
  )

  for(packageName in packageNames){
    if(!requireNamespace(packageName, quietly = TRUE)){
      stop("Pacote necessario nao encontrado: ", packageName, call. = FALSE)
    }
  }

  .TuneXgboostState$packagesLoaded <- TRUE
  invisible(TRUE)
}

#' Extrair informacoes da formula
#' @export
TuneXgboostExtractFormulaInfo <- function(formula, data){
  TuneXgboostLoadPackages()

  if(!inherits(formula, "formula")){
    stop("'formula' deve ser formula", call. = FALSE)
  }
  if(length(formula) != 3L){
    stop("'formula' deve conter outcome e preditores", call. = FALSE)
  }
  if(!is.data.frame(data)){
    stop("'data' deve ser data.frame", call. = FALSE)
  }

  targetVariables <- all.vars(formula[[2L]])
  if(length(targetVariables) != 1L){
    stop("A formula deve conter um unico outcome", call. = FALSE)
  }

  targetName <- targetVariables[1L]
  if(!(targetName %in% names(data))){
    stop("Outcome nao encontrado nos dados: ", targetName, call. = FALSE)
  }

  termsValue <- terms(formula, data = data)
  predictorNames <- attr(termsValue, "term.labels")
  termOrders <- attr(termsValue, "order")

  if(length(predictorNames) == 0L){
    stop("A formula deve conter ao menos um preditor", call. = FALSE)
  }
  if(any(termOrders > 1L)){
    stop("A formula deve conter apenas colunas diretas, sem interacoes", call. = FALSE)
  }

  invalidNames <- setdiff(predictorNames, names(data))
  if(length(invalidNames) > 0L){
    stop("Preditores nao encontrados nos dados: ", paste(invalidNames, collapse = ", "), call. = FALSE)
  }

  list(
    targetName = targetName,
    predictorNames = predictorNames,
    termsValue = termsValue
  )
}

#' Preparar target binario
#' @export
TuneXgboostPrepareTarget <- function(targetData, targetLevels = NULL){
  TuneXgboostLoadPackages()

  if(length(targetData) == 0L){
    stop("targetData nao pode ser vazio", call. = FALSE)
  }
  if(anyNA(targetData)){
    stop("targetData nao pode conter NA", call. = FALSE)
  }

  if(is.null(targetLevels)){
    if(is.factor(targetData)){
      targetLevels <- rev(levels(targetData))
    } else if(is.numeric(targetData) || is.integer(targetData)){
      targetLevels <- as.character(sort(unique(targetData)))
    } else {
      targetLevels <- sort(unique(as.character(targetData)))
    }
  }

  targetLevels <- as.character(targetLevels)
  if(length(targetLevels) != 2L){
    stop("O outcome deve conter exatamente duas classes", call. = FALSE)
  }

  targetText <- as.character(targetData)
  invalidTargetValues <- setdiff(unique(targetText), targetLevels)
  if(length(invalidTargetValues) > 0L){
    stop("targetData contem valores fora de targetLevels", call. = FALSE)
  }

  negativeClass <- targetLevels[1L]
  positiveClass <- targetLevels[2L]
  yData <- as.integer(targetText == positiveClass)

  list(
    yData = yData,
    targetLevels = targetLevels,
    negativeClass = negativeClass,
    positiveClass = positiveClass
  )
}

#' Validar scale_pos_weight
#' @export
TuneXgboostValidateScalePosWeight <- function(scalePosWeight, context = "scalePosWeight"){
  scalePosWeight <- as.numeric(scalePosWeight)
  if(length(scalePosWeight) != 1L || is.na(scalePosWeight) || !is.finite(scalePosWeight) || scalePosWeight <= 0){
    stop(context, " deve ser finito e maior que zero", call. = FALSE)
  }
  scalePosWeight
}

#' Validar metrica de avaliacao
#' @export
TuneXgboostValidateEvalMetric <- function(evalMetric){
  evalMetric <- as.character(evalMetric)
  allowedMetrics <- c("aucpr", "auc")
  if(length(evalMetric) != 1L || is.na(evalMetric) || !(evalMetric %in% allowedMetrics)){
    stop("evalMetric deve ser uma de: ", paste(allowedMetrics, collapse = ", "), call. = FALSE)
  }
  evalMetric
}

#' Obter valor de hiperparametro com default
#' @export
TuneXgboostGetHyperparameter <- function(hyperparameters, parameterName, defaultValue = NULL){
  if(is.null(hyperparameters)) return(defaultValue)
  if(!(parameterName %in% names(hyperparameters))) return(defaultValue)
  parameterValue <- hyperparameters[[parameterName]]
  if(is.null(parameterValue)) return(defaultValue)
  parameterValue
}

#' Validar feature types
#' @export
TuneXgboostValidateFeatureTypes <- function(featureTypes, featureNames){
  if(is.null(featureTypes)) return(NULL)
  featureTypes <- as.character(featureTypes)
  if(length(featureTypes) != length(featureNames)){
    stop("featureTypes deve ter o mesmo tamanho de featureNames", call. = FALSE)
  }
  names(featureTypes) <- featureNames
  featureTypes
}

#' Preparar matriz de preditores em dgCMatrix
#' @export
TuneXgboostPrepareSparseMatrix <- function(
  formula,
  data,
  featureTypes = NULL,
  targetLevels = NULL,
  formulaInfo = NULL
){
  TuneXgboostLoadPackages()

  if(is.null(formulaInfo)){
    formulaInfo <- TuneXgboostExtractFormulaInfo(formula = formula, data = data)
  }

  targetName <- formulaInfo$targetName
  featureNames <- formulaInfo$predictorNames

  missingFeatureNames <- setdiff(featureNames, names(data))
  if(length(missingFeatureNames) > 0L){
    stop("Preditores ausentes nos dados: ", paste(missingFeatureNames, collapse = ", "), call. = FALSE)
  }
  if(!(targetName %in% names(data))){
    stop("Outcome ausente nos dados: ", targetName, call. = FALSE)
  }

  xData <- data[, featureNames, drop = FALSE]
  nonNumericNames <- names(xData)[!vapply(xData, is.numeric, logical(1L))]
  if(length(nonNumericNames) > 0L){
    stop("Todos os preditores devem ser numericos: ", paste(nonNumericNames, collapse = ", "), call. = FALSE)
  }

  preparedTarget <- TuneXgboostPrepareTarget(targetData = data[[targetName]], targetLevels = targetLevels)
  featureTypes <- TuneXgboostValidateFeatureTypes(featureTypes, featureNames)

  numericMatrix <- data.matrix(xData)
  storage.mode(numericMatrix) <- "double"
  xMatrix <- Matrix::Matrix(data = numericMatrix, sparse = TRUE)
  colnames(xMatrix) <- featureNames

  list(
    xMatrix = xMatrix,
    yData = preparedTarget$yData,
    featureNames = featureNames,
    featureTypes = featureTypes,
    targetLevels = preparedTarget$targetLevels,
    targetName = targetName,
    negativeClass = preparedTarget$negativeClass,
    positiveClass = preparedTarget$positiveClass,
    formulaInfo = formulaInfo
  )
}

#' Criar xgb.DMatrix
#' @export
TuneXgboostCreateDMatrix <- function(xMatrix, yData = NULL, featureTypes = NULL, nThreads = 8L){
  TuneXgboostLoadPackages()
  dmatrixArgs <- list(
    data = xMatrix,
    nthread = as.integer(nThreads)
  )
  if(!is.null(yData)){
    dmatrixArgs$label <- yData
  }
  if(!is.null(featureTypes)){
    dmatrixArgs$feature_types <- unname(featureTypes)
  }
  do.call(xgboost::xgb.DMatrix, dmatrixArgs)
}

#' Criar folds estratificados
#' @export
TuneXgboostCreateStratifiedFolds <- function(yData, nFolds = 10L, seed = 42L){
  yData <- as.integer(yData)
  if(length(yData) == 0L) stop("yData nao pode ser vazio", call. = FALSE)
  if(anyNA(yData)) stop("yData nao pode conter NA", call. = FALSE)
  if(!all(yData %in% c(0L, 1L))) stop("yData deve conter apenas 0 e 1", call. = FALSE)
  nFolds <- as.integer(nFolds)
  if(nFolds < 2L) stop("nFolds deve ser >= 2", call. = FALSE)

  classCounts <- table(yData)
  if(length(classCounts) != 2L) stop("yData deve conter ambas as classes", call. = FALSE)
  if(any(classCounts < nFolds)) stop("Cada classe deve ter ao menos nFolds observacoes", call. = FALSE)

  set.seed(seed)
  negativeIndex <- sample(which(yData == 0L))
  positiveIndex <- sample(which(yData == 1L))
  folds <- vector("list", nFolds)

  for(foldId in seq_len(nFolds)){
    folds[[foldId]] <- c(
      negativeIndex[seq(foldId, length(negativeIndex), by = nFolds)],
      positiveIndex[seq(foldId, length(positiveIndex), by = nFolds)]
    )
  }

  if(any(lengths(folds) == 0L)){
    stop("Criacao de folds gerou dobras vazias", call. = FALSE)
  }
  folds
}

#' Limites padrao do tuning
#' @export
TuneXgboostGetDefaultBounds <- function(){
  list(
    eta = c(0.01, 0.15),
    maxDepth = c(2L, 10L),
    minChildWeight = c(1, 50),
    subsample = c(0.5, 1),
    colsampleBynode = c(0.3, 1),
    gamma = c(0, 5),
    maxBin = c(32L, 256L)
  )
}

#' Validar bounds
#' @export
TuneXgboostValidateBounds <- function(bounds){
  requiredNames <- c("eta", "maxDepth", "minChildWeight", "subsample", "colsampleBynode", "gamma", "maxBin")
  if(is.null(bounds) || !is.list(bounds)) stop("bounds deve ser lista nomeada", call. = FALSE)
  missingNames <- setdiff(requiredNames, names(bounds))
  extraNames <- setdiff(names(bounds), requiredNames)
  if(length(missingNames) > 0L) stop("bounds sem parametros obrigatorios: ", paste(missingNames, collapse = ", "), call. = FALSE)
  if(length(extraNames) > 0L) stop("bounds contem parametros extras: ", paste(extraNames, collapse = ", "), call. = FALSE)

  for(parameterName in requiredNames){
    parameterBounds <- bounds[[parameterName]]
    if(length(parameterBounds) != 2L) stop("bounds de ", parameterName, " deve ter comprimento 2", call. = FALSE)
    if(any(is.na(parameterBounds)) || any(!is.finite(parameterBounds))) stop("bounds de ", parameterName, " deve conter valores finitos", call. = FALSE)
    if(parameterBounds[1L] > parameterBounds[2L]) stop("bounds de ", parameterName, " deve estar em ordem crescente", call. = FALSE)
  }

  bounds
}

#' Construir params do XGBoost
#' @export
TuneXgboostBuildParams <- function(
  hyperparameters,
  nThreads = 8L,
  scalePosWeight = 1,
  seed = 42L,
  evalMetric = "aucpr"
){
  evalMetric <- TuneXgboostValidateEvalMetric(evalMetric)

  etaValue <- as.numeric(TuneXgboostGetHyperparameter(hyperparameters, "eta", 0.0375))
  maxDepthValue <- as.integer(round(TuneXgboostGetHyperparameter(hyperparameters, "maxDepth", 6L)))
  minChildWeightValue <- as.numeric(TuneXgboostGetHyperparameter(hyperparameters, "minChildWeight", 2))
  subsampleValue <- as.numeric(TuneXgboostGetHyperparameter(hyperparameters, "subsample", 0.8))
  colsampleBynodeValue <- as.numeric(TuneXgboostGetHyperparameter(hyperparameters, "colsampleBynode", 0.8))
  gammaValue <- as.numeric(TuneXgboostGetHyperparameter(hyperparameters, "gamma", 0.412))
  maxBinValue <- as.integer(round(TuneXgboostGetHyperparameter(hyperparameters, "maxBin", 128L)))

  if(etaValue <= 0 || etaValue > 1) stop("eta invalido", call. = FALSE)
  if(maxDepthValue < 1L) stop("maxDepth invalido", call. = FALSE)
  if(minChildWeightValue < 0) stop("minChildWeight invalido", call. = FALSE)
  if(subsampleValue <= 0 || subsampleValue > 1) stop("subsample invalido", call. = FALSE)
  if(colsampleBynodeValue <= 0 || colsampleBynodeValue > 1) stop("colsampleBynode invalido", call. = FALSE)
  if(gammaValue < 0) stop("gamma invalido", call. = FALSE)
  if(maxBinValue < 2L) stop("maxBin invalido", call. = FALSE)
  if(as.integer(nThreads) < 1L) stop("nThreads invalido", call. = FALSE)

  scalePosWeight <- TuneXgboostValidateScalePosWeight(scalePosWeight)

  list(
    objective = "binary:logistic",
    eval_metric = evalMetric,
    grow_policy = "depthwise",
    tree_method = "hist",
    max_bin = maxBinValue,
    max_depth = maxDepthValue,
    eta = etaValue,
    gamma = gammaValue,
    subsample = subsampleValue,
    min_child_weight = minChildWeightValue,
    colsample_bynode = colsampleBynodeValue,
    scale_pos_weight = scalePosWeight,
    nthread = as.integer(nThreads),
    seed = as.integer(seed)
  )
}

#' Normalizar hiperparametros efetivos
#' @export
TuneXgboostNormalizeParameterData <- function(parameterData, parameterNames){
  parameterData <- as.data.frame(parameterData[, parameterNames, drop = FALSE])
  for(parameterName in parameterNames){
    parameterData[[parameterName]] <- as.numeric(parameterData[[parameterName]])
  }
  if("maxDepth" %in% parameterNames){
    parameterData$maxDepth <- as.integer(round(parameterData$maxDepth))
  }
  if("maxBin" %in% parameterNames){
    parameterData$maxBin <- as.integer(round(parameterData$maxBin))
  }
  continuousNames <- setdiff(parameterNames, c("maxDepth", "maxBin"))
  for(parameterName in continuousNames){
    parameterData[[parameterName]] <- round(parameterData[[parameterName]], digits = 12L)
  }
  parameterData
}

#' Comparar scores
#' @export
TuneXgboostIsScoreMatch <- function(scoreA, scoreB, tolerance = 1e-6){
  scoreA <- as.numeric(scoreA)
  scoreB <- as.numeric(scoreB)
  if(!is.finite(scoreA) || !is.finite(scoreB)) return(FALSE)
  abs(scoreA - scoreB) <= tolerance * max(1, abs(scoreA), abs(scoreB))
}

#' Calcular PR AUC
#' @export
TuneXgboostCalculatePrAuc <- function(actual, predicted){
  actual <- as.integer(actual)
  predicted <- as.numeric(predicted)
  if(length(actual) != length(predicted)) stop("actual e predicted devem ter o mesmo tamanho", call. = FALSE)
  if(anyNA(actual) || anyNA(predicted)) stop("actual/predicted nao podem conter NA", call. = FALSE)
  if(!all(actual %in% c(0L, 1L))) stop("actual deve conter 0/1", call. = FALSE)

  positiveCount <- sum(actual == 1L)
  if(positiveCount == 0L) return(NA_real_)

  ord <- order(predicted, decreasing = TRUE)
  actualOrd <- actual[ord]
  tp <- cumsum(actualOrd == 1L)
  fp <- cumsum(actualOrd == 0L)
  precision <- tp / pmax(tp + fp, 1)
  recall <- tp / positiveCount

  recall <- c(0, recall)
  precision <- c(1, precision)
  sum((recall[-1L] - recall[-length(recall)]) * precision[-1L])
}

#' Executar CV nativo do XGBoost
#' @export
TuneXgboostRunCv <- function(
  dtrain,
  folds,
  hyperparameters,
  nRounds,
  earlyStoppingRounds,
  seed,
  nThreads,
  scalePosWeight,
  evalMetric,
  verbose = FALSE
){
  paramsValue <- TuneXgboostBuildParams(
    hyperparameters = hyperparameters,
    nThreads = nThreads,
    scalePosWeight = scalePosWeight,
    seed = seed,
    evalMetric = evalMetric
  )

  set.seed(seed)
  cvResult <- xgboost::xgb.cv(
    params = paramsValue,
    data = dtrain,
    nrounds = as.integer(nRounds),
    folds = folds,
    early_stopping_rounds = as.integer(earlyStoppingRounds),
    maximize = TRUE,
    verbose = verbose,
    showsd = FALSE,
    prediction = FALSE
  )

  evaluationLog <- cvResult$evaluation_log
  scoreColumn <- grep(
    pattern = stringr::str_c("test_", evalMetric, "_mean"),
    x = names(evaluationLog),
    value = TRUE
  )[1L]
  if(is.na(scoreColumn)){
    stop("Coluna de metrica nao encontrada no evaluation_log", call. = FALSE)
  }

  scoreValues <- evaluationLog[[scoreColumn]]
  bestIteration <- cvResult$best_iteration
  if(is.null(bestIteration) || is.na(bestIteration)){
    bestIteration <- which.max(scoreValues)
  }

  list(
    score = as.numeric(scoreValues[[bestIteration]]),
    bestIteration = as.integer(bestIteration),
    evaluationLog = evaluationLog
  )
}

#' Preparar folds balanceados com cache de teste
#' @export
TuneXgboostPrepareBalancedFolds <- function(
  formula,
  data,
  nFolds,
  balanceFn,
  nThreads = 1L,
  seed = 42L
){
  TuneXgboostLoadPackages()
  if(!is.function(balanceFn)) stop("balanceFn deve ser funcao", call. = FALSE)

  formulaInfo <- TuneXgboostExtractFormulaInfo(formula = formula, data = data)
  preparedFull <- TuneXgboostPrepareSparseMatrix(formula = formula, data = data, formulaInfo = formulaInfo)
  folds <- TuneXgboostCreateStratifiedFolds(yData = preparedFull$yData, nFolds = nFolds, seed = seed)

  balancedFolds <- vector("list", length(folds))

  for(foldId in seq_along(folds)){
    testIndex <- folds[[foldId]]
    trainIndex <- setdiff(seq_len(nrow(data)), testIndex)

    trainData <- data[trainIndex, , drop = FALSE]
    testData <- data[testIndex, , drop = FALSE]

    balancedTrain <- balanceFn(trainData, formula)
    if(!is.data.frame(balancedTrain)){
      stop("balanceFn deve retornar data.frame", call. = FALSE)
    }

    preparedTrain <- TuneXgboostPrepareSparseMatrix(formula = formula, data = balancedTrain, formulaInfo = formulaInfo)
    preparedTest <- TuneXgboostPrepareSparseMatrix(
      formula = formula,
      data = testData,
      formulaInfo = formulaInfo,
      targetLevels = preparedTrain$targetLevels
    )

    dtrain <- TuneXgboostCreateDMatrix(
      xMatrix = preparedTrain$xMatrix,
      yData = preparedTrain$yData,
      featureTypes = preparedTrain$featureTypes,
      nThreads = nThreads
    )

    dtest <- TuneXgboostCreateDMatrix(
      xMatrix = preparedTest$xMatrix,
      yData = preparedTest$yData,
      featureTypes = preparedTest$featureTypes,
      nThreads = nThreads
    )

    classCounts <- table(preparedTrain$yData)
    if(length(classCounts) != 2L || any(classCounts == 0L)){
      stop("Treino balanceado do fold nao contem ambas as classes", call. = FALSE)
    }

    scalePosWeight <- as.numeric(classCounts[[1L]] / classCounts[[2L]])
    scalePosWeight <- TuneXgboostValidateScalePosWeight(scalePosWeight, context = "scalePosWeight do fold")

    balancedFolds[[foldId]] <- list(
      dtrain = dtrain,
      dtest = dtest,
      yTest = preparedTest$yData,
      scalePosWeight = scalePosWeight,
      featureNames = preparedTrain$featureNames,
      targetLevels = preparedTrain$targetLevels
    )
  }

  balancedFolds
}

#' Executar CV manual com folds balanceados
#' @export
TuneXgboostRunCvManual <- function(
  balancedFolds,
  formula,
  hyperparameters,
  nRounds,
  earlyStoppingRounds,
  seed,
  nThreads,
  evalMetric
){
  TuneXgboostLoadPackages()
  if(!is.list(balancedFolds) || length(balancedFolds) == 0L){
    stop("balancedFolds deve ser lista nao vazia", call. = FALSE)
  }

  foldScores <- numeric(length(balancedFolds))
  foldBestIter <- integer(length(balancedFolds))

  for(foldId in seq_along(balancedFolds)){
    foldData <- balancedFolds[[foldId]]
    paramsValue <- TuneXgboostBuildParams(
      hyperparameters = hyperparameters,
      nThreads = nThreads,
      scalePosWeight = foldData$scalePosWeight,
      seed = seed,
      evalMetric = evalMetric
    )

    watchlistFold <- list(train = foldData$dtrain, eval = foldData$dtest)
    foldModel <- xgboost::xgb.train(
      params = paramsValue,
      data = foldData$dtrain,
      nrounds = as.integer(nRounds),
      watchlist = watchlistFold,
      early_stopping_rounds = as.integer(earlyStoppingRounds),
      maximize = TRUE,
      verbose = 0L
    )

    bestIterFold <- as.integer(foldModel$best_iteration)
    if(is.null(bestIterFold) || is.na(bestIterFold) || bestIterFold < 1L){
      bestIterFold <- as.integer(nRounds)
    }

    predictedProbability <- as.numeric(stats::predict(foldModel, newdata = foldData$dtest))
    foldScores[[foldId]] <- TuneXgboostCalculatePrAuc(actual = foldData$yTest, predicted = predictedProbability)
    foldBestIter[[foldId]] <- bestIterFold
  }

  list(
    score = as.numeric(mean(foldScores, na.rm = TRUE)),
    bestIteration = as.integer(round(mean(foldBestIter, na.rm = TRUE))),
    foldScores = foldScores
  )
}

#' Criar init grid a partir do historico
#' @export
TuneXgboostCreateInitGrid <- function(historyData, bounds){
  if(is.null(historyData) || nrow(historyData) == 0L) return(NULL)
  parameterNames <- names(bounds)
  requiredNames <- c(parameterNames, "Value")
  missingNames <- setdiff(requiredNames, names(historyData))
  if(length(missingNames) > 0L) stop("historyData incompleto", call. = FALSE)
  out <- as.data.frame(historyData[, requiredNames, drop = FALSE], stringsAsFactors = FALSE)
  out <- out[stats::complete.cases(out), , drop = FALSE]
  if(nrow(out) == 0L) return(NULL)
  out
}

#' Combinar init grids
#' @export
TuneXgboostCombineInitGrid <- function(initGridDt, newInitGridDt, bounds){
  if(is.null(initGridDt)) return(newInitGridDt)
  if(is.null(newInitGridDt)) return(initGridDt)
  combined <- rbind(initGridDt, newInitGridDt)
  TuneXgboostDeduplicateInitGrid(combined, bounds = bounds)
}

#' Deduplicar init grid
#' @export
TuneXgboostDeduplicateInitGrid <- function(gridData, bounds){
  if(is.null(gridData)) return(NULL)
  if(nrow(gridData) == 0L) return(gridData)

  parameterNames <- names(bounds)
  requiredNames <- c(parameterNames, "Value")
  missingNames <- setdiff(requiredNames, names(gridData))
  if(length(missingNames) > 0L) stop("gridData nao contem colunas obrigatorias", call. = FALSE)

  gridData <- as.data.frame(gridData[, requiredNames, drop = FALSE], stringsAsFactors = FALSE)
  completeIndex <- stats::complete.cases(gridData)
  gridData <- gridData[completeIndex, , drop = FALSE]
  if(nrow(gridData) == 0L) return(gridData)

  normalizedData <- TuneXgboostNormalizeParameterData(gridData, parameterNames)
  for(parameterName in parameterNames){
    gridData[[parameterName]] <- normalizedData[[parameterName]]
  }

  key <- do.call(paste, c(gridData[, parameterNames, drop = FALSE], sep = "|"))
  bestIndex <- tapply(seq_along(key), key, function(idx) idx[[which.max(gridData$Value[idx])]])
  gridData[unlist(bestIndex, use.names = FALSE), , drop = FALSE]
}

#' Encontrar melhor iteracao no log
#' @export
TuneXgboostFindBestIterationInLog <- function(evaluationLog, bestHyperparameters, bestScore, bounds){
  if(is.null(evaluationLog) || nrow(evaluationLog) == 0L) return(NULL)
  parameterNames <- names(bounds)
  normalizedBest <- TuneXgboostNormalizeParameterData(as.data.frame(bestHyperparameters, stringsAsFactors = FALSE), parameterNames)
  normalizedLog <- TuneXgboostNormalizeParameterData(evaluationLog, parameterNames)

  matched <- rep(TRUE, nrow(normalizedLog))
  for(parameterName in parameterNames){
    matched <- matched & (normalizedLog[[parameterName]] == normalizedBest[[parameterName]][1L])
  }
  matched <- matched & vapply(evaluationLog$Value, function(x) TuneXgboostIsScoreMatch(x, bestScore), logical(1L))
  if(!any(matched)) return(NULL)
  evaluationLog$bestIteration[[which(matched)[1L]]]
}

#' Ajustar modelo final XGBoost
#' @export
FitModelXgboost <- function(
  formula,
  dataTrain,
  hyperparameters,
  featureTypes = NULL,
  targetLevels = NULL,
  scalePosWeight = NULL,
  nThreads = 8L,
  seed = 42L,
  verbose = 0L
){
  TuneXgboostLoadPackages()

  preparedTrain <- TuneXgboostPrepareSparseMatrix(
    formula = formula,
    data = dataTrain,
    featureTypes = featureTypes,
    targetLevels = targetLevels
  )

  if(is.null(scalePosWeight)){
    classCounts <- table(preparedTrain$yData)
    if(length(classCounts) != 2L || any(classCounts == 0L)){
      stop("dataTrain deve conter ambas as classes", call. = FALSE)
    }
    scalePosWeight <- as.numeric(classCounts[[1L]] / classCounts[[2L]])
  }

  scalePosWeight <- TuneXgboostValidateScalePosWeight(scalePosWeight)
  paramsValue <- TuneXgboostBuildParams(
    hyperparameters = hyperparameters,
    nThreads = nThreads,
    scalePosWeight = scalePosWeight,
    seed = seed,
    evalMetric = TuneXgboostGetHyperparameter(hyperparameters, "evalMetric", "aucpr")
  )

  nRounds <- as.integer(TuneXgboostGetHyperparameter(hyperparameters, "nRounds", 100L))
  if(nRounds < 1L) stop("nRounds invalido", call. = FALSE)

  dtrain <- TuneXgboostCreateDMatrix(
    xMatrix = preparedTrain$xMatrix,
    yData = preparedTrain$yData,
    featureTypes = preparedTrain$featureTypes,
    nThreads = nThreads
  )

  model <- xgboost::xgb.train(
    params = paramsValue,
    data = dtrain,
    nrounds = nRounds,
    verbose = as.integer(verbose)
  )

  list(
    model = model,
    params = paramsValue,
    featureNames = preparedTrain$featureNames,
    featureTypes = preparedTrain$featureTypes,
    targetLevels = preparedTrain$targetLevels,
    targetName = preparedTrain$targetName,
    negativeClass = preparedTrain$negativeClass,
    positiveClass = preparedTrain$positiveClass,
    formulaInfo = preparedTrain$formulaInfo,
    nRounds = nRounds,
    engine = "xgboost"
  )
}

#' Predizer com modelo XGBoost
#' @export
PredictModelXGBoost <- function(modelXGBoost, newdata, threshold = 0.5){
  TuneXgboostLoadPackages()

  if(!is.data.frame(newdata)) stop("newdata deve ser data.frame", call. = FALSE)
  if(nrow(newdata) == 0L) stop("newdata nao pode ser vazio", call. = FALSE)
  threshold <- as.numeric(threshold)
  if(length(threshold) != 1L || is.na(threshold) || threshold <= 0 || threshold >= 1) stop("threshold invalido", call. = FALSE)

  if(!is.null(modelXGBoost$model) && inherits(modelXGBoost$model, "xgb.Booster")){
    modelObject <- modelXGBoost$model
    featureNames <- modelXGBoost$featureNames
    featureTypes <- modelXGBoost$featureTypes
    targetLevels <- modelXGBoost$targetLevels
    targetName <- modelXGBoost$targetName
    nThreads <- as.integer(TuneXgboostGetHyperparameter(modelXGBoost$params, "nthread", 1L))
  } else if(inherits(modelXGBoost, "xgb.Booster")){
    modelObject <- modelXGBoost
    featureNames <- modelObject$feature_names
    featureTypes <- NULL
    targetLevels <- c("0", "1")
    targetName <- NULL
    nThreads <- 1L
  } else {
    stop("Objeto de modelo invalido", call. = FALSE)
  }

  if(is.null(featureNames) || length(featureNames) == 0L){
    stop("Nao foi possivel identificar featureNames", call. = FALSE)
  }

  missingFeatureNames <- setdiff(featureNames, names(newdata))
  if(length(missingFeatureNames) > 0L){
    stop("newdata nao contem todas as variaveis esperadas: ", paste(missingFeatureNames, collapse = ", "), call. = FALSE)
  }

  xData <- newdata[, featureNames, drop = FALSE]
  nonNumericNames <- names(xData)[!vapply(xData, is.numeric, logical(1L))]
  if(length(nonNumericNames) > 0L){
    stop("Todos os preditores devem ser numericos em newdata", call. = FALSE)
  }

  numericMatrix <- data.matrix(xData)
  storage.mode(numericMatrix) <- "double"
  xMatrix <- Matrix::Matrix(numericMatrix, sparse = TRUE)
  colnames(xMatrix) <- featureNames
  dtest <- TuneXgboostCreateDMatrix(xMatrix = xMatrix, yData = NULL, featureTypes = featureTypes, nThreads = nThreads)

  probabilitySecondClass <- as.numeric(stats::predict(modelObject, newdata = dtest))
  probabilityFirstClass <- 1 - probabilitySecondClass
  predictedClass <- ifelse(probabilitySecondClass >= threshold, targetLevels[2L], targetLevels[1L])

  out <- data.frame(
    predictedClass = predictedClass,
    probabilityFirstClass = probabilityFirstClass,
    probabilitySecondClass = probabilitySecondClass,
    stringsAsFactors = FALSE
  )
  if(!is.null(targetName)){
    attr(out, "targetName") <- targetName
  }
  attr(out, "targetLevels") <- targetLevels
  out
}

#' Alias compativel com nome legado
#' @export
PredictModelXgboost <- function(modelXGBoost, newdata, threshold = 0.5){
  PredictModelXGBoost(modelXGBoost = modelXGBoost, newdata = newdata, threshold = threshold)
}

#' Tuning bayesiano de hiperparametros do XGBoost
#' @export
TuneXgboostBayesian <- function(
  formula,
  dataTrain,
  nFolds = 10L,
  initGridDt = NULL,
  initPoints = 10L,
  nIter = 30L,
  nRounds = 500L,
  earlyStoppingRounds = 15L,
  seed = 42L,
  nThreads = 8L,
  bounds = NULL,
  featureTypes = NULL,
  evalMetric = "aucpr",
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE,
  scalePosWeight = NULL,
  balanceFn = NULL
){
  TuneXgboostLoadPackages()
  evalMetric <- TuneXgboostValidateEvalMetric(evalMetric)

  if(!is.data.frame(dataTrain) || nrow(dataTrain) == 0L){
    stop("dataTrain deve ser data.frame nao vazio", call. = FALSE)
  }

  if(is.null(bounds)){
    bounds <- TuneXgboostGetDefaultBounds()
  }
  bounds <- TuneXgboostValidateBounds(bounds)

  if(!is.null(initGridDt)){
    initGridDt <- TuneXgboostDeduplicateInitGrid(initGridDt, bounds = bounds)
  }

  useBalancedCv <- !is.null(balanceFn)
  if(useBalancedCv && !is.function(balanceFn)){
    stop("balanceFn deve ser funcao com assinatura function(data, formula)", call. = FALSE)
  }

  evaluationEnvironment <- new.env(parent = emptyenv())
  evaluationEnvironment$cache <- new.env(parent = emptyenv())
  evaluationEnvironment$evaluationLogList <- vector("list", length = 0L)
  evaluationEnvironment$logIndex <- 0L

  parameterNames <- c("eta", "maxDepth", "minChildWeight", "subsample", "colsampleBynode", "gamma", "maxBin")

  appendLog <- function(logRow){
    evaluationEnvironment$logIndex <- evaluationEnvironment$logIndex + 1L
    evaluationEnvironment$evaluationLogList[[evaluationEnvironment$logIndex]] <- logRow
    invisible(NULL)
  }

  if(useBalancedCv){
    balancedFolds <- TuneXgboostPrepareBalancedFolds(
      formula = formula,
      data = dataTrain,
      nFolds = nFolds,
      balanceFn = balanceFn,
      nThreads = nThreads,
      seed = seed
    )

    TuneXgboostEvaluateCv <- function(eta, maxDepth, minChildWeight, subsample, colsampleBynode, gamma, maxBin, nThreads){
      hyperparameters <- list(
        eta = eta,
        maxDepth = maxDepth,
        minChildWeight = minChildWeight,
        subsample = subsample,
        colsampleBynode = colsampleBynode,
        gamma = gamma,
        maxBin = maxBin
      )

      parameterData <- as.data.frame(hyperparameters, stringsAsFactors = FALSE)
      normalizedData <- TuneXgboostNormalizeParameterData(parameterData, parameterNames)
      cacheKey <- paste(unlist(normalizedData[1L, parameterNames, drop = FALSE], use.names = FALSE), collapse = "|")

      if(exists(cacheKey, envir = evaluationEnvironment$cache, inherits = FALSE)){
        cachedResult <- get(cacheKey, envir = evaluationEnvironment$cache, inherits = FALSE)
        return(list(Score = as.numeric(cachedResult$score), Pred = 0))
      }

      cvSummary <- TuneXgboostRunCvManual(
        balancedFolds = balancedFolds,
        formula = formula,
        hyperparameters = hyperparameters,
        nRounds = nRounds,
        earlyStoppingRounds = earlyStoppingRounds,
        seed = seed,
        nThreads = nThreads,
        evalMetric = evalMetric
      )

      scoreValue <- as.numeric(cvSummary$score)
      bestIteration <- as.integer(cvSummary$bestIteration)
      logRow <- data.frame(
        eta = as.numeric(normalizedData$eta),
        maxDepth = as.numeric(normalizedData$maxDepth),
        minChildWeight = as.numeric(normalizedData$minChildWeight),
        subsample = as.numeric(normalizedData$subsample),
        colsampleBynode = as.numeric(normalizedData$colsampleBynode),
        gamma = as.numeric(normalizedData$gamma),
        maxBin = as.numeric(normalizedData$maxBin),
        Value = scoreValue,
        bestIteration = bestIteration,
        stringsAsFactors = FALSE
      )
      appendLog(logRow)
      assign(cacheKey, list(score = scoreValue, bestIteration = bestIteration), envir = evaluationEnvironment$cache)
      list(Score = scoreValue, Pred = 0)
    }
  } else {
    preparedTrain <- TuneXgboostPrepareSparseMatrix(formula = formula, data = dataTrain, featureTypes = featureTypes)
    if(is.null(scalePosWeight)){
      classCounts <- table(preparedTrain$yData)
      if(length(classCounts) != 2L || any(classCounts == 0L)){
        stop("dataTrain deve conter ambas as classes", call. = FALSE)
      }
      scalePosWeight <- as.numeric(classCounts[[1L]] / classCounts[[2L]])
    }
    scalePosWeight <- TuneXgboostValidateScalePosWeight(scalePosWeight, context = "scalePosWeight calculado no tuning")

    dtrain <- TuneXgboostCreateDMatrix(
      xMatrix = preparedTrain$xMatrix,
      yData = preparedTrain$yData,
      featureTypes = preparedTrain$featureTypes,
      nThreads = nThreads
    )
    folds <- TuneXgboostCreateStratifiedFolds(yData = preparedTrain$yData, nFolds = nFolds, seed = seed)

    TuneXgboostEvaluateCv <- function(eta, maxDepth, minChildWeight, subsample, colsampleBynode, gamma, maxBin, nThreads){
      hyperparameters <- list(
        eta = eta,
        maxDepth = maxDepth,
        minChildWeight = minChildWeight,
        subsample = subsample,
        colsampleBynode = colsampleBynode,
        gamma = gamma,
        maxBin = maxBin
      )

      parameterData <- as.data.frame(hyperparameters, stringsAsFactors = FALSE)
      normalizedData <- TuneXgboostNormalizeParameterData(parameterData, parameterNames)
      cacheKey <- paste(unlist(normalizedData[1L, parameterNames, drop = FALSE], use.names = FALSE), collapse = "|")

      if(exists(cacheKey, envir = evaluationEnvironment$cache, inherits = FALSE)){
        cachedResult <- get(cacheKey, envir = evaluationEnvironment$cache, inherits = FALSE)
        return(list(Score = as.numeric(cachedResult$score), Pred = 0))
      }

      cvSummary <- TuneXgboostRunCv(
        dtrain = dtrain,
        folds = folds,
        hyperparameters = hyperparameters,
        nRounds = nRounds,
        earlyStoppingRounds = earlyStoppingRounds,
        seed = seed,
        nThreads = nThreads,
        scalePosWeight = scalePosWeight,
        evalMetric = evalMetric,
        verbose = FALSE
      )

      scoreValue <- as.numeric(cvSummary$score)
      bestIteration <- as.integer(cvSummary$bestIteration)
      logRow <- data.frame(
        eta = as.numeric(normalizedData$eta),
        maxDepth = as.numeric(normalizedData$maxDepth),
        minChildWeight = as.numeric(normalizedData$minChildWeight),
        subsample = as.numeric(normalizedData$subsample),
        colsampleBynode = as.numeric(normalizedData$colsampleBynode),
        gamma = as.numeric(normalizedData$gamma),
        maxBin = as.numeric(normalizedData$maxBin),
        Value = scoreValue,
        bestIteration = bestIteration,
        stringsAsFactors = FALSE
      )
      appendLog(logRow)
      assign(cacheKey, list(score = scoreValue, bestIteration = bestIteration), envir = evaluationEnvironment$cache)
      list(Score = scoreValue, Pred = 0)
    }
  }

  set.seed(seed)
  tuningResult <- rBayesianOptimization::BayesianOptimization(
    FUN = TuneXgboostEvaluateCv,
    bounds = bounds,
    init_grid_dt = initGridDt,
    init_points = as.integer(initPoints),
    n_iter = as.integer(nIter),
    acq = acq,
    kappa = kappa,
    eps = eps,
    verbose = verbose
  )

  bestHyperparameters <- as.list(tuningResult$Best_Par)
  bestScore <- as.numeric(tuningResult$Best_Value)
  evaluationLog <- if(length(evaluationEnvironment$evaluationLogList) > 0L) {
    data.table::rbindlist(evaluationEnvironment$evaluationLogList, fill = TRUE)
  } else {
    data.frame()
  }

  bestIteration <- TuneXgboostFindBestIterationInLog(
    evaluationLog = evaluationLog,
    bestHyperparameters = bestHyperparameters,
    bestScore = bestScore,
    bounds = bounds
  )

  if(is.null(bestIteration)){
    if(useBalancedCv){
      bestCvSummary <- TuneXgboostRunCvManual(
        balancedFolds = balancedFolds,
        formula = formula,
        hyperparameters = bestHyperparameters,
        nRounds = nRounds,
        earlyStoppingRounds = earlyStoppingRounds,
        seed = seed,
        nThreads = nThreads,
        evalMetric = evalMetric
      )
    } else {
      bestCvSummary <- TuneXgboostRunCv(
        dtrain = dtrain,
        folds = folds,
        hyperparameters = bestHyperparameters,
        nRounds = nRounds,
        earlyStoppingRounds = earlyStoppingRounds,
        seed = seed,
        nThreads = nThreads,
        scalePosWeight = scalePosWeight,
        evalMetric = evalMetric,
        verbose = FALSE
      )
    }
    bestIteration <- as.integer(bestCvSummary$bestIteration)
  }

  bestHyperparameters$nRounds <- bestIteration
  bestHyperparameters$evalMetric <- evalMetric
  if(!useBalancedCv){
    bestHyperparameters$scalePosWeight <- as.numeric(scalePosWeight)
  }

  newInitGridDt <- TuneXgboostCreateInitGrid(historyData = evaluationLog, bounds = bounds)
  returnedInitGridDt <- TuneXgboostCombineInitGrid(initGridDt = initGridDt, newInitGridDt = newInitGridDt, bounds = bounds)

  list(
    bestHyperparameters = bestHyperparameters,
    bestScore = bestScore,
    initGridDt = returnedInitGridDt,
    evaluationLog = evaluationLog
  )
}
