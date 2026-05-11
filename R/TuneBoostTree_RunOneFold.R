#' Executar um fold em cache
#'
#' @param foldData Fold com matrizes R serializáveis.
#' @param hyperparameters Named canonical hyperparameter list.
#' @param nRounds Inteiro com limite de rodadas da tunagem.
#' @param earlyStoppingRounds Inteiro com paciência da parada antecipada.
#' @param seed Inteiro usado como semente aleatória.
#' @param nThreads Inteiro com threads deste worker de fold.
#' @param evalMetric Nome da métrica do XGBoost.
#' @param engine Nome da engine.
#' @param prAucBackend Resolved PR-AUC backend used inside fold scoring.
#'
#' @details Cria os objetos nativos da engine dentro do worker antes de treinar e predizer.
#'
#' @return Lista com score do fold e melhor iteração.
#' @noRd

TuneBoostTree_RunOneFold <- function(
  foldData,
  hyperparameters,
  nRounds,
  earlyStoppingRounds,
  seed,
  nThreads,
  evalMetric,
  engine,
  prAucBackend = "auto"
) {

  paramsValue <- TuneBoostTree_BuildParams(
    hyperparameters = hyperparameters,
    nThreads = nThreads,
    scalePosWeight = foldData$scalePosWeight,
    seed = seed,
    evalMetric = evalMetric,
    engine = engine
  )

  trainObject <- TuneBoostTree_CreateDataObject(
    xMatrix = foldData$xTrain,
    yData = foldData$yTrain,
    featureTypes = foldData$featureTypes,
    nThreads = nThreads,
    engine = engine
  )

  if(engine == "xgboost") {
    testObject <- TuneBoostTree_CreateDataObject(
      xMatrix = foldData$xTest,
      yData = foldData$yTest,
      featureTypes = foldData$featureTypes,
      nThreads = nThreads,
      engine = engine
    )

    foldModel <- xgboost::xgb.train(
      params = paramsValue,
      data = trainObject,
      nrounds = as.integer(nRounds),
      watchlist = list(train = trainObject, eval = testObject),
      early_stopping_rounds = as.integer(earlyStoppingRounds),
      maximize = TRUE,
      verbose = 0L
    )

    bestIterFold <- as.integer(foldModel$best_iteration)

    if(is.null(bestIterFold) || is.na(bestIterFold) || bestIterFold < 1L) {
      bestIterFold <- as.integer(nRounds)
    }

    predictedProbability <- as.numeric(stats::predict(foldModel, newdata = testObject))
  } else {
    paramsValue$early_stopping_round <- as.integer(earlyStoppingRounds)
    testObject <- lightgbm::lgb.Dataset.create.valid(
      dataset = trainObject,
      data = foldData$xTest,
      label = foldData$yTest
    )
    trainArgs <- list(
      params = paramsValue,
      data = trainObject,
      nrounds = as.integer(nRounds),
      valids = list(eval = testObject),
      verbose = -1L
    )
    foldModel <- tryCatch(
      do.call(lightgbm::lgb.train, trainArgs),
      error = function(e) e
    )
    if(inherits(foldModel, "error") && paramsValue$metric %in% c("average_precision", "aucpr")) {
      paramsValue$metric <- "auc"
      trainArgs$params <- paramsValue
      foldModel <- do.call(lightgbm::lgb.train, trainArgs)
    }
    if(inherits(foldModel, "error")) {
      stop(foldModel)
    }

    bestIterFold <- as.integer(foldModel$best_iter)

    if(is.null(bestIterFold) || is.na(bestIterFold) || bestIterFold < 1L) {
      bestIterFold <- as.integer(nRounds)
    }

    predictedProbability <- as.numeric(
      stats::predict(foldModel, data = TuneBoostTree_AsPredictionMatrix(foldData$xTest))
    )
  }

  list(
    score = TuneBoostTree_CalculatePrAuc(foldData$yTest, predictedProbability, backend = prAucBackend),
    bestIteration = bestIterFold
  )
}
####
## Fim
#


