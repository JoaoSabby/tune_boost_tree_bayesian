#' Executar um fold de predição
#' @noRd

TuneBoostTree_RunOneFoldPrediction <- function(
  foldData,
  hyperparameters,
  nRounds,
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

  testObject <- TuneBoostTree_CreateDataObject(
    xMatrix = foldData$xTest,
    yData = foldData$yTest,
    featureTypes = foldData$featureTypes,
    nThreads = nThreads,
    engine = engine
  )

  if(engine == "xgboost") {
    foldModel <- xgboost::xgb.train(
      params = paramsValue,
      data = trainObject,
      nrounds = as.integer(nRounds),
      verbose = 0L
    )

    predictedProbability <- as.numeric(stats::predict(foldModel, newdata = testObject))
  } else {
    foldModel <- lightgbm::lgb.train(
      params = paramsValue,
      data = trainObject,
      nrounds = as.integer(nRounds),
      verbose = -1L
    )

    predictedProbability <- as.numeric(
      stats::predict(foldModel, data = TuneBoostTree_AsPredictionMatrix(foldData$xTest))
    )
  }

  list(actual = foldData$yTest, predicted = predictedProbability)
}
####
## Fim
#

