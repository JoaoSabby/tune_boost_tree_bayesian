#' Ajustar modelo boosted tree final
#'
#' @param formula Fórmula de duas faces.
#' @param dataTrain data.frame de treino.
#' @param hyperparameters Lista de hiperparâmetros escolhidos pela tunagem.
#' @param featureTypes Vetor opcional de tipos de features do XGBoost.
#' @param targetLevels Ordenação opcional de dois níveis da variável resposta.
#' @param scalePosWeight Peso opcional da classe positiva; é calculado quando
#'   `NULL`.
#' @param nThreads Inteiro com número de threads da engine.
#' @param seed Inteiro usado como semente aleatória.
#' @param verbose Verbosidade da engine.
#' @param engine Nome da engine, `"xgboost"` ou `"lightgbm"`.
#'
#' @details Ajusta o modelo final com hiperparâmetros canônicos e isola a
#'   tradução de parâmetros no limite da engine.
#'
#' @return Lista nomeada com o modelo nativo em `model`, parâmetros efetivamente
#'   usados em `params`, nomes/tipos de features, níveis e nomes das classes,
#'   metadados da fórmula, número de rodadas (`nRounds`), `threshold` e `engine`.
#' @export

FitBoostTreeModel <- function(
  formula,
  dataTrain,
  hyperparameters,
  featureTypes = NULL,
  targetLevels = NULL,
  scalePosWeight = NULL,
  nThreads = 8L,
  seed = 42L,
  verbose = 0L,
  engine = "lightgbm"
) {

  if(!(engine %in% c("xgboost", "lightgbm"))) {
    cli::cli_abort("`engine` must be 'xgboost' or 'lightgbm'.")
  }

  preparedTrain <- TuneBoostTree_PrepareMatrix(
    formula = formula,
    data = dataTrain,
    featureTypes = featureTypes,
    targetLevels = targetLevels,
    formulaInfo = NULL
  )

  classCounts <- table(preparedTrain$yData)

  if(length(classCounts) != 2L || any(classCounts == 0L)) {
    cli::cli_abort("`dataTrain` must contain both binary classes.")
  }

  if(is.null(scalePosWeight)) {
    scalePosWeight <- as.numeric(classCounts[["0"]] / classCounts[["1"]])
  } else {
    scalePosWeight <- as.numeric(scalePosWeight)
  }

  if(is.null(hyperparameters$eval_metric)) {
    if(engine == "xgboost") {
      evalMetric <- "aucpr"
    } else {
      evalMetric <- "average_precision"
    }
  } else {
    evalMetric <- as.character(hyperparameters$eval_metric)
  }

  if(!is.null(hyperparameters$scale_pos_weight)) {
    scalePosWeight <- hyperparameters$scale_pos_weight
  }

  paramsValue <- TuneBoostTree_BuildParams(
    hyperparameters = hyperparameters,
    nThreads = nThreads,
    scalePosWeight = scalePosWeight,
    seed = seed,
    evalMetric = evalMetric,
    engine = engine
  )

  if(is.null(hyperparameters$trees)) {
    nRounds <- 100L
  } else {
    nRounds <- as.integer(hyperparameters$trees)
  }

  trainObject <- TuneBoostTree_CreateDataObject(
    xMatrix = preparedTrain$xMatrix,
    yData = preparedTrain$yData,
    featureTypes = preparedTrain$featureTypes,
    nThreads = nThreads,
    engine = engine
  )

  if(engine == "xgboost") {
    model <- xgboost::xgb.train(
      params = paramsValue,
      data = trainObject,
      nrounds = nRounds,
      verbose = as.integer(verbose)
    )
  } else {
    trainArgs <- list(
      params = paramsValue,
      data = trainObject,
      nrounds = nRounds,
      verbose = as.integer(verbose)
    )
    model <- tryCatch(
      do.call(lightgbm::lgb.train, trainArgs),
      error = function(e) e
    )
    if(inherits(model, "error") && paramsValue$metric %in% c("average_precision", "aucpr")) {
      paramsValue$metric <- "auc"
      trainArgs$params <- paramsValue
      model <- do.call(lightgbm::lgb.train, trainArgs)
    }
    if(inherits(model, "error")) {
      stop(model)
    }
  }

  if(is.null(hyperparameters$threshold)) {
    threshold <- 0.5
  } else {
    threshold <- as.numeric(hyperparameters$threshold)[1L]
  }

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
    threshold = threshold,
    engine = engine
  )
}
####
## Fim
#

