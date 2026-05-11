#' Criar objeto de dados da engine
#'
#' @param xMatrix Matriz numérica ou `dgCMatrix` esparsa de preditores.
#' @param yData Vetor numérico opcional do alvo.
#' @param featureTypes Vetor opcional de tipos de features do XGBoost.
#' @param nThreads Inteiro com threads da engine para construção dos dados.
#' @param engine Nome da engine, `"xgboost"` ou `"lightgbm"`.
#'
#' @details Esta é a única função interna que constrói objetos de dados nativos das engines.
#'
#' @return Um `xgb.DMatrix` para XGBoost ou `lgb.Dataset` para LightGBM.
#' @noRd

TuneBoostTree_CreateDataObject <- function(
  xMatrix,
  yData = NULL,
  featureTypes = NULL,
  nThreads = 1L,
  engine = "xgboost"
) {

  if(engine == "xgboost") {
    args <- list(
      data = xMatrix,
      nthread = as.integer(nThreads)
    )

    if(!is.null(yData)) {
      args$label <- yData
    }

    if(!is.null(featureTypes)) {
      args$feature_types <- unname(featureTypes)
    }

    return(do.call(xgboost::xgb.DMatrix, args))
  }

  lightgbm::lgb.Dataset(data = xMatrix, label = yData)
}
####
## Fim
#

