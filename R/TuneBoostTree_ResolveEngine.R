#' Resolver configuração da engine
#' @noRd

TuneBoostTree_ResolveEngine <- function(engine) {

  if(is.character(engine)) {
    engineName <- match.arg(as.character(engine)[1L], c("xgboost", "lightgbm"))
    if(engineName == "xgboost") {
      return(TuneBoostTreeXgboost())
    }

    return(TuneBoostTreeLightgbm())
  }
  if(!is.list(engine) || is.null(engine$name) || !(engine$name %in% c("xgboost", "lightgbm"))) {
    cli::cli_abort("`engine` must be 'xgboost', 'lightgbm', or a TuneBoostTree engine configuration.")
  }
  engine
}
####
## Fim
#

