#' Resolver configuração do otimizador
#' @noRd

TuneBoostTree_ResolveOptimizer <- function(optimizer) {

  if(is.null(optimizer)) {
    optimizer <- TuneBoostTreeOptimizerRBayesianOptimization()
  }
  if(is.character(optimizer)) {
    optimizerName <- as.character(optimizer[1L])
    if(identical(optimizerName, "internal")) {
      optimizer <- TuneBoostTreeInternalOptimizer()
    } else if(identical(optimizerName, "rBayesianOptimization")) {
      optimizer <- TuneBoostTreeOptimizerRBayesianOptimization()
    } else if(identical(optimizerName, "limbo")) {
      optimizer <- TuneBoostTreeOptimizerLimbo()
    } else {
      cli::cli_abort("`optimizer` as character must be one of 'internal', 'rBayesianOptimization', or 'limbo'.")
    }
  }
  if(!is.list(optimizer) || is.null(optimizer$type)) {
    cli::cli_abort(paste0(
      "`optimizer` must be created by `TuneBoostTreeOptimizerLimbo()`, ",
      "`TuneBoostTreeOptimizerRBayesianOptimization()`, or `TuneBoostTreeInternalOptimizer()`."
    ))
  }
  if(!(optimizer$type %in% c("limbo", "internal", "rBayesianOptimization"))) {
    cli::cli_abort("Unsupported optimizer type: {optimizer$type}")
  }
  optimizer
}
####
## Fim
#

