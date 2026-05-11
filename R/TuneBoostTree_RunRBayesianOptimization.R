#' Executar backend rBayesianOptimization
#' @noRd

TuneBoostTree_RunRBayesianOptimization <- function(
  objective,
  bounds,
  initGridDt = NULL,
  initPoints = 10L,
  nIter = 30L,
  acq = "ucb",
  kappa = 2.576,
  eps = 0,
  verbose = TRUE,
  seed = 42L
) {

  set.seed(as.integer(seed))
  normalizedBounds <- lapply(bounds, function(x) c(as.numeric(x[1L]), as.numeric(x[2L])))
  parameterNames <- names(bounds)
  if(!is.null(initGridDt)) {
    initGridDt <- as.data.frame(initGridDt, stringsAsFactors = FALSE)
    if("Value" %in% names(initGridDt)) {
      initGridDt <- initGridDt[is.finite(as.numeric(initGridDt$Value)), c(parameterNames, "Value"), drop = FALSE]
    } else {
      initGridDt <- NULL
    }
  }
  result <- rBayesianOptimization::BayesianOptimization(
    FUN = objective,
    bounds = normalizedBounds,
    init_grid_dt = initGridDt,
    init_points = as.integer(initPoints),
    n_iter = as.integer(nIter),
    acq = acq,
    kappa = kappa,
    eps = eps,
    verbose = isTRUE(verbose)
  )
  list(
    Best_Par = as.list(result$Best_Par),
    Best_Value = as.numeric(result$Best_Value),
    History = result$History
  )
}
####
## Fim
#

