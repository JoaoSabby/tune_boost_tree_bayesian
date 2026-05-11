#' Executar otimizador de hiperparâmetros
#' @noRd

TuneBoostTree_RunOptimizer <- function(
  objective,
  bounds,
  initGridDt = NULL,
  initPoints = 10L,
  nIter = 30L,
  acq = "ucb",
  kappa = 2.576,
  eps = 0,
  verbose = TRUE,
  seed = 42L,
  optimizerBackend = "internal",
  limboCommand = NA_character_,
  limboFallback = TRUE
) {

  if(identical(optimizerBackend, "limbo")) {
    if(TuneBoostTree_IsExecutableCommand(limboCommand)) {
      limboResult <- tryCatch(
        TuneBoostTree_RunLimboOptimizer(
          objective,
          bounds,
          initGridDt,
          initPoints,
          nIter,
          acq,
          kappa,
          eps,
          seed,
          limboCommand
        ),
        error = function(e) e
      )
      if(!inherits(limboResult, "error")) {
        return(limboResult)
      }
      if(!isTRUE(limboFallback)) {
        cli::cli_abort("Limbo optimizer failed and `fallback = FALSE`: {conditionMessage(limboResult)}")
      }
      cli::cli_warn(paste0(
        "Limbo optimizer failed; using the package-native Bayesian optimizer fallback. ",
        "Cause: {conditionMessage(limboResult)}"
      ))
      return(
        TuneBoostTree_RunInternalOptimizer(
          objective,
          bounds,
          initGridDt,
          initPoints,
          nIter,
          seed,
          acq,
          kappa,
          eps
        )
      )
    }
    if(!isTRUE(limboFallback)) {
      cli::cli_abort("Limbo optimizer command is not available or executable and `fallback = FALSE`.")
    }
    if(!is.na(limboCommand) && nzchar(limboCommand)) {
      cli::cli_warn("Limbo optimizer command is not available; using the package-native Bayesian optimizer fallback.")
    }
    return(
      TuneBoostTree_RunInternalOptimizer(
        objective,
        bounds,
        initGridDt,
        initPoints,
        nIter,
        seed,
        acq,
        kappa,
        eps
      )
    )
  }
  if(identical(optimizerBackend, "rBayesianOptimization") && requireNamespace("rBayesianOptimization", quietly = TRUE)) {
    return(
      TuneBoostTree_RunRBayesianOptimization(
        objective,
        bounds,
        initGridDt,
        initPoints,
        nIter,
        acq,
        kappa,
        eps,
        verbose,
        seed
      )
    )
  }
  if(identical(optimizerBackend, "rBayesianOptimization")) {
    if(!isTRUE(limboFallback)) {
      cli::cli_abort("Package {.pkg rBayesianOptimization} is not available and `fallback = FALSE`.")
    }
    cli::cli_warn(paste0(
      "Package {.pkg rBayesianOptimization} is not available; ",
      "using the package-native Bayesian optimizer fallback."
    ))
  }
  TuneBoostTree_RunInternalOptimizer(objective, bounds, initGridDt, initPoints, nIter, seed, acq, kappa, eps)
}
####
## Fim
#

