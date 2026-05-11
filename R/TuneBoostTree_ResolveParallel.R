#' Resolver execução paralela
#' @noRd

TuneBoostTree_ResolveParallel <- function(parallel, nRows, nFolds) {

  totalCores <- TuneBoostTree_DetectCpuBudget()
  if(isFALSE(parallel) || identical(parallel, "sequential")) {
    return(
      list(
        workers = 1L,
        threads_per_worker = totalCores
      )
    )
  }
  if(is.character(parallel) && identical(parallel[1L], "auto")) {
    if(nRows < 1000L) {
      workers <- 1L
    } else {
      workers <- min(as.integer(nFolds), max(1L, floor(totalCores / 2L)))
    }

    threads <- max(1L, floor(totalCores / workers))

    return(TuneBoostTree_FinalizeParallel(workers, threads, nFolds, totalCores))
  }
  if(is.list(parallel)) {
    if(is.null(parallel$strategy)) {
      strategy <- "auto"
    } else {
      strategy <- as.character(parallel$strategy)[1L]
    }
    if(strategy == "sequential") {
      return(TuneBoostTree_FinalizeParallel(1L, totalCores, nFolds, totalCores))
    }
    if(strategy == "engine") {
      return(TuneBoostTree_FinalizeParallel(1L, totalCores, nFolds, totalCores))
    }
    if(identical(parallel$workers, "auto")) {
      workers <- min(as.integer(nFolds), max(1L, floor(totalCores / 2L)))
    } else {
      workers <- as.integer(parallel$workers)
    }

    if(length(workers) != 1L || is.na(workers) || workers < 1L) {
      cli::cli_abort("Parallel `workers` must be positive or 'auto'.")
    }

    if(identical(parallel$threads_per_worker, "auto")) {
      threads <- max(1L, floor(totalCores / workers))
    } else {
      threads <- as.integer(parallel$threads_per_worker)
    }

    if(length(threads) != 1L || is.na(threads) || threads < 1L) {
      cli::cli_abort("Parallel `threads_per_worker` must be positive or 'auto'.")
    }
    return(TuneBoostTree_FinalizeParallel(workers, threads, nFolds, totalCores))
  }
  cli::cli_abort("`parallel` must be 'auto', FALSE, 'sequential', or `TuneBoostTreeParallel()`.")
}
####
## Fim
#

