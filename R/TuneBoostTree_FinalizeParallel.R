#' Finalizar configuração paralela e avisar sobre oversubscription manual
#' @noRd

TuneBoostTree_FinalizeParallel <- function(workers, threads, nFolds, totalCores) {

  workers <- min(as.integer(workers), as.integer(nFolds))
  threads <- as.integer(threads)
  totalCores <- max(1L, as.integer(totalCores))
  requestedThreads <- as.numeric(workers) * as.numeric(threads)
  if(is.finite(requestedThreads) && requestedThreads > as.numeric(totalCores)) {
    adjustedThreads <- max(1L, as.integer(floor(as.numeric(totalCores) / as.numeric(workers))))
    cli::cli_warn(paste0(
      "workers ({workers}) * threads_per_worker ({threads}) = {requestedThreads} ",
      "exceeds the detected CPU budget ({totalCores}); using threads_per_worker = ",
      "{adjustedThreads} to avoid oversubscription."
    ))
    threads <- adjustedThreads
  }
  list(workers = as.integer(workers), threads_per_worker = threads)
}
####
## Fim
#

