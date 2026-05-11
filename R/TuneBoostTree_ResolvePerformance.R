#' Resolver configuração de performance
#' @noRd

TuneBoostTree_ResolvePerformance <- function(performance) {

  if(is.null(performance)) {
    performance <- TuneBoostTreePerformance()
  }
  if(!is.list(performance)) {
    cli::cli_abort("`performance` must be created by `TuneBoostTreePerformance()` or be a compatible list.")
  }
  defaults <- TuneBoostTreePerformance()
  defaults[names(performance)] <- performance
  TuneBoostTreePerformance(metric = defaults$metric, backend = defaults$backend)
}
####
## Fim
#

