#' Resolver controle de execução
#' @noRd

TuneBoostTree_ResolveControl <- function(control) {

  if(is.null(control)) {
    control <- TuneBoostTreeControl()
  }
  if(!is.list(control)) {
    cli::cli_abort("`control` must be created by `TuneBoostTreeControl()` or be a compatible list.")
  }
  defaults <- TuneBoostTreeControl()
  defaults[names(control)] <- control
  TuneBoostTreeControl(
    seed = defaults$seed,
    parallel = defaults$parallel,
    verbose = defaults$verbose,
    fallback_trees = defaults$fallback_trees
  )
}
####
## Fim
#

