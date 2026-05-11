#' Resolver configuração de boosting
#' @noRd

TuneBoostTree_ResolveBoost <- function(boost) {

  if(is.null(boost)) {
    boost <- TuneBoostTreeBoostParams()
  }
  if(!is.list(boost)) {
    cli::cli_abort("`boost` must be created by `TuneBoostTreeBoostParams()` or be a compatible list.")
  }
  defaults <- TuneBoostTreeBoostParams()
  defaults[names(boost)] <- boost
  TuneBoostTreeBoostParams(
    trees = defaults$trees,
    stop_iter = defaults$stop_iter,
    learn_rate = defaults$learn_rate,
    tree_depth = defaults$tree_depth,
    min_n = defaults$min_n,
    loss_reduction = defaults$loss_reduction,
    sample_size = defaults$sample_size,
    mtry = defaults$mtry,
    max_bin = defaults$max_bin
  )
}
####
## Fim
#

