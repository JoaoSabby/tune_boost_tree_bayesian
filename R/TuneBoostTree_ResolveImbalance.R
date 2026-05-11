#' Resolver configuração de desbalanceamento
#' @noRd

TuneBoostTree_ResolveImbalance <- function(imbalance) {

  if(is.null(imbalance)) {
    imbalance <- TuneBoostTreeImbalance()
  }
  if(!is.list(imbalance)) {
    cli::cli_abort("`imbalance` must be created by `TuneBoostTreeImbalance()` or be a compatible list.")
  }
  if(is.null(imbalance$balance_args)) {
    args <- list()
  } else {
    args <- imbalance$balance_args
  }
  do.call(
    TuneBoostTreeImbalance,
    c(
      list(
        balanceFn = imbalance$balanceFn,
        scale_pos_weight = imbalance$scale_pos_weight
      ),
      args
    )
  )
}
####
## Fim
#

