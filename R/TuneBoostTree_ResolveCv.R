#' Resolver configuração de validação cruzada
#' @noRd

TuneBoostTree_ResolveCv <- function(cv) {

  if(is.null(cv)) {
    cv <- TuneBoostTreeCv()
  }
  if(!is.list(cv)) {
    cli::cli_abort("`cv` must be created by `TuneBoostTreeCv()` or be a compatible list.")
  }
  defaults <- TuneBoostTreeCv()
  defaults[names(cv)] <- cv
  TuneBoostTreeCv(folds = defaults$folds, stratified = defaults$stratified)
}
####
## Fim
#

