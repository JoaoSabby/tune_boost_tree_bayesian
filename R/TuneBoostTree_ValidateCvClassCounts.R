#' Validar contagens de classes da validação cruzada
#' @noRd

TuneBoostTree_ValidateCvClassCounts <- function(yData, nFolds) {

  classCounts <- table(as.integer(yData))
  if(length(classCounts) != 2L || any(classCounts == 0L)) {
    cli::cli_abort("`data` must contain both binary classes.")
  }
  minClassCount <- min(as.integer(classCounts))
  if(minClassCount < 2L) {
    cli::cli_abort("The minority class must contain at least two observations for cross-validation.")
  }
  if(minClassCount < as.integer(nFolds)) {
    cli::cli_warn(paste0(
      "Minority class has {.val {minClassCount}} observation(s), fewer than requested ",
      "{.val {nFolds}} fold(s); using {.val {minClassCount}} fold(s) so every ",
      "validation fold contains both classes."
    ))
    return(as.integer(minClassCount))
  }
  as.integer(nFolds)
}
####
## Fim
#

