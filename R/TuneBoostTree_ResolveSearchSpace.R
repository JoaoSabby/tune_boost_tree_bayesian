#' Resolver espaço de busca
#' @noRd

TuneBoostTree_ResolveSearchSpace <- function(searchSpace, boost) {

  if(is.null(searchSpace)) {
    searchSpace <- TuneBoostTreeSearchSpace()
  }
  if(!is.list(searchSpace)) {
    cli::cli_abort("`searchSpace` must be created by `TuneBoostTreeSearchSpace()` or be a compatible list.")
  }
  defaults <- TuneBoostTreeSearchSpace()
  defaults[names(searchSpace)] <- searchSpace
  bounds <- do.call(TuneBoostTreeSearchSpace, defaults)
  candidateFixedNames <- intersect(names(boost), names(bounds))
  fixedNames <- candidateFixedNames[
    !vapply(boost[candidateFixedNames], is.null, logical(1L))
  ]
  for(parameterName in fixedNames) {
    if(identical(boost[[parameterName]], "default")) {
      next
    }
    value <- as.numeric(boost[[parameterName]])[1L]
    if(!is.finite(value)) {
      cli::cli_abort("Fixed boost parameter `{parameterName}` must be finite.")
    }
    bounds[[parameterName]] <- c(value, value)
  }
  bounds
}
####
## Fim
#

