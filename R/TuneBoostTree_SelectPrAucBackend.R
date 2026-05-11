#' Selecionar backend de PR-AUC
#'
#' @param backend Nome do backend solicitado.
#'
#' @details `auto` prefere C compilado, depois Fortran compilado, depois Rfast
#'   e por fim R base. Solicitações explícitas indisponíveis retornam para R
#'   base para não abortar uma tunagem longa.
#'
#' @return Nome do backend resolvido.
#' @noRd

TuneBoostTree_SelectPrAucBackend <- function(backend = "auto") {

  backend <- match.arg(
    as.character(backend)[1L],
    c("auto", "c", "fortran", "rfast", "r")
  )
  if(identical(backend, "auto")) {
    if(TuneBoostTree_LoadNativeBackend("c")) {
      return("c")
    }
    if(TuneBoostTree_LoadNativeBackend("fortran")) {
      return("fortran")
    }
    if(requireNamespace("Rfast", quietly = TRUE)) {
      return("rfast")
    }
    return("r")
  }
  if(identical(backend, "c") && !TuneBoostTree_LoadNativeBackend("c")) {
    return("r")
  }
  if(identical(backend, "fortran") && !TuneBoostTree_LoadNativeBackend("fortran")) {
    return("r")
  }
  if(identical(backend, "rfast") && !requireNamespace("Rfast", quietly = TRUE)) {
    return("r")
  }
  backend
}
####
## Fim
#

