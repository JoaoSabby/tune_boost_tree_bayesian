#' Carregar backend nativo opcional
#'
#' @param backend `"c"` ou `"fortran"`.
#'
#' @details Verifica se a DLL instalada do pacote, contendo as rotinas C e Fortran registradas, está carregada.
#'
#' @return Lógico indicando se o símbolo está disponível.
#' @noRd

TuneBoostTree_LoadNativeBackend <- function(backend) {

  packageDll <- "TuneBoostTreeBayesian"
  packageDll %in% names(getLoadedDLLs())
}
####
## Fim
#

