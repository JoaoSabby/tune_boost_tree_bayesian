#' Configurar espera passiva para OpenMP quando o usuário não definiu política
#' @noRd

TuneBoostTree_SetPassiveOpenMp <- function() {

  if(!nzchar(Sys.getenv("OMP_WAIT_POLICY", unset = ""))) {
    Sys.setenv(OMP_WAIT_POLICY = "passive")
  }
  if(!nzchar(Sys.getenv("GOMP_SPINCOUNT", unset = ""))) {
    Sys.setenv(GOMP_SPINCOUNT = "0")
  }
  invisible(TRUE)
}
####
## Fim
#

