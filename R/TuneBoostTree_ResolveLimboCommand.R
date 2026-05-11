#' Resolver localização do comando Limbo
#' @noRd

TuneBoostTree_ResolveLimboCommand <- function(command = NULL) {

  if(!is.null(command)) {
    command <- as.character(command)
    if(length(command) != 1L) {
      cli::cli_abort("`command` must be `NULL` or a single executable path/command.")
    }
    if(is.na(command) || !nzchar(command)) {
      return(NA_character_)
    }

    return(command)
  }
  envCommand <- Sys.getenv("TBTB_LIMBO_COMMAND", unset = NA_character_)
  if(!is.na(envCommand) && nzchar(envCommand)) {
    return(envCommand)
  }
  if(.Platform$OS.type == "windows") {
    executableName <- "tbtb-limbo-ask.exe"
  } else {
    executableName <- "tbtb-limbo-ask"
  }
  pkgCommand <- system.file("bin", executableName, package = "TuneBoostTreeBayesian")
  if(nzchar(pkgCommand)) {
    return(pkgCommand)
  }
  NA_character_
}
####
## Fim
#

