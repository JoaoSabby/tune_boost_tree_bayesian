#' Verificar comando executável
#' @noRd

TuneBoostTree_IsExecutableCommand <- function(command) {

  command <- as.character(command)[1L]
  if(is.na(command) || !nzchar(command)) {
    return(FALSE)
  }
  command <- path.expand(command)
  if(grepl(.Platform$file.sep, command, fixed = TRUE) || grepl("/", command, fixed = TRUE)) {
    return(file.exists(command) && file.access(command, mode = 1L) == 0L)
  }
  nzchar(Sys.which(command))
}
####
## Fim
#

