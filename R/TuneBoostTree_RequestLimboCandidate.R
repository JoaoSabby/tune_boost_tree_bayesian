#' Solicitar candidato a executável Limbo ask/tell externo
#' @noRd

TuneBoostTree_RequestLimboCandidate <- function(
  limboCommand,
  bounds,
  history,
  acq = "ucb",
  kappa = 2.576,
  eps = 0,
  seed = 42L,
  iteration = 1L
) {

  workDir <- tempfile("tbtb_limbo_")
  dir.create(workDir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(workDir, recursive = TRUE, force = TRUE), add = TRUE)
  boundsFile <- file.path(workDir, "bounds.csv")
  observationsFile <- file.path(workDir, "observations.csv")
  configFile <- file.path(workDir, "config.csv")
  candidateFile <- file.path(workDir, "candidate.csv")
  boundsData <- data.frame(
    parameter = names(bounds),
    lower = vapply(bounds, `[[`, numeric(1L), 1L),
    upper = vapply(bounds, `[[`, numeric(1L), 2L),
    type = ifelse(
      names(bounds) %in% c("tree_depth", "min_n", "max_bin", "num_leaves", "min_data_in_leaf"),
      "integer",
      "double"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(boundsData, boundsFile, row.names = FALSE)
  utils::write.csv(history, observationsFile, row.names = FALSE)
  configData <- data.frame(
    acq = as.character(acq)[1L],
    kappa = as.numeric(kappa)[1L],
    eps = as.numeric(eps)[1L],
    seed = as.integer(seed),
    iteration = as.integer(iteration)
  )

  utils::write.csv(configData, configFile, row.names = FALSE)
  status <- suppressWarnings(
    system2(
      limboCommand,
      args = c(boundsFile, observationsFile, configFile, candidateFile),
      stdout = TRUE,
      stderr = TRUE,
      timeout = as.integer(Sys.getenv("TBTB_LIMBO_TIMEOUT", "600"))
    )
  )
  exitStatus <- attr(status, "status")
  if(!is.null(exitStatus) && !identical(as.integer(exitStatus), 0L)) {
    cli::cli_abort("Limbo command failed with exit status {exitStatus}: {paste(status, collapse = '\n')}")
  }
  if(!file.exists(candidateFile)) {
    cli::cli_abort("Limbo command did not create `candidate.csv`.")
  }
  candidate <- utils::read.csv(candidateFile, stringsAsFactors = FALSE, check.names = FALSE)
  if(nrow(candidate) != 1L) {
    cli::cli_abort("Limbo `candidate.csv` must contain exactly one candidate row.")
  }
  TuneBoostTree_ValidateCandidate(candidate, bounds)
}
####
## Fim
#

