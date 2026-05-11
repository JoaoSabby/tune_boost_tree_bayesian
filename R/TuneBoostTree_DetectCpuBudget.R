#' Detectar orçamento físico de CPU
#' @noRd

TuneBoostTree_DetectCpuBudget <- function() {

  physical <- tryCatch({
    lscpu <- Sys.which("lscpu")

    if(!nzchar(lscpu)) {
      NA_integer_
    } else {
      raw <- system2(
        lscpu,
        args = "-p=Core,Socket",
        stdout = TRUE,
        stderr = FALSE
      )
      raw <- raw[!startsWith(raw, "#") & nzchar(raw)]
      physicalPairs <- unique(raw)

      if(length(physicalPairs) > 0L) {
        as.integer(length(physicalPairs))
      } else {
        NA_integer_
      }
    }
  }, error = function(e) NA_integer_, warning = function(w) NA_integer_)
  if(is.na(physical) || physical < 1L) {
    physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  }
  if(is.na(physical) || physical < 1L) {
    physical <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  if(is.na(physical) || physical < 1L) {
    physical <- 1L
  }
  reserve <- min(2L, max(0L, as.integer(physical) - 1L))
  as.integer(max(1L, as.integer(physical) - reserve))
}
####
## Fim
#

