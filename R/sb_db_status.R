#' Check available backends
#'
#' @return A list with backend availability
#' @export
sb_db_status <- function() {
  list(
    dbi = .sb_env$dbi_available,
    api = .sb_env$api_available,
    connected = !is.null(.sb_env$conn)
  )
}
