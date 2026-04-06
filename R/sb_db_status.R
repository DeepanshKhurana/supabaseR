#' Check DBI backend status
#'
#' @return A list with DBI connection info
#' @export
sb_db_status <- function() {
  list(
    connected = !is.null(.sb_env$conn),
    schema = get_schema()
  )
}
