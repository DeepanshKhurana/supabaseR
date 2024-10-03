#' Query data from a table with a freeform text query
#'
#' @param query The custom query as text
#' @param conn A database connection object.
#' @return A data frame with table data.
#' @export
get_freeform_table_query <- function(
  query = NULL,
  conn = make_connection()
) {
  on.exit(DBI::dbDisconnect(conn))
  checkmate::assert_string(query)
  DBI::dbGetQuery(
    conn,
    query
  )
}
