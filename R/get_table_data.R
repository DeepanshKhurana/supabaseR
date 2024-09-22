#' Retrieve data from a table
#'
#' @param table_name The name of the table.
#' @param limit The number of rows to retrieve.
#' @param schema The schema name.
#' @param conn A database connection object.
#' @return A data frame with table data.
#' @export
get_table_data <- function(
  table_name = NULL,
  limit = 0,
  schema = "public",
  conn = make_connection()
) {
  on.exit(DBI::dbDisconnect(conn))
  checkmate::assert(
    checkmate::check_string(table_name),
    checkmate::check_numeric(limit),
    combine = "and"
  )

  query_filter <- if (limit > 0) glue::glue("LIMIT {limit}") else ""

  if (is_valid_table(table_name, conn)) {
    DBI::dbGetQuery(
      conn,
      glue::glue(
        "
          SELECT * FROM {schema}.{table_name}
          {query_filter}
        ",
        .con = conn
      )
    )
  } else {
    stop(
      glue::glue(
        "Table '{table_name}' does not exist!"
      )
    )
  }
}
