#' Query data from a table
#'
#' @param table_name The name of the table.
#' @param columns The columns spread as a list. Default is list("*")
#' @param filter_query The custom query spread as list
#* e.g. list("WHERE column = 'value'", "ORDER BY column DESC")
#' @param schema The schema name.
#' @param conn A database connection object.
#' @return A data frame with table data.
#' @export
get_table_query <- function(
  table_name = NULL,
  columns = list("*"),
  filter_query = list(),
  schema = "public",
  conn = make_connection()
) {
  on.exit(DBI::dbDisconnect(conn))
  checkmate::assert(
    checkmate::check_string(table_name),
    checkmate::check_list(columns),
    checkmate::check_list(filter_query),
    combine = "and"
  )

  if (length(columns) > 1 || columns[[1]] != "\\*") {
    columns <- paste(columns, collapse = ", ")
  } else {
    columns <- "*"
  }

  if (length(filter_query) > 0) {
    query <- paste(filter_query, collapse = " ")
  } else {
    query <- ""
  }

  if (is_valid_table(table_name, schema, conn)) {
    DBI::dbGetQuery(
      conn,
      glue::glue(
        "
          SELECT {columns} FROM {schema}.{table_name}
          {query}
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
