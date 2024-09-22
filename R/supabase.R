#' Delete all rows from table
#'
#' @param table_name The name of the table.
#' @param schema The schema name.
#' @param conn A database connection object.
#' @export
empty_table <- function(
    table_name = NULL,
    schema = "public",
    conn = make_connection()
) {
  on.exit(dbDisconnect(conn))
  assert(
    check_string(table_name)
  )
  if (is_valid_table(table_name, conn)) {
    delete_query <- glue(
      "
        TRUNCATE TABLE {schema}.{table_name}
      "
    )
    dbExecute(conn, delete_query)
  } else {
    stop(
      glue(
        "Table '{table_name}' does not exist!"
      )
    )
  }
}
