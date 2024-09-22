#' Delete a row from a table
#'
#' @param table_name The name of the table.
#' @param id_value The ID value of the row to delete.
#' @param id_column The ID column name.
#' @param schema The schema name.
#' @param conn A database connection object.
#' @export
delete_table_row <- function(
  table_name = NULL,
  id_value = NULL,
  id_column = "id",
  schema = "public",
  conn = make_connection()
) {
  on.exit(dbDisconnect(conn))
  assert(
    check_string(table_name),
    check_numeric(id_value),
    check_string(id_column),
    combine = "and"
  )

  if (is_valid_table(table_name, conn)) {
    delete_query <- glue(
      "
        DELETE FROM {schema}.{table_name}
        WHERE {id_column} = {id_value}
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
