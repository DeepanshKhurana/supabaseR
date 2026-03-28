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
  schema = Sys.getenv("SUPABASE_SCHEMA"),
  conn = make_connection()
) {
  on.exit(DBI::dbDisconnect(conn))
  checkmate::assert(
    checkmate::check_string(table_name),
    checkmate::check_numeric(id_value),
    checkmate::check_string(id_column),
    combine = "and"
  )

  if (is_valid_table(table_name, schema, conn)) {
    delete_query <- glue::glue(
      "
        DELETE FROM {schema}.{table_name}
        WHERE {id_column} = {id_value}
      "
    )
    DBI::dbExecute(conn, delete_query)
  } else {
    stop(
      glue::glue(
        "Table '{table_name}' does not exist!"
      )
    )
  }
}
