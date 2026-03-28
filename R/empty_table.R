#' Delete all rows from table
#'
#' @param table_name The name of the table.
#' @param schema The schema name.
#' @param conn A database connection object.
#' @export
empty_table <- function(
  table_name = NULL,
  schema = Sys.getenv("SUPABASE_SCHEMA"),
  conn = make_connection()
) {
  on.exit(DBI::dbDisconnect(conn))
  checkmate::assert(
    checkmate::check_string(table_name)
  )
  if (is_valid_table(table_name, schema, conn)) {
    delete_query <- glue::glue(
      "
        TRUNCATE TABLE {schema}.{table_name}
      "
    )
    dbExecute(conn, delete_query)
  } else {
    stop(
      glue::glue(
        "Table '{table_name}' does not exist!"
      )
    )
  }
}
