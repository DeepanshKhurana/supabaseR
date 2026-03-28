#' Truncate a table
#'
#' @param table The table name
#' @param schema The schema name
#' @return Invisible NULL
#' @export
sb_db_truncate <- function(
  table = NULL,
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert_string(table)

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  DBI::dbExecute(
    conn,
    glue::glue_sql("TRUNCATE TABLE {`schema`}.{`table`}", .con = conn)
  )
  cli::cli_alert_success("Truncated {.field {table}}")
  invisible(NULL)
}
