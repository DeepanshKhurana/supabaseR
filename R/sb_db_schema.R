#' Get table schema
#'
#' @param table The table name
#' @param schema The schema name
#' @return A data frame with column_name and data_type
#' @example man/examples/sb_db_schema.R
#' @export
sb_db_schema <- function(
  table = NULL,
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert_string(table)

  if (!sb_db_table_exists(table, schema)) {
    stop(glue::glue("Table '{table}' does not exist!"))
  }

  DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "
        SELECT column_name, data_type
        FROM information_schema.columns
        WHERE table_schema = {schema}
        AND table_name = {table}
      ",
      .con = conn
    )
  )
}
