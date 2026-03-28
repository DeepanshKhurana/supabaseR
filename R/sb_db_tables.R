#' List tables in schema
#'
#' @param schema The schema name
#' @return A character vector of table names
#' @export
sb_db_tables <- function(
  schema = get_schema()
) {
  conn <- get_connection()
  checkmate::assert_string(schema)

  DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = {schema}
        AND table_type = 'BASE TABLE'
      ",
      .con = conn
    )
  )$table_name
}
