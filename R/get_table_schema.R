#' Retrieve the schema of a table
#'
#' @param table_name The name of the table.
#' @param schema The schema name.
#' @param conn A database connection object.
#' @return A data frame with table schema details.
#' @export
get_table_schema <- function(
  table_name = NULL,
  schema = "public",
  conn = make_connection()
) {
  checkmate::assert(
    checkmate::check_string(table_name),
    checkmate::check_string(schema),
    combine = "and"
  )
  if (is_valid_table(table_name, conn)) {
    schema_query <- glue::glue_sql(
      "
        SELECT column_name, data_type
        FROM information_schema.columns
        WHERE table_schema = {schema}
        AND table_name = {table_name}
      ",
      .con = conn
    )
    DBI::dbGetQuery(conn, schema_query)
  } else {
    stop(
      glue::glue(
        "Table '{table_name}' does not exist!"
      )
    )
  }
}
