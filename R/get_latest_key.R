#' Get the latest key value for a table
#'
#' @param table_name The name of the table.
#' @param id_column The ID column name.
#' @param schema The schema name.
#' @param conn A database connection object.
#' @return The latest key value plus one.
get_latest_key <- function(
  table_name = NULL,
  id_column = "id",
  schema = "public",
  conn = make_connection()
) {
  checkmate::assert(
    checkmate::check_string(table_name),
    checkmate::check_string(id_column),
    combine = "and"
  )

  if (is_valid_table(table_name, schema, conn)) {
    latest_key_query <- glue::glue(
      "
        SELECT {id_column}
        FROM {schema}.{table_name}
        ORDER BY {id_column} DESC
        LIMIT 1
      ",
      .con = conn
    )
    latest_key <- DBI::dbGetQuery(conn, latest_key_query)

    if (nrow(latest_key) > 0) {
      as.numeric(latest_key[[1]])
    } else {
      1
    }
  } else {
    stop(
      glue::glue(
        "Table '{table_name}' does not exist!"
      )
    )
  }
}
