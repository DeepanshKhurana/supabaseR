#' Get a list of tables in the schema
#'
#' @param schema The schema name.
#' @param conn A database connection object.
#' @return A vector of table names.
get_table_list <- function(
  schema = Sys.getenv("SUPABASE_SCHEMA"),
  conn = make_connection()
) {
  DBI::dbGetQuery(
    conn,
    glue::glue_sql(
      "
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = {schema}
        AND table_type = 'BASE TABLE';
      ",
      .con = conn
    )
  ) |>
    as.list() |>
    unname() |>
    unlist()
}
