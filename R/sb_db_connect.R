#' Connect to Supabase
#'
#' @param schema The schema name
#' @return Invisible connection object
#' @example man/examples/sb_db_connect.R
#' @export
sb_db_connect <- function(
  schema = Sys.getenv("SUPABASE_SCHEMA")
) {
  if (!.sb_env$dbi_available) {
    stop(
      "DBI credentials not found.\
      Set SUPABASE_HOST, SUPABASE_DBNAME, SUPABASE_USER, SUPABASE_PASSWORD."
    )
  }

  creds <- sb_db_creds()
  .sb_env$conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = creds$host,
    port = creds$port,
    dbname = creds$dbname,
    user = creds$user,
    password = creds$password
  )
  .sb_env$schema <- schema

  cli::cli_alert_success("Connected to {.field {creds$host}}")
  invisible(.sb_env$conn)
}

#' Disconnect from Supabase
#'
#' @return Invisible NULL
#' @export
sb_db_disconnect <- function() {
  if (!is.null(.sb_env$conn)) {
    DBI::dbDisconnect(.sb_env$conn)
    .sb_env$conn <- NULL
    cli::cli_alert_success("Disconnected")
  }
  invisible(NULL)
}

#' Get the current connection
#'
#' @return The current database connection
get_connection <- function() {
  if (is.null(.sb_env$conn)) {
    stop("Not connected. Call sb_db_connect() first.")
  }
  .sb_env$conn
}

#' Get the current schema
#'
#' @return The current schema name
get_schema <- function() {
  if (is.null(.sb_env$schema)) {
    Sys.getenv("SUPABASE_SCHEMA")
  } else {
    .sb_env$schema
  }
}
