#' Read Supabase credentials from environment variables
#'
#' @return A list of Supabase credentials
sb_db_creds <- function() {
  creds <- list(
    host = Sys.getenv("SUPABASE_HOST"),
    port = 6543,
    dbname = Sys.getenv("SUPABASE_DBNAME"),
    user = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    schema = Sys.getenv("SUPABASE_SCHEMA")
  )

  required <- c("host", "dbname", "user", "password")
  missing <- required[sapply(creds[required], function(x) nchar(x) == 0)]

  if (length(missing) > 0) {
    stop(glue::glue(
      "Missing Supabase credentials: {glue::glue_collapse(missing, sep = ', ')}. ",
      "Set environment variables: SUPABASE_HOST, SUPABASE_DBNAME, SUPABASE_USER, SUPABASE_PASSWORD"
    ))
  }

  creds
}
