#' Read Supabase credentials from environment variables
#'
#' Connection parameters should be obtained from the Connect tab in your
#' Supabase Dashboard (Project Settings > Database > Connection Strings).
#' All three connection modes work (Direct, Transaction Pooler, Session Pooler),
#' but if Direct connection fails, try Transaction or Session Pooler modes.
#' Be sure to use the correct host, port, user, and password for your chosen mode.
#' Port defaults to 6543 if not specified.
#'
#' @return A list of Supabase credentials
sb_db_creds <- function() {
  creds <- list(
    host = Sys.getenv("SUPABASE_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_PORT", "6543")),
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
