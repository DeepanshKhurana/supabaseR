#' Read Supabase credentials from environment variables
#'
#' @return A list of Supabase credentials.
read_supabase_creds <- function() {
  creds <- list(
    host = Sys.getenv("SUPABASE_HOST"),
    port = 6543,
    dbname = Sys.getenv("SUPABASE_DBNAME"),
    user = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD")
  )
  if (any(sapply(creds, nchar) == 0)) {
    stop(generate_error())
  } else {
    creds
  }
}

#' Helper function generate an error message
generate_error <- function() {
  print(
    "Cannot connect to Supabase. Are environment variables set?"
  )
}
