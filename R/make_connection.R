#' Create a database connection
#'
#' @param supabase_creds A list of Supabase credentials.
#' @return A database connection object.
make_connection <- function(
  supabase_creds = read_supabase_creds()
) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = supabase_creds$host,
    port = supabase_creds$port,
    dbname = supabase_creds$dbname,
    user = supabase_creds$user,
    password = supabase_creds$password
  )
}
