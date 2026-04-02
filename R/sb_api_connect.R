#' Connect to Supabase via REST API
#'
#' @param url The Supabase URL (default: from SUPABASE_URL env var)
#' @param key The Supabase API key (default: from SUPABASE_ANON_KEY env var)
#' @return Invisible list with API credentials
#' @export
sb_api_connect <- function(
    url = Sys.getenv("SUPABASE_URL"),
    key = Sys.getenv("SUPABASE_ANON_KEY")
) {
  if (url == "" || key == "") {
    stop(
      "API credentials not found.\
      Set SUPABASE_URL and SUPABASE_ANON_KEY."
    )
  }

  .sb_env$api_url <- url
  .sb_env$api_key <- key

  cli::cli_alert_success("API credentials set for {.field {url}}")
  invisible(list(url = url, key = key))
}
