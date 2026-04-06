#' Disconnect from Supabase API
#'
#' Clears the stored API credentials from the session.
#'
#' @return Invisible NULL
#' @example man/examples/sb_api_disconnect.R
#' @export
sb_api_disconnect <- function() {
  .sb_env$api_url <- NULL
  .sb_env$api_key <- NULL
  .sb_env$api_secret_key <- NULL
  .sb_env$api_available <- FALSE
  cli::cli_alert_success("API credentials cleared")
  invisible(NULL)
}
