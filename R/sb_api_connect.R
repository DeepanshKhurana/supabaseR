#' Connect to Supabase via REST API
#'
#' At least one key must be supplied alongside the URL. When both `key` and
#' `secret_key` are available, the secret key is always preferred for requests
#' because it has higher privileges (bypasses Row Level Security).
#'
#' @param url The Supabase URL (default: from SUPABASE_URL env var)
#' @param key The Supabase API key. Accepts the legacy `anon` JWT key or the
#'   new publishable key (`sb_publishable_...`). Optional when `secret_key` is
#'   provided.
#'   (default: from SUPABASE_PUBLISHABLE_KEY or SUPABASE_ANON_KEY env var)
#' @param secret_key The Supabase secret key. Accepts the legacy `service_role`
#'   JWT key or the new secret key (`sb_secret_...`). When present it is used
#'   for all requests, bypassing Row Level Security.
#'   (default: from SUPABASE_SECRET_KEY or SUPABASE_ROLE_KEY env var)
#' @return Invisible list with API credentials
#' @example man/examples/sb_api_connect.R
#' @export
sb_api_connect <- function(
  url = Sys.getenv("SUPABASE_URL"),
  key = Sys.getenv(
    if (nchar(Sys.getenv("SUPABASE_PUBLISHABLE_KEY")) > 0)
      "SUPABASE_PUBLISHABLE_KEY"
    else
      "SUPABASE_ANON_KEY"
  ),
  secret_key = Sys.getenv(
    if (nchar(Sys.getenv("SUPABASE_SECRET_KEY")) > 0)
      "SUPABASE_SECRET_KEY"
    else
      "SUPABASE_ROLE_KEY"
  )
) {
  if (url == "" || (key == "" && secret_key == "")) {
    cli::cli_abort(
      c(
        "x" = "API credentials not found.",
        "i" = "Set {.envvar SUPABASE_URL} and at least one of
               {.envvar SUPABASE_PUBLISHABLE_KEY}, {.envvar SUPABASE_ANON_KEY},
               {.envvar SUPABASE_SECRET_KEY}, or {.envvar SUPABASE_ROLE_KEY}."
      ),
      call. = FALSE
    )
  }

  if (nchar(key) > 0) .warn_if_legacy_key(key, "key")
  if (nchar(secret_key) > 0) .warn_if_legacy_key(secret_key, "secret_key")

  .sb_env$api_url <- url
  .sb_env$api_key <- if (nchar(key) > 0) key else NULL
  .sb_env$api_secret_key <- if (nchar(secret_key) > 0) secret_key else NULL
  .sb_env$api_available <- TRUE

  cli::cli_alert_success("API credentials set for {.field {url}}")
  invisible(
    list(
      url = url,
      key = if (nchar(key) > 0) key else NULL,
      secret_key = if (nchar(secret_key) > 0) secret_key else NULL
    )
  )
}

#' Detect and warn about legacy JWT-based Supabase API keys
#'
#' Supabase is migrating from long-lived JWT keys (anon / service_role) to a
#' new key system with publishable (`sb_publishable_...`) and secret
#' (`sb_secret_...`) keys. Legacy keys begin with "eyJ" (base64-encoded JSON).
#'
#' @param key The key string to check
#' @param arg_name The argument name to include in the warning message
#' @keywords internal
.warn_if_legacy_key <- function(key, arg_name) {
  if (startsWith(key, "eyJ")) {
    cli::cli_warn(
      c(
        "!" = "{.arg {arg_name}} appears to be a legacy JWT key (anon / service_role).",
        "i" = "Supabase is migrating to publishable ({.code sb_publishable_...}) and
               secret ({.code sb_secret_...}) keys.",
        "i" = "See {.url https://github.com/orgs/supabase/discussions/29260} for details."
      )
    )
  }
}
