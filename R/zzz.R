.sb_env <- new.env(parent = emptyenv())

.onLoad <- function(
  libname,
  pkgname
) {
  .sb_env$dbi_available <- FALSE
  .sb_env$api_available <- FALSE
  .sb_env$conn <- NULL
  .sb_env$legacy_key_warning <- FALSE
  .sb_env$legacy_secret_warning <- FALSE

  dbi_vars <- c("SUPABASE_HOST", "SUPABASE_DBNAME", "SUPABASE_USER", "SUPABASE_PASSWORD")
  if (all(nchar(Sys.getenv(dbi_vars)) > 0)) {
    .sb_env$dbi_available <- TRUE
  }

  api_url <- Sys.getenv("SUPABASE_URL")

  api_key <- Sys.getenv("SUPABASE_PUBLISHABLE_KEY")
  if (nchar(api_key) == 0) api_key <- Sys.getenv("SUPABASE_ANON_KEY")

  secret_key <- Sys.getenv("SUPABASE_SECRET_KEY")
  if (nchar(secret_key) == 0) secret_key <- Sys.getenv("SUPABASE_ROLE_KEY")

  if (nchar(api_url) > 0 && (nchar(api_key) > 0 || nchar(secret_key) > 0)) {
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- api_url
    .sb_env$api_key <- if (nchar(api_key) > 0) api_key else NULL
    .sb_env$api_secret_key <- if (nchar(secret_key) > 0) secret_key else NULL

    if (nchar(api_key) > 0 && startsWith(api_key, "eyJ")) {
      .sb_env$legacy_key_warning <- TRUE
    }
    if (nchar(secret_key) > 0 && startsWith(secret_key, "eyJ")) {
      .sb_env$legacy_secret_warning <- TRUE
    }
  }
}

.onAttach <- function(
  libname,
  pkgname
) {
  packageStartupMessage(startup_message())

  if (isTRUE(.sb_env$legacy_key_warning)) {
    warning(
      "SUPABASE_ANON_KEY appears to be a legacy JWT key. ",
      "Supabase is migrating to new-format keys (sb_publishable_..., sb_secret_...). ",
      "See https://github.com/orgs/supabase/discussions/29260",
      call. = FALSE
    )
  }
  if (isTRUE(.sb_env$legacy_secret_warning)) {
    warning(
      "SUPABASE_ROLE_KEY appears to be a legacy JWT key. ",
      "Supabase is migrating to new-format keys (sb_publishable_..., sb_secret_...). ",
      "See https://github.com/orgs/supabase/discussions/29260",
      call. = FALSE
    )
  }
}

startup_message <- function() {
  dbi_status <- if (.sb_env$dbi_available) {
    cli::col_green(cli::symbol$tick)
  } else {
    cli::col_red(cli::symbol$cross)
  }
  api_status <- if (.sb_env$api_available) {
    cli::col_green(cli::symbol$tick)
  } else {
    cli::col_red(cli::symbol$cross)
  }

  glue::glue(
    "{cli::col_blue('supabaseR')} {cli::col_grey('v', utils::packageVersion('supabaseR'))}
  DBI Backend: {dbi_status}
  API Backend: {api_status}"
  )
}

.onUnload <- function(
  libpath
) {
  if (!is.null(.sb_env$conn)) {
    try(DBI::dbDisconnect(.sb_env$conn), silent = TRUE)
  }
}
