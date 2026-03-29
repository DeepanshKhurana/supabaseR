.sb_env <- new.env(parent = emptyenv())

.onLoad <- function(
  libname,
  pkgname
) {
  .sb_env$dbi_available <- FALSE
  .sb_env$api_available <- FALSE
  .sb_env$conn <- NULL

  dbi_vars <- c("SUPABASE_HOST", "SUPABASE_DBNAME", "SUPABASE_USER", "SUPABASE_PASSWORD")
  if (all(nchar(Sys.getenv(dbi_vars)) > 0)) {
    .sb_env$dbi_available <- TRUE
  }

  api_vars <- c("SUPABASE_URL", "SUPABASE_ANON_KEY")
  if (all(nchar(Sys.getenv(api_vars)) > 0)) {
    .sb_env$api_available <- TRUE
  }
}

.onAttach <- function(
  libname,
  pkgname
) {
  packageStartupMessage(startup_message())
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
