#' Generate the CRON time
#' @param time The time for which the CRON time is to be generated
#' @param tz The timezone of the time
#' @return The CRON time
#' @export
get_cron_time <- function(
  time = Sys.time(),
  tz = "Asia/Kolkata"
) {
  minute <- as.numeric(
    format(
      time,
      "%M",
      tz = tz
    )
  )
  hour <- as.numeric(
    format(
      time,
      "%H",
      tz = tz
    )
  )
  glue::glue(
    "{hour}:{ifelse(minute < 15, '00', ifelse(minute < 45, '30', '00'))}"
  )
}
