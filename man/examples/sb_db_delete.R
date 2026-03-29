\dontrun{
# Delete by id
sb_db_delete("users", where = list(id = 1))

# Delete with operator
sb_db_delete("sessions", where = list(expires_at = list(lt = Sys.time())))
}
