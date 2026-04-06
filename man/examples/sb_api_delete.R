\dontrun{
sb_api_connect()

# Delete a row by id
sb_api_delete("users", where = list(id = 1))

# Delete with an operator
sb_api_delete("logs", where = list(created_at = list(lt = "2024-01-01")))
}
