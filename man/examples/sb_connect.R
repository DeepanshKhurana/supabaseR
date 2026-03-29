\dontrun{
# Auto-detect backend based on available env vars
sb_connect()

# Explicitly use DBI backend
sb_connect(backend = "db")

# Explicitly use API backend
sb_connect(backend = "api")

# Disconnect when done
sb_disconnect()
}
