# Connect to Supabase

Uses DBI/PostgreSQL to connect directly to your Supabase database. Get
connection parameters from the Connect tab in your Supabase Dashboard
(Project Settings \> Database \> Connection Strings). All three modes
work: Direct Connection, Transaction Pooler, and Session Pooler. If
Direct connection doesn't work, try Transaction or Session Pooler.
Remember to use the correct host, user, and password from your chosen
connection mode.

## Usage

``` r
sb_db_connect(schema = Sys.getenv("SUPABASE_SCHEMA", "public"))
```

## Arguments

- schema:

  The schema name

## Value

Invisible connection object

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect using environment variables
sb_db_connect()

# Connect with a specific schema
sb_db_connect(schema = "public")

# Disconnect when done
sb_db_disconnect()
} # }
```
