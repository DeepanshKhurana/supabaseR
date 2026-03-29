# Connect to Supabase

Unified connection that auto-detects or uses specified backend.

## Usage

``` r
sb_connect(
  backend = c("auto", "db", "api"),
  schema = Sys.getenv("SUPABASE_SCHEMA", "public")
)
```

## Arguments

- backend:

  Backend to use: "auto", "db", or "api"

- schema:

  The schema name (db backend only)

## Value

Invisible connection info

## Examples

``` r
if (FALSE) { # \dontrun{
# Auto-detect backend based on available env vars
sb_connect()

# Explicitly use DBI backend
sb_connect(backend = "db")

# Explicitly use API backend
sb_connect(backend = "api")

# Disconnect when done
sb_disconnect()
} # }
```
