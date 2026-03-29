# Connect to Supabase

Connect to Supabase

## Usage

``` r
sb_db_connect(schema = Sys.getenv("SUPABASE_SCHEMA"))
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
