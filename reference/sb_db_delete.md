# Delete rows from a table

Delete rows from a table

## Usage

``` r
sb_db_delete(table = NULL, where = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- where:

  A named list for WHERE clause. Supports operators via nested lists.

- schema:

  The schema name

## Value

Number of rows deleted (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
# Delete by id
sb_db_delete("users", where = list(id = 1))

# Delete with operator
sb_db_delete("sessions", where = list(expires_at = list(lt = Sys.time())))
} # }
```
