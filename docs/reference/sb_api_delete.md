# Delete rows from a table via API

Delete rows from a table via API

## Usage

``` r
sb_api_delete(table = NULL, where = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- where:

  A named list for filtering. Supports operators via nested lists.

- schema:

  The schema name

## Value

Number of rows deleted (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# Delete a row by id
sb_api_delete("users", where = list(id = 1))

# Delete with an operator
sb_api_delete("logs", where = list(created_at = list(lt = "2024-01-01")))
} # }
```
