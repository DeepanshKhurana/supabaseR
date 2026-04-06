# Insert rows into a table via API

Insert rows into a table via API

## Usage

``` r
sb_api_insert(table = NULL, data = NULL, schema = get_schema())
```

## Arguments

- table:

  The table name

- data:

  A data frame of rows to insert

- schema:

  The schema name

## Value

Number of rows inserted (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
sb_api_connect()

# Insert a single row
sb_api_insert("users", data.frame(name = "Alice", email = "alice@example.com"))

# Insert multiple rows
new_users <- data.frame(
  name  = c("Bob", "Carol"),
  email = c("bob@example.com", "carol@example.com")
)
sb_api_insert("users", new_users)
} # }
```
