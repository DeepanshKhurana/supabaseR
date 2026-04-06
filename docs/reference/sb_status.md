# Get connection status

Returns availability and connection state for both backends, the active
backend (if
[`sb_connect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_connect.md)
has been called), and the current schema.

## Usage

``` r
sb_status()
```

## Value

A list with `dbi`, `api`, `backend`, and `schema` entries
