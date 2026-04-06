# Parse the affected-row count from a PostgREST Content-Range header

Parse the affected-row count from a PostgREST Content-Range header

## Usage

``` r
.parse_count_header(response)
```

## Arguments

- response:

  An httr2 response object

## Value

Integer row count, or 0L if the header is absent or unparseable
