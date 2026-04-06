# Make a PostgREST API request

Make a PostgREST API request

## Usage

``` r
.sb_api_request(
  method = "GET",
  path,
  params = NULL,
  body = NULL,
  prefer = NULL,
  schema = "public"
)
```

## Arguments

- method:

  HTTP method: "GET", "POST", "PATCH", or "DELETE"

- path:

  URL path to append after the base URL (e.g. "rest/v1/mytable")

- params:

  Named list of URL query parameters

- body:

  Request body (list or data frame, JSON-encoded)

- prefer:

  Character vector of Prefer header values

- schema:

  Schema name; triggers Accept-Profile (GET) or Content-Profile
  (mutations) header when not "public"

## Value

An httr2 response object
