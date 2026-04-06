# .sb_api_abort() / should abort with status code and message from JSON body

    Code
      .sb_api_abort(resp)
    Condition
      Error in `.sb_api_abort()`:
      x API request failed with status 400.
      ! Bad Request
      i Check your query

# .sb_api_abort() / should abort with plain string body when not JSON

    Code
      .sb_api_abort(resp)
    Condition
      Error in `.sb_api_abort()`:
      x API request failed with status 500.
      ! Internal Server Error

