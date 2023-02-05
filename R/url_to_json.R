# Read URL into a JSON format
url_to_json <- function(url) url |>
  throttle() |>
  utils::URLencode() |>
  readLines(warn = FALSE) |>
  RJSONIO::fromJSON() |>
  tryCatch(error = function(e) {
    warning(e)
    return(list())
  })
