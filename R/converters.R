# Convert a UTC timestamp to a date in format YYYY-MM-DD
timestamp_to_date <- function(timestamp) timestamp |>
  as.POSIXct(origin="1970-01-01") |>
  as.Date() |>
  format("%Y-%m-%d")


# Convert NULL to NA values
null2na <- function(x) ifelse(is.null(x), NA, x)


# Convert NA to zeros
na2zero <- function(x) ifelse(is.na(x), 0, x)


# Convert spaces to "+" in a string
space2plus <- function(s) gsub("\\s", "+", s)


# Convert to UTF-8 format
to_utf8 <- function(x) iconv(x, "latin1", "UTF-8", '')


# Convert to UTF-8 format if the input is a character
maybe_to_utf8 <- function(x) if(is.character(x)) to_utf8(x) else x


# Strip everything following ".json" from a string
strip_json <- function(s) gsub("\\.json.*$", "", s)
