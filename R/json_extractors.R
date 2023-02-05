# Extract JSON content by attribute
extract_json_attribute <- function(json, attribute) json |>
  seq() |>
  sapply(function(item) json[[item]]$data[[attribute]]) |>
  sapply(null2na) |>
  sapply(maybe_to_utf8)


# Extract comment attributes
extract_comments_attributes <- function(json, attribute) json |>
  lapply(function(x) extract_element_attributes(x, attribute)) |>
  unlist()


# Extract an attribute out of a json element
extract_element_attributes <- function(element, attribute) list(
  maybe_to_utf8(element$data[[attribute]]),
  element |>
    get_replies_or_null() |>
    lapply(function(x) extract_element_attributes(x, attribute))
)


# Get either replies or a NULL
get_replies_or_null <- function(element) {
  if(is.list(element$data$replies)) element$data$replies$data$children else NULL
}


# Get the children component from JSON
get_children <- function(json) if(length(json)) json[[2]]$children else NULL


# Extract thread content out of a json structure
get_thread_json <- function(json) json[[1]]$data$children[[1]]$data


# Exract comment content out of a json structure
get_comment_json <- function(json) json[[2]]$data$children
