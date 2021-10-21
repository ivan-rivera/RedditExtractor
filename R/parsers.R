# Parse request URL into a data frame with reference URLs of interest
parse_request_url <- function(request_url, data_builder, after = NA, data_list = list(), depth=1, max_depth=100) {
  cat(paste("parsing URLs on page " %+% depth %+% "...\n"))
  json <- url_to_json(request_url %+% (after %||% ""))
  updated_data_list <- json |>
    get_children() |>
    maybe_build(data_builder) |>
    list() |>
    append(data_list)
  if(is.null(json$data$after) || depth + 1 > max_depth) updated_data_list else {
    parse_request_url(
      request_url,
      data_builder,
      "&after=" %+% json$data$after,
      updated_data_list,
      depth+1,
      max_depth
    )
  }
}


parse_thread_url <- function(request_url){
  json <- url_to_json(request_url %+% ".json?limit=500")
  comments_exist <- length(get_comment_json(json)) > 0
  list(
    thread = json |>
      get_thread_json() |>
      build_thread_content_df(request_url),
    comments = if(comments_exist) json |>
      get_comment_json() |>
      build_comments_content_df(request_url) else NA
  )
}

# Apply a builder function to data if it contains elements
maybe_build <- function(data, builder) {
  if(length(data) > 0) builder(data) else NA
}
