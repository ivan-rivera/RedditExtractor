#' Find subreddits by keywords
#'
#' Search for subreddits and their attributes based on a keyword
#'
#' @param keywords A string representing your search query
#' @return A data frame with obtained reddits
#' @examples
#' \dontrun{
#' find_subreddits("cats")
#' }
#' @export
find_subreddits <- function(keywords) build_subreddit_search_url(keywords) |>
  parse_request_url(data_builder = build_subreddit_df) |>
  rbind_list() |>
  dedup_df()


# Built subreddit data frame
build_subreddit_df <- function(json) {
  data.frame(
    id = extract_json_attribute(json, "id"),
    date_utc = extract_json_attribute(json, "created_utc") |> timestamp_to_date(),
    timestamp = extract_json_attribute(json, "created_utc"),
    subreddit = extract_json_attribute(json, "display_name"),
    title = extract_json_attribute(json, "title"),
    description = extract_json_attribute(json, "public_description"),
    subscribers = extract_json_attribute(json, "subscribers") |> na2zero(),
    stringsAsFactors = FALSE
  )
}


# Build the URL to search for subreddits based on given keywords
build_subreddit_search_url <- function(keywords) REDDIT_URL %+%
  "/subreddits/search.json?limit=100&q=" %+%
  space2plus(keywords)
