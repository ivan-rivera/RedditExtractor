#' Find Reddit thread URLs
#'
#' Find URLs to reddit threads of interest.
#' There are 2 available search strategies: by keywords and by home page. Using a set of keywords
#' Can help you narrow down your search to a topic of interest that crosses multiple subreddits
#' whereas searching by home page can help you find, for example, top posts within a specific subreddit
#'
#' @param keywords A optional string that you want to search for, e.g. "cute kittens".
#' If NA, then either your front page will be searched or the front page of a specified subreddit
#'
#' @param sort_by A string representing how you want Reddit to sort the results.
#' Note that this string is conditional on whether you are searching by keywords or not.
#' If you are searching by keywords, then it must be one of: relevance, comments, new, hot, top;
#' if you are not searching by keywords, then it must be one of: hot, new, top, rising
#'
#' @param subreddit (optional) A string representing the subreddit of interest
#'
#' @param period A string representing the period of interest (hour, day, week, month, year, all)
#'
#' @return a data frame with URLs to Reddit threads that are relevant to your input parameters
#'
#' @examples
#' \dontrun{
#' find_thread_urls(keywords="cute kittens", subreddit="cats", sort_by="new", period="month")
#' find_thread_urls(subreddit="cats", sort_by="rising", period="all")
#' }
#' @export
find_thread_urls <- function(keywords=NA, sort_by="top", subreddit=NA, period="month") ifelse(
  is.na(keywords),
  build_homepage_url(sort_by, subreddit, period),
  build_keywords_url(keywords, sort_by, subreddit, period)
) |>
  parse_request_url(data_builder = build_thread_df) |>
  rbind_list() |>
  dedup_df()


# Build data frame with URLs of interest
build_thread_df <- function(json) {
  data.frame(
    date_utc = extract_json_attribute(json, "created_utc") |> timestamp_to_date(),
    timestamp = extract_json_attribute(json, "created_utc"),
    title = extract_json_attribute(json, "title"),
    text = extract_json_attribute(json, "selftext"),
    subreddit = extract_json_attribute(json, "subreddit"),
    comments = extract_json_attribute(json, "num_comments"),
    url = REDDIT_URL %+% extract_json_attribute(json, "permalink"),
    stringsAsFactors = FALSE
  )
}


# Build a URL string to search Reddit by keywords
build_keywords_url <- function(keywords, sort_by, subreddit, period) {
  validate_one_of(sort_by, SORT_KEYWORD_OPTIONS)
  build_base_request_url(subreddit) %+%
    "search.json?" %+%
    ifelse(is.na(subreddit), "", "restrict_sr=on&") %+%
    "q=" %+% space2plus(keywords) %+%
    "&sort=" %+% sort_by %+%
    "&" %+% create_url_limits(period)
}


# Build a URL string to search the homepage restricted to the specified parameters
build_homepage_url <- function(sort_by, subreddit, period) {
  validate_one_of(sort_by, SORT_HOMEPAGE_OPTIONS)
  build_base_request_url(subreddit) %+%
    sort_by %+% ".json?" %+%
    create_url_limits(period)
}


# Build base search request URL conditional on subreddit
build_base_request_url <- function(subreddit) REDDIT_URL %+%
  ifelse(is.na(subreddit), "/", "/r/" %+% space2plus(subreddit) %+% "/")


# Append time and result limits to the search query; note limit=100 is the max
create_url_limits <- function(period) {
  validate_one_of(period, PERIOD_OPTIONS)
  "t=" %+% period %+% "&limit=100"
}
