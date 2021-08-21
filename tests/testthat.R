library(testthat)
library(RedditExtractoR)

# This is how the test data was generated
# if(FALSE){
#   thread_json <- url_to_json("https://www.reddit.com/r/todayilearned/comments/oxddja/til_beavers_build_their_dams_as_an_instinct_to/" %+% ".json?limit=500")
#   regular_user_json <- build_user_search_url("memes") |> url_to_json()
#   about_user_json <- build_user_info_url("memes") |> url_to_json()
#   thread_urls_json <- build_homepage_url("top", "Sverige", "month") |> url_to_json()
#   empty_thread_urls_json <- build_homepage_url("top", "bigpants", "month") |> url_to_json()
#   subreddit_json <- build_subreddit_search_url("cats") |> url_to_json()
#   subreddit_json$data$after <- NULL
#   usethis::use_data(
#     thread_urls_json,
#     empty_thread_urls_json,
#     subreddit_json,
#     about_user_json,
#     regular_user_json,
#     thread_json,
#     internal=TRUE,
#     overwrite=TRUE
#   )
# }

test_check("RedditExtractoR")
