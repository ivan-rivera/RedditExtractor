#' Get all data attributes from search query
#' 
#' @examples
#' \dontrun{
#' reddit_data = get_reddit(search_terms = "science",subreddit = "science",cn_threshold=10)
#' }
#' 
#' @param search_terms A string of terms to be searched on Reddit.
#' @param regex_filter An optional regular expression filter that will remove URLs with titles that do not match the condition.
#' @param subreddit An optional character string that will restrict the search to the specified subreddit.
#' @param cn_threshold Comment number threshold that remove URLs with fewer comments that cn_threshold. 0 by default.
#' @param page_threshold Page threshold that controls the number of pages is going to be searched for a given search word. 1 by default.
#' @param sort_by Sorting parameter, either "comments" (default) or "new".
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with structure / position of the comment with respect to other comments (structure), ID (id), post / thread date (post_date), 
#' comment date (comm_date), number of comments within a post / thread (num_comments), subreddit (subreddit)
#' upvote proportion (upvote_prop), post /thread score (post_score), author of the post / thread (author), user corresponding to the comment (user),
#' comment score (comment_score), controversiality (controversiality), comment (comment), title (title), post / thread text (post_text), URL referenced (link)
#' domain of the references URL (domain)
#' @export

get_reddit = function(search_terms=NA,
                      regex_filter="",
                      subreddit=NA,
                      cn_threshold=1,
                      page_threshold=1,
                      sort_by="comments",
                      wait_time=2){
  
  URL = unique(as.character(reddit_urls(search_terms,
                                        regex_filter,
                                        subreddit,
                                        cn_threshold,
                                        page_threshold,
                                        sort_by,
                                        wait_time)$URL))
  
  retrieved_data = reddit_content(URL,wait_time)
  
  return(retrieved_data)
  
}
