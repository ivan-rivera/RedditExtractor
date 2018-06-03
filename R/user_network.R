#' User relationship network
#'
#' @param thread_df a data frame with columns structure, user, author -- designed for the output of the reddit_content() or get_reddit() functions
#' @param include_author a TRUE/FALSE indicator for whether or not the author should be considered in the resulting network, if TRUE, comments at the top of the tree will be treated as relies to the original posts, otherwise these comments will be disregarded
#' @param agg a TRUE/FALSE indicator that allows you to aggregate results, if FALSE, the results will remain disaggregated
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr filter select mutate left_join coalesce rename group_by ungroup summarise row_number
#' @importFrom rlang .data
#' @return a list with df (effectively an edge list without IDs), node_df (node list), edge_df (edge list), igrpah (igraph object), plot (plot object)
#' @export
#'
#' @examples
#' # load libraries
#' library(dplyr)
#' library(RedditExtractoR)
#' target_urls <- reddit_urls(search_terms="cats", subreddit="Art", cn_threshold=50)
#' target_df <- target_urls %>% 
#' filter(num_comments==min(target_urls$num_comments)) %$% 
#' URL %>% reddit_content # get the contents of a small thread
#' network_list <- target_df %>% user_network(include_author=FALSE, agg=TRUE) # extract the network
#' network_list$plot # explore the plot
#' str(network_list$df) # check out the contents
user_network <- function(thread_df, include_author=TRUE, agg=FALSE){
  
  # identify sender / receiver relationships
  sender_receiver_df <- thread_df %>% 
    select(.data$structure, .data$user, .data$author, .data$comment) %>% 
    rename("sender"=.data$user) %>%
    mutate(response_to=ifelse(!grepl("_", .data$structure), "", gsub("_\\d+$", "", .data$structure))) %>%
    left_join(
      thread_df %>% 
        select(.data$structure, .data$user) %>%
        rename("response_to"=.data$structure, "receiver"=.data$user),
      by="response_to"
    ) %>% mutate(receiver=coalesce(.data$receiver, ifelse(include_author, .data$author, ""))) %>%
    filter(
      .data$sender!=.data$receiver, 
      !(.data$sender %in% c("[deleted]", "")), 
      !(.data$receiver %in% c("[deleted]", ""))
    ) %>% mutate(count=1) %>%
    select(.data$sender, .data$receiver, .data$comment, .data$count)
  
  if(agg){
    sender_receiver_df %<>% 
      group_by(.data$sender, .data$receiver) %>%
      summarise(
        count=sum(.data$count),
        comment=paste(.data$comment, collapse="\n\n")
      ) %>% ungroup
  }
  
  # generate nodes
  node_df <- data.frame(
    user=with(sender_receiver_df, {unique(c(sender, receiver))}), 
    stringsAsFactors=FALSE
  ) %>% mutate(id=as.integer(row_number()-1)) %>% select(.data$id, .data$user)
  
  # generate edges
  edge_df <- sender_receiver_df %>% left_join(
    node_df %>% rename("sender"=.data$user, "from"=.data$id),
    by="sender"
  ) %>% left_join(
    node_df %>% rename("receiver"=.data$user, "to"=.data$id),
    by="receiver"
  ) %>% rename("weight"=.data$count, "title"=.data$comment) %>% 
    select(.data$from, .data$to, .data$weight, .data$title)
  
  # generate an igraph object
  ig_object <- igraph::graph_from_data_frame(d=edge_df, vertices=node_df, directed=TRUE)
  
  # generate a plot
  plot_object <- visNetwork::visNetwork(
    node_df %>% rename("label"=.data$user), 
    edge_df %>% rename("width"=.data$weight) %>% mutate(arrows="to"),
    main="User Network")
  
  # package the outputs
  out_list <- list(
    "df"=sender_receiver_df,
    "nodes"=node_df,
    "edges"=edge_df,
    "igraph"=ig_object,
    "plot"=plot_object
  )
  
  return(out_list)
  
}
