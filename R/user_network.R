#' User relationship network
#'
#' @param thread_df a data frame with columns structure, user, author -- designed for the output of the reddit_content() or get_reddit() functions
#' @param include_author a TRUE/FALSE indicator for whether or not the author should be considered in the resulting network, if TRUE, comments at the top of the tree will be treated as relies to the original posts, otherwise these comments will be disregarded
#' @param agg a TRUE/FALSE indicator that allows you to aggregate results, if FALSE, the results will remain disaggregated
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr select mutate left_join coalesce rename group_by ungroup summarise row_number
#' @return a list with df (effectively an edge list without IDs), node_df (node list), edge_df (edge list), igrpah (igraph object), plot (plot object)
#' @export
#'
#' @examples
#' # load libraries
#' library(dplyr)
#' library(RedditExtractoR)
#' target_urls <- reddit_urls(search_terms="cats", subreddit="Art", cn_threshold=50) # isolate some URLs
#' target_df <- target_urls %>% filter(num_comments==min(target_urls$num_comments)) %$% URL %>% reddit_content # get the contents of a small thread
#' network_list <- target_df %>% user_network(include_author=FALSE, agg=TRUE) # extract the network
#' network_list$plot # explore the plot
#' str(network_list$df) # check out the contents
user_network <- function(thread_df, include_author=TRUE, agg=FALSE){
  
  # identify sender / receiver relationships
  sender_receiver_df <- thread_df %>% 
    select(structure, user, author, comment) %>% 
    rename("sender"=user) %>%
    mutate(response_to=ifelse(!grepl("_", structure), "", gsub("_\\d+$", "", structure))) %>%
    left_join(
      target_df %>% 
        select(structure, user) %>%
        rename("response_to"=structure, "receiver"=user),
      by="response_to"
    ) %>% mutate(receiver=coalesce(receiver, ifelse(include_author, author, ""))) %>%
    filter(
      sender!=receiver, 
      !(sender %in% c("[deleted]", "")), 
      !(receiver %in% c("[deleted]", ""))
    ) %>% mutate(count=1) %>%
    select(sender, receiver, comment, count)
  
  if(agg){
    sender_receiver_df %<>% 
      group_by(sender, receiver) %>%
      summarise(
        count=sum(count),
        comment=paste(comment, collapse="\n\n")
      ) %>% ungroup
  }
  
  # generate nodes
  node_df <- data.frame(
    user=with(sender_receiver_df, {unique(c(sender, receiver))}), 
    stringsAsFactors=FALSE
  ) %>% mutate(id=as.integer(row_number()-1)) %>% select(id, user)
  
  # generate edges
  edge_df <- sender_receiver_df %>% left_join(
    node_df %>% rename("sender"=user, "from"=id),
    by="sender"
  ) %>% left_join(
    node_df %>% rename("receiver"=user, "to"=id),
    by="receiver"
  ) %>% rename("weight"=count, "title"=comment) %>% 
    select(from, to, weight, title)
  
  # generate an igraph object
  ig_object <- igraph::graph_from_data_frame(d=edge_df, vertices=node_df, directed=TRUE)
  
  # generate a plot
  plot_object <- visNetwork::visNetwork(
    node_df %>% rename("label"=user), 
    edge_df %>% rename("width"=weight) %>% mutate(arrows="to"),
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
