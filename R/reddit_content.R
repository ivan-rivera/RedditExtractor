#' Extract data attributes
#' 
#' @examples
#' \dontrun{
#' example_attr = reddit_content(URL="reddit.com/r/gifs/comments/39tzsy/whale_watching")
#' }
#' 
#' @param URL a string or a vector of strings with the URL address of pages of interest
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with structure / position of the comment with respect to other comments (structure), ID (id), post / thread date (post_date), 
#' comment date (comm_date), number of comments within a post / thread (num_comments), subreddit (subreddit)
#' upvote proportion (upvote_prop), post /thread score (post_score), author of the post / thread (author), user corresponding to the comment (user),
#' comment score (comment_score), controversiality (controversiality), comment (comment), title (title), post / thread text (post_text), URL referenced (link)
#' domain of the references URL (domain)
#' @export

reddit_content = function(URL,wait_time=2){
  
  if(is.null(URL) | length(URL)==0 | !is.character(URL)){stop("invalid URL parameter")}
  
  # setting up a function for extraction of comment specific information:
  GetAttribute  = function(node,feature){
    Attribute   = node$data[[feature]]
    replies     = node$data$replies
    reply.nodes = if (is.list(replies)) replies$data$children else NULL
    return(list(Attribute, lapply(reply.nodes,function(x){GetAttribute(x,feature)})))  
  }
  
  get.structure = function(node, depth=0) {
    if(is.null(node)) {return(list())}
    filter     = is.null(node$data$author)
    replies     = node$data$replies
    reply.nodes = if (is.list(replies)) replies$data$children else NULL
    return(list(paste0(filter," ",depth), lapply(1:length(reply.nodes), function(x) get.structure(reply.nodes[[x]], paste0(depth, "_", x)))))
  }
  
  # setting up the data frame
  data_extract = data.frame(id               = numeric(),
                            structure        = character(),
                            post_date        = as.Date(character()),
                            comm_date        = as.Date(character()),
                            num_comments     = numeric(),
                            subreddit        = character(),
                            upvote_prop      = numeric(),
                            post_score       = numeric(),
                            author           = character(),
                            user             = character(),
                            comment_score    = numeric(),
                            controversiality = numeric(),
                            comment          = character(),
                            title            = character(),
                            post_text        = character(),
                            link             = character(),
                            domain           = character(),
                            URL              = character())
  
  
  pb = utils::txtProgressBar(min = 0, max = length(URL), style = 3)
  
  for(i in seq(URL)){
    
    if(!grepl("^https?://(.*)",URL[i])) URL[i] = paste0("https://www.",gsub("^.*(reddit\\..*$)","\\1",URL[i]))
    if(!grepl("\\?ref=search_posts$",URL[i])) URL[i] = paste0(gsub("/$","",URL[i]),"/?ref=search_posts")
    
    X        = paste0(gsub("\\?ref=search_posts$","",URL[i]),".json?limit=500") # 500 is the maximum
    raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),error = function(e) NULL)
    
    # try again if it fails
    if(is.null(raw_data)){
      Sys.sleep(min(1,wait_time))
      raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),error = function(e) NULL)
    }
    
    if(is.null(raw_data)==FALSE){
      
      # extracting comment specific information:
      meta.node     = raw_data[[1]]$data$children[[1]]$data
      main.node     = raw_data[[2]]$data$children
      
      if(min(length(meta.node),length(main.node))>0){
        
        structure     = unlist(lapply(1:length(main.node), function(x) get.structure(main.node[[x]], x)))
        
        TEMP          =          data.frame(id               = NA,
                                            structure        = gsub("FALSE ","",structure[!grepl("TRUE",structure)]),
                                            post_date        = format(as.Date(as.POSIXct(meta.node$created_utc,origin="1970-01-01")),"%d-%m-%y"),
                                            comm_date        = format(as.Date(as.POSIXct(unlist(lapply(main.node, function(x){GetAttribute(x,"created_utc")})),
                                                                                         origin="1970-01-01")),"%d-%m-%y"),
                                            num_comments     = meta.node$num_comments,
                                            subreddit        = ifelse(is.null(meta.node$subreddit),"UNKNOWN",meta.node$subreddit),
                                            upvote_prop      = meta.node$upvote_ratio,
                                            post_score       = meta.node$score,
                                            author           = meta.node$author,
                                            user             = unlist(lapply(main.node, function(x){GetAttribute(x,"author")})),
                                            comment_score    = unlist(lapply(main.node, function(x){GetAttribute(x,"score")})),
                                            controversiality = unlist(lapply(main.node, function(x){GetAttribute(x,"controversiality")})),
                                            comment          = unlist(lapply(main.node, function(x){GetAttribute(x,"body")})),
                                            title            = meta.node$title,
                                            post_text        = meta.node$selftext,
                                            link             = meta.node$url,
                                            domain           = meta.node$domain,
                                            URL              = URL[i],
                                            stringsAsFactors = FALSE)
        
        TEMP$id = 1:nrow(TEMP)
        
        if(dim(TEMP)[1]>0 & dim(TEMP)[2]>0) data_extract = rbind(TEMP,data_extract)
        else print(paste("missed",i,":",URL[i]))
        
      }
      
    }
    
    utils::setTxtProgressBar(pb, i)
    
    Sys.sleep(min(2,wait_time))
  }
  
  close(pb)
  
  return(data_extract)
  
}
