#' Returns relevant reddit URLs
#' 
#' @examples
#' \dontrun{
#' example_urls = reddit_urls(search_terms="science")
#' }
#' 
#' @param search_terms A character string to be searched on Reddit.
#' @param regex_filter An optional regular expression filter that will remove URLs with titles that do not match the condition.
#' @param subreddit An optional character string that will restrict the search to the specified subreddits (separated by space).
#' @param cn_threshold Comment number threshold that remove URLs with fewer comments that cn_threshold. 0 by default.
#' @param page_threshold Page threshold that controls the number of pages is going to be searched for a given search word. 1 by default.
#' @param sort_by Sorting parameter, either "comments" (default) or "new".
#' @param wait_time wait time in seconds between page requests. 2 by default and it is also the minimum (API rate limit).
#' @return A data frame with URLs (links), number of comments (num_comments), title (title),date (date) and subreddit (subreddit).
#' @export

reddit_urls = function(search_terms=NA,
                       regex_filter="",
                       subreddit=NA,
                       cn_threshold=0,
                       page_threshold=1,
                       sort_by="relevance",
                       wait_time=2){
  
  if(!grepl("^comments$|^new$|^relevance$",sort_by)){stop("sort_by must be either 'new', 'comments' or 'relevance'")}
  
  sterms       = ifelse(is.na(search_terms),NA,gsub("\\s","+",search_terms))
  
  cached_links = data.frame(date = as.Date(character()), 
                            num_comments = numeric(), 
                            title = character(), 
                            subreddit=character(),
                            URL = character())
  
  subreddit      = ifelse(is.na(subreddit),"",paste0("r/",gsub("\\s+","+",subreddit),"/"))
  sterms         = ifelse(is.na(sterms),"",paste0("q=",sterms,"&restrict_sr=on&"))
  sterms_prefix  = ifelse(sterms=="","new","search")
  
  search_address = search_query = paste0("https://www.reddit.com/",subreddit,sterms_prefix,".json?",sterms,"sort=",sort_by)
  
  next_page      = index = ""
  page_counter   = 0
  comm_filter    = 10000
  
  while(is.null(next_page)==FALSE & page_counter < page_threshold & comm_filter >= cn_threshold & length(index)>0){
    
    search_JSON  = tryCatch(RJSONIO::fromJSON(readLines(search_query, warn = FALSE)), error = function(e) NULL)
    
    if(is.null(search_JSON)){
      cat(paste("Cannot connect to the website, skipping...\n"))
      next
    }
    
    else{
      
      contents      = search_JSON[[2]]$children
      
      search_permalink    = paste0("http://www.reddit.com",sapply(seq(contents),function(x)contents[[x]]$data$permalink))
      search_num_comments = sapply(seq(contents),function(x)contents[[x]]$data$num_comments)
      search_title        = sapply(seq(contents),function(x)contents[[x]]$data$title)
      search_score        = sapply(seq(contents),function(x)contents[[x]]$data$score)
      search_subreddit    = sapply(seq(contents),function(x)contents[[x]]$data$subreddit)
      
      index = which(search_num_comments >= cn_threshold & grepl(regex_filter,search_title,ignore.case=T,perl=T))
      
      if(length(index)>0){
        
        search_date  = format(as.Date(as.POSIXct(unlist(lapply(seq(contents),function(x)contents[[x]]$data$created_utc)),origin="1970-01-01")),"%d-%m-%y")
        
        
        temp_dat     = data.frame(date             = search_date,
                                  num_comments     = search_num_comments, 
                                  title            = search_title, 
                                  subreddit        = search_subreddit,
                                  URL              = search_permalink,
                                  stringsAsFactors = FALSE
        )[index,]
        
        cached_links = as.data.frame(rbind(cached_links,temp_dat))
        
        next_page    = search_JSON$data$after
        comm_filter  = utils::tail(search_num_comments,1)
        search_query = paste0(search_address,"&after=",next_page)
        page_counter = page_counter + 1
        
      }
      Sys.sleep(min(2,wait_time))
    }
  }
  
  final_table = cached_links[!duplicated(cached_links),]
  
  if(dim(final_table)[1]==0){
    cat(paste("\nNo results retrieved, check your query"))} else{
      
      remove_row  = which(final_table[,1]=="")
      
      if(length(remove_row)>0){final_table = final_table[-remove_row,]}
      
      return(final_table)
      
    }
}
