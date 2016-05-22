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

#' Create a graph file from a single Reddit thread
#' 
#' @examples
#' \dontrun{
#' my_url = "reddit.com/r/web_design/comments/2wjswo/design_last_reordering_the_web_design_process/"
#' url_data = reddit_content(my_url)
#' graph_object = construct_graph(url_data)
#' }
#' 
#' @param content_data A data frame produced by reddit_content command
#' @param plot A logical parameter indicating whether a graph is to be plotted or no (TRUE by default). Note that the root node corresponds to the thread itself rather than any of the comments.
#' @param write_to A character string specifying the path and file name where a graph object will be saved. The file must end with an extension such as *.gml. 
#' The following formats are allowed: edgelist, pajek, ncol,lgl, graphml, dimacs, gml, dot", leda.
#' @return A graph object
#' @export


construct_graph = function(content_data,plot=TRUE,write_to=NA){
  
  edges_frame = content_data[,c("id","structure")]
  if(sum(duplicated(edges_frame$structure))>0){
    stop("duplicates found in the structure variable -- make sure that content_data only contains a single thread")
  }
  
  edges_frame$structure = paste0("0_",as.character(edges_frame$structure))
  edges_frame = rbind(data.frame(id="0",structure="0",stringsAsFactors=F),edges_frame)
  
  
  A = B = edges_frame
  colnames(A) = c("from_id","from_structure")
  colnames(B) = c("to_id",  "to_structure")
  edges_frame = merge(A,B)
  edges_frame = edges_frame[sapply(1:nrow(edges_frame), 
                                   function(x)grepl(paste0("^",edges_frame$from_structure[x],"_\\d+$"),
                                                    edges_frame$to_structure[x])), c("from_id","to_id")]
  
  nodes_frame = content_data
  extra_row   = data.frame(id = 0, comment_score = 0, author=nodes_frame$author[1], controversiality = 0, stringsAsFactors=T)
  other_cols  = setdiff(colnames(nodes_frame),colnames(extra_row))
  cols_to_add = as.data.frame(matrix(NA,nrow=1,ncol=length(other_cols)))
  colnames(cols_to_add) = other_cols
  extra_row = cbind(extra_row,cols_to_add)
  nodes_frame = rbind(extra_row,nodes_frame)
  nodes_frame$start_node = ifelse(nodes_frame$id==0,1,0)
  nodes_frame = nodes_frame[,-which(colnames(nodes_frame)=="comment")] 
  # comments create problems when you export the file and try to read it with other software
  
  if(!is.na(write_to)){
    
    fmt = gsub(".*\\.(\\w+)$","\\1",write_to)
    
    if(!(fmt %in% c("edgelist", "pajek", "ncol","lgl", "graphml", "dimacs", "gml", "dot", "leda"))){
      stop("export_format must be either edgelist, pajek, ncol, lgl, graphml, dimacs, gml, dot, or leda. See igraph::write.graph for details.")
    }
    igraph::write.graph(igraph::graph.data.frame(edges_frame, directed = T, vertices = nodes_frame),write_to,format = fmt)
  }
  
  if(plot==T){
    
    
    colscale = function(x){
      if(min(x$comment_score)<0){
        min_col  = "firebrick2"
        mincolsq = seq(min(as.numeric(x$comment_score)), 0, length.out = ceiling(nrow(x)/2))
        maxcolsq = seq(0, max(as.numeric(x$comment_score)), length.out = ceiling(nrow(x)/2))
        if(nrow(x) %% 2 == 1){
          colsq  = c(mincolsq[-length(mincolsq)],maxcolsq[-1])
        } else{
          colsq  = c(mincolsq,maxcolsq)
        }
      } else{
        min_col = "ivory"
        colsq   = seq(0,max(as.numeric(x$comment_score)),length.out = nrow(x))
      }
      RP  = grDevices::colorRampPalette(c(min_col,"royalblue"))
      col = RP(nrow(x))[c(findInterval(x$comment_score,colsq,all.inside=T))]
      return(col)
    }
    
    g = igraph::graph.data.frame(edges_frame, directed = T, vertices = nodes_frame)
    plot(g,
         vertex.frame.color = ifelse(igraph::V(g)$controversiality==1,"red",NA),
         layout             = igraph::layout.reingold.tilford,
         vertex.label       = ifelse(igraph::V(g)$start_node==1,igraph::V(g)$author,igraph::V(g)$user),
         vertex.label.cex   = ifelse(igraph::V(g)$start_node==1,1,0.65),
         vertex.label.color = ifelse(igraph::V(g)$controversiality==1,"darkred","navy"),
         vertex.label.font  = ifelse(igraph::V(g)$author==igraph::V(g)$user,4,1),
         vertex.size        = ifelse(igraph::V(g)$start_node==1,20,12),
         vertex.color       = ifelse(igraph::V(g)$start_node==1,"orange2",colscale(nodes_frame)),
         edge.arrow.size    = 0.5,
         edge.color         = "skyblue2")
    graphics::title(content_data$title[1],cex.main=0.75,col.main="royalblue")
  }
  
  return(igraph::graph.data.frame(edges_frame, directed = T, vertices = nodes_frame))
  
}

