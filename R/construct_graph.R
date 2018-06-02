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
