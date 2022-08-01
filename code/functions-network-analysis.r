# functions for network analysis


#####################################################################################
community.size <- function(clusters, mm){
  c_sizes <- table(membership(clusters) )%>%
            sort( decreasing = TRUE)%>%
            unname()

  c_sizes <- tibble(c_sizes) %>% 
    mutate(method=mm)%>%
    mutate(i = 1:n())

  return(c_sizes)
}


#####################################################################################
describe_communities <- function(g, clusters,mm){
    print(glue("Results of community detection by ", mm, " algorithm"))
    sizedist=sort(as.vector(sizes(clusters)))
    print(table(sizedist))
    #windows();plot(sizedist)
    
    #print(table(membership(clusters)))

    #print(summary(clusters$membership))
    #boxplot(a ~ b, data = dataframe, xlab = "",  ylab = "", main = "")
    #windows();hist(sizes(clusters))
    


    #windows();
    #plot(clusters,g,vertex.size=2,vertex.label=NA,layout=layout.fruchterman.reingold,main = mm )
}

# TODO improve as per https://stackoverflow.com/questions/18250684/add-title-and-legend-to-igraph-plots


#####################################################################################
show_subgraphs <- function( g, clusters_membership, nrows=1, ncols=3, label="" ) {

    nsubgraphs <- nrows*ncols
    windows()
    par(mfrow=c(nrows,ncols))

    cluster_summary <- clusters_membership%>% 
		as_tibble_col()%>%
		mutate(companies = V(g)$name)%>%
		group_by(value)%>%
		tally()%>%
		arrange(desc(n))
    
    list_clusters <- cluster_summary %>%
		filter(n>1) %>%
		unique() %>%
		arrange(-n) %>%
		pull(value) %>%
		head(nsubgraphs)
    
    for (i in list_clusters ){
      gi <- g %>% induced_subgraph(which(clusters_membership==i)) 
      plot(gi, 
        edge.color="gray",
        edge.width=E(gi)$weight/2,
        edge.arrow.size= E(gi)$weight/20,
        vertex.color=factor(V(gi)$core),
        vertex.label=NA,
        vertex.size=V(gi)$core,
        layout=layout.graphopt) 

      text(x=0, y=1.3,  glue(label," Community ",i)  ,cex=1.0)
      text(x=0, y=1.1,  glue("number of nodes: ", length(V(gi)) ),cex=.8)
    }
}


#####################################################################################
cluster_N_times <- function(g, res, n_trials, clustering_algorithm) {
  results<-tibble(
    m=0.0,
    nnclust=as.integer(0),
    random_resolution=1.0)%>%head(0)
  all_clusters <- c()
  m_best<-99999
  for (i in 1:n_trials){
    if (clustering_algorithm=="Louvian"){ 
        random_resolution = as.numeric(sample(res, 1))
        cluster_tmp <- cluster_louvain(g,  resolution = random_resolution)
      }
    else if (clustering_algorithm=="Leiden"){
        cluster_tmp <- cluster_leiden(g,  resolution = res)    
    }
    else{
        cluster_tmp <- cluster_edge_betweenness(g)   
    }
    all_clusters<- cbind(all_clusters,cluster_tmp$membership)
    m <- modularity (g,  cluster_tmp$membership)
    if(m<m_best){
        m_best<-m
        i_best=i
        best_clusters <- cluster_tmp 
    }

    nnclust <- max(best_clusters$membership)

    results <- results %>% 
        add_row(m,nnclust,random_resolution )
  }
  
  t=glue("Modularity - number of trials:", n_trials, " Algorithm:", clustering_algorithm)
  #hist(results$m, main=t)
  
  t=glue("Number of clusters - number of trials:", n_trials, " Algorithm:", clustering_algorithm)
  #windows();hist(results$nnclust, main=t)
  cluster_summary <- best_clusters$membership %>%
        as_tibble_col()%>%
        mutate(companies = best_clusters$names)%>%
        group_by(value)%>%
        tally()%>%
        arrange(desc(n)) 

  if (echo) {print(cluster_summary)}

  return(all_clusters)
}



#####################################################################################
mixmat <- function(mygraph, attrib, use.density=TRUE) {
 
  require(igraph)
  # get unique list of characteristics of the attribute
  attlist <- sort(unique(get.vertex.attribute(mygraph,attrib)))
  numatts <- length(attlist)
  # build an empty mixing matrix by attribute
  mm <- matrix(nrow=numatts,
               ncol=numatts,
               dimnames=list(attlist,attlist))
 
  # calculate edge density for each matrix entry by pairing type
  # lends itself to parallel if available
  el <- get.edgelist(mygraph,names=FALSE)
   
  for (i in 1:numatts) {
    for (j in 1:numatts) {
      
      tmp <- length(which(apply(el,1,function(x) {
          get.vertex.attribute(mygraph, attrib, x[1] ) == attlist[i] && 
          get.vertex.attribute(mygraph, attrib, x[2] ) == attlist[j]  } )))

      mm[i,j] <- tmp
    }  
  }
 
  # convert to proportional mixing matrix if desired (ie by edge density)
  if (use.density) mm/ecount(mygraph) else mm
}