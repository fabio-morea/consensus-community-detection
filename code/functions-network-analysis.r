# Author: Fabio Morea @ Area Science Park, Trieste, Italy (www.areasciencepark.it)
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano,
# within the frame of PhD in Applied Data Science and Artificial Intelligence at University of Trieste
# Package: Consensus community Detection - version 1.0
# SPDX-License-Identifier: CC-BY-4.0
# GitHab repository: https://github.com/fabio-morea-areasciencepark/consensus-community-detection

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
 
}

 

#####################################################################################
show_subgraphs <- function( g, clusters_membership, nrows=1, ncols=3, label="" ) {

    nsubgraphs <- nrows*ncols
    V(g)$colorscale <- round(V(g)$CL1_p,1)
    V(g)$color <- '#c1bb77'
    V(g)$color[V(g)$colorscale == 0.7] <- '#fc91efcc'
    V(g)$color[V(g)$colorscale == 0.8] <- '#fb6a4acc'
    V(g)$color[V(g)$colorscale == 0.9] <- '#2638decc'
    V(g)$color[V(g)$colorscale == 1.0] <- '#24d51080'
    
    par(mfrow=c(nrows,ncols), mar=c(.01,.01,.01,.01))

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
        #vertex.color=factor(V(gi)$core),
        vertex.color = V(gi)$color,
        vertex.frame.color="#ffffffaa",
        vertex.label=NA,
        vertex.size=sqrt(V(gi)$deg)*3,
        layout=layout.graphopt) 

      text(x=0, y=1.0,  glue(label," Community ",i)  ,cex=1.5)
      text(x=0, y=0.9,  glue("number of nodes: ", length(V(gi)) ),cex=1.2)
    }
}


#####################################################################################
cluster_N_times <- function(g, clustering_algorithm, n_trials, alpha = 0 , res = NA,start = NA , epsilon = .0001) {
 
 #epsilon is a small but non null value to reduce the weight of selected edges
  results<-tibble(
    m=0.0,
    nnclust=as.integer(0),
    random_resolution=1.0)%>%
    head(0)

  all_clusters <- c()
  m_best<-99999
  for (i in 1:n_trials){
    if (echo) {print(paste("Trial", i))}
    
    applied_weights <- E(g)$ww 
    if (alpha > 0.0){
      n_items <- length(applied_weights)
      n_null <- as.integer(alpha * n_items)
      applied_weights[sample(n_items,n_null)] <- epsilon

    }
    if (clustering_algorithm=="Louvian"){ 
        random_resolution = as.numeric(sample(res, 1))
        cluster_tmp <- cluster_louvain(g, weights = applied_weights , resolution = random_resolution)
    }
    else if (clustering_algorithm=="Eigenvector"){
        cluster_tmp <- cluster_leading_eigen (g, weights = applied_weights , start=start )   
    }
    else{
        print("not supported") #cluster_tmp <- cluster_edge_betweenness(g)   
    }
    

    all_clusters <- cbind(all_clusters,cluster_tmp$membership)

    #identify cluster with best modularity
    m <- modularity (g,  cluster_tmp$membership)
    if(m<m_best){
        m_best<-m
        i_best=i
        best_clusters <- cluster_tmp 
    }

    nnclust <- max(best_clusters$membership)

    results <- results %>% add_row(m,nnclust) 
  }
  
  t=glue("Modularity - number of trials:", n_trials, " Algorithm:", clustering_algorithm)
  windows();hist(results$m, main=t)
  
  t=glue("Number of clusters - number of trials:", n_trials, " Algorithm:", clustering_algorithm)
  #windows();hist(results$nnclust, main=t)
  cluster_summary <- best_clusters$membership %>%
        as_tibble_col()%>%
        mutate(companies = best_clusters$names)%>%
        group_by(value)%>%
        tally()%>%
        arrange(desc(n)) 

  if (echo) {print(cluster_summary)}
  
  # print("Filtering results with modularity below median")
  # print(results$m < mean(results$m ))
  # clusters_mod_below_median <- all_clusters[ results$m < mean(results$m ) ]

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
  el <- get.edgelist(mygraph,names=FALSE)
  
  total <- numatts*numatts
  iii <-0
  for (i in 1:numatts) {
    for (j in 1:numatts) {
      iii <- iii+1
      if (echo) {print(paste("progress",round(iii/total*100,1),"%"))}
      if (i <= j){
        mm[i,j] <- length(which(apply(el,1,function(x) {
          get.vertex.attribute(mygraph, attrib, x[1] ) == attlist[i] && 
          get.vertex.attribute(mygraph, attrib, x[2] ) == attlist[j]  } )))
      }
      else{
         mm[i,j] <- mm[j,i]
      }

    }  
  }
 
  # convert to proportional mixing matrix if desired (ie by edge density)
  if (use.density) {mm <- mm/ecount(mygraph)}
  return(mm)
}