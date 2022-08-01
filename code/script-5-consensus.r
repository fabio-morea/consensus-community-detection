# Author: Fabio Morea @ Area Science Park
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains personal information
#               Test data is contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network

# script 5: consensus

#A good clusteing algoritm in this domain should identify clusters that
#-	Have at leas min.point 
#-	All trivial clusters of 1 or 2 points are grouped in a cluster 0 FRINGE
#-	Each point is assigned with a PROBABILITY of being part of consensus cluster (as in fuzzy clustering) so we can identify points that are confidently placed in this cluster and points that are not. A result is a matrix of probabilities (N.points X N_clusters)
#- clusters have better ehomogeneity than the original network
#- high intra cluster connections, low inter cluster connections

#We use consensus, similar to random forrest
#-	Small perturbation of algoritm parameters and network (randomly exclude a fraction of elements with low coreness)
#-	Pairwise omparison


## clear terminal
shell("cls")

## debug mode
debug <- FALSE
echo <- FALSE

## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)

source("./code/functions-network-analysis.R")

## load graph
print("Loading giant component and making a graph...")
gc <- read_graph("./results/giant_component.csv", format="graphml")

if (debug){
    gc <- induced.subgraph(gc,which(V(gc)$core>3))
    print("Debug mode")
    }

# undirected graph to be used for algorithms that do not support directed
gc_undirected <- as.undirected(gc,mode = "collapse", edge.attr.comb = "sum")

print("consensus clustering...")
## CONSENSUS
res=c(0.90,0.95,1.0,1.05,1.1) 
n_trials = 500
cl_louvian_N <- cluster_N_times(g=gc_undirected, 
    res=res,
    n_trials=n_trials, 
    clustering_algorithm="Louvian")  

as.data.frame(cl_louvian_N,
    row.names = V(gc_undirected)$name ) %>% 
    write_csv("./results/clusters_N.csv")

# inspect and compare the clustering results
# in this case compare all 200 trials and calculate the probability that each company is in the same cluster of any other company
# Then select as a cluster only those who have a probability > threshold 50%

all_clusters <- cl_louvian_N ##DEBUG

ncompanies <- nrow(all_clusters)
N_trials   <- ncol(all_clusters)
x <- matrix(0, nrow=ncompanies, ncol=ncompanies)
for (i in (1:N_trials)){
  	nclusters <- max(all_clusters[,i])
    for (k in 1:nclusters) {
      samecluster <- (which(all_clusters[,i]==k))
      nc <- length(samecluster)
      for (t in 1:nc){
        for (j in 1:nc){ 
          x[samecluster[j],samecluster[t]] <- x[samecluster[j],samecluster[t]] +1
        }
      }
    }
}

 
as.data.frame(x)%>%write_csv("./results/x.csv")

x<-x/N_trials#normalize
#print(mean(x))
#windows();heatmap(x)
#windows();hist(x[x>0.0])
colnames(x)<-V(gc)$name
rownames(x)<-V(gc)$name

threshold = .5			    # threshold to decide on membership
current.cluster = 0  	# counts only clusters with 3 or more members

consensus_clusters <- as_tibble_col(rownames(x))%>%
	mutate(membership=0)
more_clusers_to_be_found=TRUE

N_trials   <- ncol(all_clusters)

remaining_prob<-x
min.points = 10

print("identify clusters above min.points")
ccs <- tibble(name = "x",mbshp = -1)%>%head(0)

while (more_clusers_to_be_found){
  cluster_ii_members <- which(remaining_prob[1,] > threshold)
  remaining_prob<- remaining_prob[-cluster_ii_members,-cluster_ii_members]

  if(length(cluster_ii_members)>=min.points) {
    current.cluster <- current.cluster + 1
    for (nn in names(cluster_ii_members)){
      ccs <- ccs %>% add_row(name=nn , mbshp=current.cluster)
    }
    if(echo){print(paste("Cluster ", current.cluster, " has ", length(cluster_ii_members), " members"))}
  }
  if ( nrow(remaining_prob)  > min.points)  {
    more_clusers_to_be_found=TRUE
  } 
  else{
    more_clusers_to_be_found=FALSE
  }
}

print("Saving results")
ccs %>% write_csv("./results/clusters_CONSENSUS.csv")
#save cluster info in graph structure
V(gc)$clust_cons <- 0
for(i in 1:nrow(ccs)) {
    mmbrsp <- as.numeric(ccs[[i,2]] )
    nname <- as.character(ccs[[i,1]] )
    V(gc)[V(gc)$name == nname]$clust_cons <- mmbrsp
    #print(paste(nname, mmbrsp))
}

clusters_lvC <- make_clusters(
  gc,
  membership = V(gc)$clust_cons,
  algorithm = "louvian consensus",
  merges = NULL,
  modularity = FALSE
)

show_subgraphs (gc_undirected, 
  clusters_membership = clusters_lvC$membership, 
  nrows=3,
  ncols=5,
  label="Consensus Louvian" ) 

gc %>% write_graph("./results/gc_communities_consensus.csv", format="graphml")
#as_long_data_frame(gc) %>% write_csv("./results/gc_communities_consensus_as_df.csv")

print("analysis...")
print("heatmap by sorted nodes")
sorted_nodes <- order(V(gc)$clust_cons)
#windows();heatmap(x,Rowv = sorted_nodes, Colv = sorted_nodes)

print("heatmap by clusters")
m <- mixmat(gc,"clust_cons", use.density=TRUE )
mdf <- m %>% ### HEATMAP DA RIVEDERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  as.data.frame() %>%
  rownames_to_column("from") %>%
  pivot_longer(-c("from"), names_to = "to", values_to = "weight") 

clusters_graph <- graph_from_data_frame(mdf, directed = FALSE, vertices = NULL)
# TODO add vertices info in the daa frame
V(clusters_graph)$core <- graph.coreness(clusters_graph)
V(clusters_graph)$strength <- strength( clusters_graph, loops = FALSE) 

edgew = (E(clusters_graph)$weight/max(E(clusters_graph)$weight)*100)

edgec=ifelse(is.loop(clusters_graph), "#ffffff00","#18128e89")
edgec=ifelse(edgew > 2, edgec,"#ffffff00")# no colour for weak links  
print(edgew)
vertexs<-V(clusters_graph)$strength  * 120 

windows();plot(clusters_graph,
                layout=layout.graphopt,
                edge.color=edgec,
                edge.width=edgew,
                vertex.size=vertexs,
                vertex.color = "#26b252",
                vertex.label.font=1,
                vertex.label.color="black")

#vertex.color=factor(V(clusters_graph)$strength),
#vertex.size=V(clusters_graph)$strength

clusters_graph %>% write_graph("./results/_clusters_graph.csv", format="graphml")
as_long_data_frame(clusters_graph) %>% write_csv("./results/_clusters_graph_as_df.csv")

mdf %>% write_csv("mdf.csv")
  
mdf %>%
  ggplot(aes(x=from, y=to, fill=weight)) + 
  geom_raster() + scale_fill_gradient(low = "white", high = "black")
ggsave("hmg.png")

print("Script completed.")