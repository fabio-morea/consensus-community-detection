# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

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
echo <- TRUE

## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)

source("./code/functions-network-analysis.R")

## load graph
print("Loading graph...")
g <- read_graph("./results/graph.csv", format="graphml")
g <- induced.subgraph(g, V(g)[ CL0 == 1])

n_trials = 1000
if (debug){
    g <- induced.subgraph(g, which(V(g)$core>3))
    n_trials = 50
    print("Debug mode")
    }

# undirected graph to be used for algorithms that do not support directed
gu <- as.undirected(g,mode = "each")

print("consensus clustering...")
## CONSENSUS
res=c(0.90,0.95,1.0,1.05,1.1)


all_clusters <- cluster_N_times(g=gu, 
    res=res,
    n_trials=n_trials, 
    clustering_algorithm="Louvian")  

as.data.frame(all_clusters,
    row.names = V(gu)$name ) %>% 
    write_csv("./results/clusters_N.csv")

# inspect and compare the clustering results
# in this case compare all 200 trials and calculate the probability that each company is in the same cluster of any other company
# Then select as a cluster only those who have a probability > threshold 50%

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

 
#as.data.frame(x)%>%write_csv("./results/x.csv")

x<-x/N_trials#normalize
#print(mean(x))
#windows();heatmap(x)
windows();hist(x[x>0.0])
colnames(x)<-V(gu)$name
rownames(x)<-V(gu)$name

threshold = .5			    # threshold to decide on membership
current.cluster = 0  	# counts only clusters with 3 or more members

consensus_clusters <- as_tibble_col(rownames(x))%>%
	mutate(membership=0)
more_clusers_to_be_found=TRUE

N_trials   <- ncol(all_clusters)

remaining_prob<-x
min.points = 5

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
# from here on we are back to directed graph g (not gu!)
print("sorting cluster labels...")
V(g)$CL1 <- 0
cl_conv_table = as.data.frame(table(ccs$mbshp))%>%
    rename(comm_size = Freq)%>%
    rename(cccc=Var1)%>%
    arrange(-comm_size)
print(cl_conv_table)

cl_new_labels <- 1
for (i in cl_conv_table$cccc){
  print(paste(i,cl_new_labels))
  selected_vids <- ccs %>%
    filter(mbshp == i) %>%
    select(name)%>%
    pull()%>%
    unlist()
  V(g)[ V(g)$name %in% selected_vids ]$CL1 <- cl_new_labels
  cl_new_labels <- cl_new_labels + 1
}

print("Saving results")
tibble(name=V(g)$name,cluster=V(g)$CL1) %>% 
  write_csv("./results/clusters_consensus.csv")

# create a "community" object, standard igraph output
# clusters_lvC <- make_clusters(
#   g,
#   membership = V(g)$mmbrsp,
#   algorithm = "louvian consensus",
#   merges = NULL,
#   modularity = FALSE
# )

show_subgraphs (g, 
  clusters_membership = V(g)$CL1, 
  nrows=3,
  ncols=5,
  label="CL1" ) 

g %>% write_graph("./results/communities_consensus.csv", format="graphml")
as_long_data_frame(g) %>% write_csv("./results/communities_consensus_as_df.csv")

print("analysis...")

print("mixmat by clusters")
m <- mixmat(g,"CL1", use.density=TRUE )

print("heatmap by clusters")
mdf <- m %>% ### HEATMAP improve labels sorting
  as.data.frame() %>%
  rownames_to_column("from") %>%
  pivot_longer(-c("from"), names_to = "to", values_to = "weight") 

mdf %>% write_csv("mdf.csv")
mdf %>%
  ggplot(aes(x=from, y=to, fill=weight)) + 
  geom_raster() + scale_fill_gradient(low = "white", high = "black")
ggsave("./results/figures/heatmap_clusters.png")



clusters_graph <- graph_from_data_frame(mdf, directed = FALSE, vertices = NULL)
# TODO add vertices info in the daa frame
V(clusters_graph)$core <- graph.coreness(clusters_graph)
V(clusters_graph)$strength <- strength( clusters_graph, loops = FALSE) 

 

edgew = (E(clusters_graph)$weight/max(E(clusters_graph)$weight)*100)

edgec=ifelse(is.loop(clusters_graph), "#ffffff00","#07d84d6d")
edgec=ifelse(edgew > 1, edgec,"#ffffff00")# no colour for weak links  
print(edgew)
vertexs<-V(clusters_graph)$strength  * 200 

windows();plot(clusters_graph,
                layout=layout.graphopt,
                edge.color=edgec,
                edge.width=edgew,
                vertex.size=vertexs,
                vertex.color = "#04b0ff",
                vertex.label.font=1,
                vertex.label.color="black")

top_clusters <-  V(clusters_graph)$name [1:10]
ggg <- induced.subgraph(clusters_graph,vids = top_clusters)
windows();plot( ggg,
                layout=layout.circle,
                edge.width =  E(ggg)$weight / max(E(ggg)$weight)*50,
                vertex.color = "#04b0ff",
                vertex.label.font=1,
                vertex.label.color="black")

clusters_graph %>% write_graph("./results/_clusters_graph.csv", format="graphml")
as_long_data_frame(clusters_graph) %>% write_csv("./results/_clusters_graph_as_df.csv")
print("Script completed.")