# Author: Fabio Morea @ Area Science Park
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains personal information
#               Test data is contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network

# script 5: consensus

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
print("Loading giant componente and making a graph...")
gc <- read_graph("./results/giant_component.csv", format="graphml")

if (debug){gc <- induced.subgraph(gc,which(V(gc)$core>5))}

# undirected graph to be used for algorithms that do not support directed
gc_undirected <- as.undirected(gc,mode = "collapse", edge.attr.comb = "sum")

## CONSENSUS
res=c(0.90,0.95,1.0,1.05,1.1) 
n_trials = 200
cl_louvian_N <- cluster_N_times(g=gc_undirected, 
    res=res,
    n_trials=n_trials, 
    min_cl_size = 2,
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

 

x<-x/N_trials#normalize
print(mean(x))
windows();heatmap(x)
windows();hist(x[x>0.0])
colnames(x)<-V(gc)$name
rownames(x)<-V(gc)$name

threshold = .5			# threshold to decide on membership
relevant_clusters = 0  	# counts only clusters with 3 or more members

consensus_clusters <- as_tibble_col(rownames(x))%>%
	mutate(membership=0)
more_clusers_to_be_found=TRUE

N_trials   <- ncol(all_clusters)

remaining_prob<-x

while (more_clusers_to_be_found){
  
  print(relevant_clusters)
  cluster_ii_members <- which(remaining_prob[1,] > threshold)
  clust_prob	<- remaining_prob[ cluster_ii_members, cluster_ii_members]
  remaining_prob<- remaining_prob[-cluster_ii_members,-cluster_ii_members]

  if(length(cluster_ii_members)>=2) {
    relevant_clusters <- relevant_clusters +1
    consensus_clusters$membership[cluster_ii_members]<- relevant_clusters
    print(paste("Cluster ", relevant_clusters, " has ", length(cluster_ii_members), " members"))
	
  }
  	if ( nrow(remaining_prob)  > 3)  {
		more_clusers_to_be_found=TRUE
	} 
  	else{
		more_clusers_to_be_found=FALSE
	}
}

as.data.frame(consensus_clusters)%>%
    write_csv("./results/clusters_CONSENSUS.csv")

#show_subgraphs (gc_undirected, clusters_membership=consensus_clusters$membership, nrows=2,ncols=4,label="Consensus Louvian" ) 

print("Script completed.")