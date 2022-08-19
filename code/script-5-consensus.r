# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
# Original data is not included in the package as it contains personal information
# Test data is contains no personal information

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
echo <- TRUE
debug <- FALSE
if (debug){print("Debug mode")}


## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)

source("./code/functions-network-analysis.R")

## load graph
print("Loading graph...")
g <- read_graph("./results/graph.csv", format="graphml")
g <- induced.subgraph(g, V(g)[ V(g)$CL0 == 1]) 

n_trials = 1000
if (debug){n_trials <- 50}

# undirected graph to be used for algorithms that do not support directed
gu <- as.undirected(g,mode = "each")

if (echo) print(paste("repeat clustering ", n_trials, "times ..."))
## CONSENSUS
# resolution is a relevant parameter to define the size of clusters
# alpha is used to induce a variability in the consensus procedure

res=c( 1.0, 1.2, 1.4, 1.6, 1.8 ) 
alpha = 0.05

all_clusters <- cluster_N_times(g=gu, 
	res=res,
	n_trials=n_trials, 
	alpha = alpha,
	clustering_algorithm="Louvian") 
 

#### TODO select only clusters with modularity below median 

as.data.frame(all_clusters,
 row.names = V(gu)$name ) %>% 
 write_csv("./results/clusters_N.csv")

# inspect and compare the clustering results
# in this case compare all N trials and calculate the probability that each company is in the same cluster of any other company
# Then select as a cluster only those who have a probability > threshold 50%

ncompanies <- nrow(all_clusters)
x <- matrix(0, nrow=ncompanies, ncol=ncompanies)
colnames(x)<-V(gu)$name
rownames(x)<-V(gu)$name

for (i in (1:n_trials)){
 	if (echo) print(paste("comparing cluster assignation ", i))
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

x<-x/n_trials #normalize

threshold = .5			 # threshold to decide on membership
current.cluster = 0 	

consensus_clusters <- as_tibble_col(rownames(x)) %>% mutate(membership=0)
more_clusers_to_be_found=TRUE
remaining <- x
min_vids <- 3 # counts only clusters with min_vids or more members

print("identify clusters above min_vids")
ccs <- tibble(name = "x", mbshp = -1, prob = 0.0) %>% head(0)

while (more_clusers_to_be_found){

	cluster_ii_members <- which(remaining[1, ] > threshold)
	selected <- remaining[cluster_ii_members, cluster_ii_members]
	remaining<- remaining[-cluster_ii_members, -cluster_ii_members]

	if(length(cluster_ii_members) >= min_vids) {
		current.cluster <- current.cluster + 1
		if(echo) print(paste("Processing cluster ", current.cluster, " with ", nrow(selected), "vertices"))
		
		for (j in 1:nrow(selected)) {
			selected[j,j]<- 0.0 #diagonal elements do not matter
			pp <- max(selected[j,])
			nn <- names(selected[1,])[j]
			ccs <- ccs %>% add_row(name=nn , mbshp=current.cluster, prob = pp) 
			print(paste("Adding vid ", nn,pp))
		}
	}
	else{
		if(echo){print(paste("a group below threshold ", current.cluster, " with ", length(cluster_ii_members), "vertices"))
	}
}
	if (nrow(remaining) > min_vids) {more_clusers_to_be_found=TRUE} 
	else{more_clusers_to_be_found=FALSE}
}
 
# from here on we are back to directed graph g (not gu!)
print("sorting cluster labels...")
V(g)$CL1 <- 0
cl_conv_table = as.data.frame(table(ccs$mbshp)) %>%
	rename(comm_size = Freq) %>%
	rename(cccc=Var1) %>%
	arrange(-comm_size)
 
cl_new_labels <- 1
for (i in cl_conv_table$cccc){
	selected_vids <- ccs %>%
	filter(mbshp == i) %>%
	select(name) %>%
	pull() %>%
	unlist()
	V(g)[ V(g)$name %in% selected_vids ]$CL1 <- cl_new_labels
	cl_new_labels <- cl_new_labels + 1
}

# print("Assign probability")

V(g)$CL1_p <- 0.0
for (i in 1:nrow(ccs)) V(g)[ccs[i,]$name]$CL1_p <- ccs[i,]$prob
windows();hist(V(g)$CL1_p)
 
print("Saving results")
ccs %>% write_csv("./results/clusters_consensus.csv")


# create a "community" object, standard igraph output
# clusters_consensus <- make_clusters(
# g,
# membership = V(g)$CL1,
# algorithm = "louvian consensus",
# merges = NULL,
# modularity = FALSE
# )
####

show_subgraphs (g, 
 clusters_membership = V(g)$CL1, 
 nrows=5,
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
vertexs<-V(clusters_graph)$strength * 200 

windows();plot(clusters_graph,
 layout=layout.graphopt,
 edge.color=edgec,
 edge.width=edgew,
 vertex.size=vertexs,
 vertex.color = "#04b0ff",
 vertex.label.font=1,
 vertex.label.color="black")


top_clusters <- V(clusters_graph)$name [1:10]
ggg <- induced.subgraph(clusters_graph,vids = top_clusters)
windows();plot( ggg,
 layout=layout.graphopt,
 edge.width = E(ggg)$weight / max(E(ggg)$weight)*10,
 vertex.size= strength(ggg)*100,
 vertex.color = "#04b0ff",
 vertex.label.font=1,
 vertex.label.color="black")

top_clusters <- V(clusters_graph)$name [1:10]
ggg <- induced.subgraph(clusters_graph,vids = top_clusters)
windows();plot( ggg,
 layout=layout.circle,
 edge.width = E(ggg)$weight / max(E(ggg)$weight)*10,
 vertex.size= strength(ggg)*100,
 vertex.color = "#04b0ff",
 vertex.label.font=1,
 vertex.label.color="black")

clusters_graph %>% write_graph("./results/_clusters_graph.csv", format="graphml")
as_long_data_frame(clusters_graph) %>% write_csv("./results/_clusters_graph_as_df.csv")
print("Script completed.")