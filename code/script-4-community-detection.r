# Author: Fabio Morea @ Area Science Park
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains persnoal information
#               Test data is random an contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/greencodes 

# script 4: community detection

## clear terminal
shell("cls")


## debug mode
debug <- FALSE
echo <- TRUE

## load libraries
library(tidyverse)
library(igraph)

## load graph
print("Loading giant componente and making a graph...")
gc <- read_graph("./results/giant_component.csv", format="graphml")

## community detection using Edge Betweenneess algorithm
## The edge betweenness score of an edge measures the number of shortest paths through it, 
## The idea of the edge betweenness based community structure detection is that it is likely that edges connecting separate modules 
## have high edge betweenness as all the shortest paths from one module to another must traverse through them. 
## So if we gradually remove the edge with the highest edge betweenness score we will get a hierarchical map, a rooted tree, called a dendrogram of the graph. 
## The leafs of the tree are the individual vertices and the root of the tree represents the whole graph

## Weights of the edges is set to NA so E()$weight will not be used for community detection. 
## otherwise edges are interpreted as distances, not as connection strengths
## https://www.rdocumentation.org/packages/igraph/versions/1.3.2/topics/cluster_edge_betweenness
print("Community detection using Edge Betweenneess algorithm...")

clusters_eb <- cluster_edge_betweenness(gc, 
                         weights = NA,
                         directed = TRUE,
                         edge.betweenness = TRUE,
                         merges = TRUE,
                         bridges = TRUE,
                         modularity = TRUE,
                         membership = TRUE)
                           

cluster_summary <- clusters_eb$membership %>%
    as_tibble_col()%>%
    mutate(companies = clusters_eb$names)%>%
    group_by(value)%>%
    tally()%>%
    arrange(desc(n)) 

# membership stored in igraph object
gc$cl_eb <- clusters_eb$membership

# saving
print("Saving giant component and edge betweenneess membership...")
gc %>% write_graph("./results/gc_eb.csv", format="graphml")
as_long_data_frame(gc) %>% write_csv("./results/gc_eb_df.csv")

print("A summary of clusters by size")
print(cluster_summary)
  
windows();plot(sort(cluster_summary$n), main="Cluster size in the case of maximum modularity")
  
my_tab <- table(clusters_eb$membership)
#    as.data.frame() %>% 
#    arrange(desc(Freq)) %>%


print(my_tab)
windows();plot(my_tab[my_tab>3])

 
print("Script completed.")