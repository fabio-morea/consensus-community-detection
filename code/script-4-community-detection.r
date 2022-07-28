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
suppressPackageStartupMessages(library(tidyverse))
library(igraph)

source("./code/functions-network-analysis.R")

## load graph
print("Loading giant componente and making a graph...")
gc <- read_graph("./results/giant_component.csv", format="graphml")

if (debug){gc <- induced.subgraph(gc,which(V(gc)$core>3))}

## community detection using Edge Betweenneess algorithm *************************************************************
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

# membership stored in igraph object
V(gc)$cl_eb <- membership(clusters_eb)
gc <- delete_vertex_attr(gc, "id")

# saving
print("Saving giant component and edge betweenneess membership...")
gc %>% write_graph("./results/gc_eb.csv", format="graphml")
as_long_data_frame(gc) %>% write_csv("./results/gc_eb_df.csv")
tibble(membership(clusters_eb)) %>% write_csv("./results/clusters_eb.csv")

describe_communities(gc, clusters_eb)
print("EB completed.")

## community detection using Eigenvector algorithm  *************************************************************
print("Community detection using Eigenvector algorithm...")

clusters_ev <- cluster_leading_eigen (gc, 
  steps = -1,
  weights = NA,
  start = NULL,
  options = arpack_defaults,
  callback = NULL,
  extra = NULL,
  env = parent.frame)-> clusters  

# membership stored in igraph object
V(gc)$cl_ev <- membership(clusters_ev)

# saving
print("Saving giant component and eigenvector membership...")
gc %>% write_graph("./results/gc_ev.csv", format="graphml")
as_long_data_frame(gc) %>% write_csv("./results/gc_ev_df.csv")
tibble(membership(clusters_eb)) %>% write_csv("./results/clusters_ev.csv")

describe_communities(gc, clusters_ev)
print("EV completed.")

## community detection using Louvian algorithm  *************************************************************
print("Community detection using Louvian algorithm...")

gc_undirected <- as.undirected(gc,mode = "collapse", edge.attr.comb = "sum")
clusters_lv <- cluster_louvain(gc_undirected,  resolution = 1)

# membership stored in igraph object
V(gc)$cl_lv <- membership(clusters_lv)

# saving
print("Saving giant component and eigenvector membership...")
gc %>% write_graph("./results/gc_lv.csv", format="graphml")
as_long_data_frame(gc) %>% write_csv("./results/gc_lv_df.csv")
tibble(membership(clusters_lv)) %>% write_csv("./results/clusters_lv.csv")

describe_communities(gc, clusters_lv)
print("Louvian completed.")

## community detection using Leiden algorithm  *************************************************************
print("Community detection using Leiden algorithm...")

gc_undirected <- as.undirected(gc,mode = "collapse", edge.attr.comb = "sum")
clusters_ld <- cluster_leiden(gc_undirected,  resolution = 1)

# membership stored in igraph object
V(gc)$cl_ld <- membership(clusters_ld)

# saving
print("Saving giant component and eigenvector membership...")
gc %>% write_graph("./results/gc_ld.csv", format="graphml")
as_long_data_frame(gc) %>% write_csv("./results/gc_ld_df.csv")
tibble(membership(clusters_ld)) %>% write_csv("./results/clusters_ld.csv")

describe_communities(gc, clusters_ld)
print("Leiden completed.")

print("Script completed.")