# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains personal information
#               Test data is contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network

# script 4-1: resolution, modularity and cluster size distribution

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
g <- induced.subgraph(g, V(g)[ CL0 == 1])#giant component
if (debug){
    g <- induced.subgraph(g,which(V(g)$core>3))
    print("Debug mode")
    }
# undirected graph to be used for algorithms that do not support directed
gu <- as.undirected(g,mode = "each")

results <- tibble(resolution=0.0, nn=1, size=0.0)%>%head(0)
resolutions <- c(seq(0.6, 2.0, 0.1) )
for (res in resolutions){
    clusters_lv <- cluster_louvain(gu,  resolution = res)
    assigned_membership <- membership(clusters_lv)
    clusters <- as.integer(unname(table(assigned_membership)))%>%
            as_tibble_col() %>%
            rename(size = value)%>%
            filter(size > 5) %>%
            arrange (-size)%>%
            rownames_to_column()  %>%
            rename(nn = rowname)%>%
            mutate(nn = as.numeric(nn))%>%
            mutate(resolution=res)

     
    n_clusters = nrow(clusters)
    average_size = round(mean(clusters$size),2)

    results <- results %>% add_row(clusters)

    print(paste("Resolution: ", res, "non-trivial clusters: ", n_clusters, "average size", average_size))
    #print(modularity(clusters_lv))
}
  
 
figure<- ggplot(results)+
geom_line(aes(x=nn,y=size, group=resolution, col=resolution))+
geom_point(size=5, aes(x=nn,y=size, group=resolution, col=resolution))+
theme_light()+theme(aspect.ratio=0.71)+
facet_wrap(. ~ resolution, ncol = 4)
windows();plot(figure)
ggsave (file="./results/figures/figure_resolution.png", width=20, height=12, dpi=300)

print("Script completed.")