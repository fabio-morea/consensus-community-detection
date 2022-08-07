# Author: Fabio Morea @ Area Science Park
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains personal information
#               Test data is contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network

# script 6: compare results of community detection
# the basic idea is that clusters should be more homogeneous than the whole network

## clear terminal
shell("cls")

## debug mode
debug <- FALSE
echo <- FALSE

## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)
library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)


source("./code/functions-cluster-attributes.R")

## load graph
print("Loading graph...")
g <- read_graph("./results/graph.csv", format="graphml")
gc <- induced.subgraph(g, V(g)[ CL0 == 1])
if (debug){
    gc <- induced.subgraph(gc,which(V(gc)$core>3))
    print("Debug mode")
    }
# undirected graph to be used for algorithms that do not support directed
gc_undirected <- as.undirected(g,mode = "each")

get.professional.groups <- function(g, cluster_name){
  prof_groups <- E(g)$group
  n <- length(prof_groups)
  summary <- 
    as.data.frame(table(prof_groups )) %>%
    mutate(rel_freq = Freq/n) %>%
    mutate(cl_name = cluster_name)
    print(summary)
  return(summary)
}

cl <- read_csv("./results/clusters_CONSENSUS.csv")
reference <- get.professional.groups(g, cluster_name="reference")

plot_list <- list()

for (i in 0:9){
  print(i)
  ci <- ( cl$mbshp == i )
  gi <- induced.subgraph(g, V(g)[ ci ])
  current <- get.professional.groups(gi, cluster_name="current")
  print(current)
  if (nrow(current)>0){
      filename <- paste0("community_",i,".pdf")
      print(filename)
      data <- bind_rows(current,reference)
      data<-data%>%
        select(-Freq)%>%
        pivot_wider(names_from=cl_name , values_from = rel_freq) %>%
        mutate(current = if_else(is.na(current), 0, current))%>%
        mutate(variation = (current/reference)-1)%>%
        arrange(variation)

      p <- ggplot(data, aes(x=prof_groups,y=variation)) + 
            geom_col() + 
            theme_light() + 
            ggtitle(paste("Community", i , " variation of professional groups"))
      
      plot_list[[i]] <- p
  }
}
plot_grob <- arrangeGrob(grobs=plot_list)
png("./results/figures/comm_variation_prof_group.png")
grid.arrange(plot_grob)
dev.off()

#
# list_all_names <- read_csv("./tmp/organisations.csv")
# list_all_CF <- read_csv("./results/clusters_CONSENSUS.csv")
# ## create the subgraph
# for (i in 1:10){  

#     CFs <- list_all_CF %>%
#       filter(mbshp == i)%>%
#       select(name) %>%
#       pull() 
#     selected <- (V(g)$name %in% CFs)
#     gi <- induced.subgraph(g,  selected )
#     #windows();plot(gi)

#     list_community_names <- list_all_names %>%
#       select(CF,az_ragione_soc)%>%
#       filter(CF %in% CFs) %>%
#       unique()
#     print(paste("CLUSTER", i))
#     print(nrow(list_community_names))
# }

print("Script completed.")




