# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains persnoal information
#               Test data is random an contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network

# script 3: analysing netwoek components

## debug mode
debug <- FALSE
echo <- TRUE

## load libraries
library(tidyverse)
library(igraph)

## load graph
print("Loading data...")
g <- read_graph("./results/graph.csv", format="graphml")



windows();plot(g,
     layout=layout_with_mds,
     edge.color="gray",
     edge.size=1,
     edge.arrow.size = 0.5,
     vertex.color="black",
     vertex.size=2,
     vertex.label=NA)

## identify components
print("Analysing components...")
V(g)$comp<-components(g)$membership

print(table(components(g)$membership))

# giant component
maxcomp<- which.max(table(components(g)$membership))

##plot giant component in red
print("Plotting full network in separate window...")
plot_title <- "Full network g"
windows();plot(g,
             edge.color="gray",
             edge.size = .1,
             edge.arrow.size = .1,
             vertex.color= "black",
             vertex.label=NA,
             vertex.size=2, 
             layout=layout_with_mds,)
title(main=plot_title,cex.main=1,col.main="black")

# plot only giant component
print("Plotting giant component in separate window...")
gg <- induced_subgraph(g, V(g)[ V(g)$comp == maxcomp ])

plot_title = "Giant component "
windows();plot(gg,
             edge.color="gray",
               edge.size = .1,
             edge.arrow.size = 0.2,
             vertex.color= if_else ( V(gg)$comp == maxcomp,"red","blue" ),
             vertex.label=NA,
             vertex.size=2, 
             layout=layout_with_mds)
title(main=plot_title,cex.main=1,col.main="black")

print("Plotting other components in separate window...")
oc <- induced_subgraph(g, V(g)[ V(g)$comp != maxcomp ])
plot_title = "Other components "
windows();plot(oc,
             edge.color="gray",
              edge.size = .1,
             edge.arrow.size = 0.2,
             vertex.color= if_else ( V(gg)$comp != maxcomp,"red","blue" ),
             vertex.label=NA,
             vertex.size=3, 
             layout=layout_with_graphopt)
title(main=plot_title,cex.main=1,col.main="black")




# number components in decreasing order and select only those above min_size
min_size <- 2
conversion_table <- as.data.frame(table(components(g)$membership))%>%
     rename(comp_size = Freq)%>%
     rename(comp_number=Var1)%>%
     arrange(-comp_size)%>%
     rownames_to_column("cluster_level_0")%>%
     mutate(cluster_level_0 = as.integer(cluster_level_0))%>%
     mutate(cluster_level_0 = if_else(comp_size > min_size, cluster_level_0, as.integer(0)))

assigned_comps <- as.data.frame(components(g)$membership)%>%
     rename(comp_number = "components(g)$membership")%>%
     merge(conversion_table,by="comp_number")

V(g)$CL0<-assigned_comps$cluster_level_0


print(table(assigned_comps$cluster_level_0))

# saving
print("Saving results...")
g %>% write_graph("./results/graph.csv", format="graphml")
as_long_data_frame(g)%>% write_csv("./results/graph_as_df.csv")

print("Process completed, please check results folder.")

 