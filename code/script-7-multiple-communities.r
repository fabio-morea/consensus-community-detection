# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains personal information
#               Test data is contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network

# script 7: compare multiple communities
 
## clear terminal
shell("cls")

## debug mode
echo <- FALSE
debug <- FALSE
if (debug){print("Debug mode")}

## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)
library(tidyverse)

##source("./code/functions-cluster-attributes.R")

## load graph
print("Loading graph...")

g <- read_graph("./results/communities_consensus.csv", format="graphml")
g <- induced.subgraph(g, V(g)[ V(g)$CL0 == 1]) 

org_names <- read_csv("./tmp/organisations.csv")%>%
      select(CF,az_ragione_soc) %>%
      distinct(CF, .keep_all = TRUE)

info_vids  <- tibble(CF = V(g)$name, core = V(g)$core, str=V(g)$str, p = V(g)$CL1_p) %>%
      inner_join(org_names, by="CF")

info_edges <- tibble(   comune = E(g)$sede_op_comune,   loc = E(g)$LOC, 
                        sector = E(g)$sede_op_ateco, nace   = E(g)$NACE_group,
                        qualif = E(g)$qualif,            pg = E(g)$PG)


show_clusters_AB <- function(title="", a, b, order = 0, delta = 0, scale_vids=1, scale_edges=1){

# a=1
# b=2
# order = 1
# delta = 0.4
# scale_vids=1.5
# scale_edges=2

      ids_only_A <- which( (V(g)$CL1 == a ))
      ids_only_B <- which( (V(g)$CL1 == b ))

      vids_A <- unique(names(unlist(ego(g, order=0, nodes = ids_only_A))))
      vids_B <- unique(names(unlist(ego(g, order=0, nodes = ids_only_B))))

      neib_A <- unique(names(unlist(ego(g, order=order, nodes = ids_only_A))))
      neib_B <- unique(names(unlist(ego(g, order=order, nodes = ids_only_B))))
      
  
      if(order == 0) {gg <- induced.subgraph(g, append(neib_A,neib_B))} else {gg <- induced.subgraph(g, intersect(neib_A,neib_B))}
       
      V(gg)$color <- "gray"
      V(gg)[V(gg)$name %in% intersect(neib_A, neib_B)]$color <- "#fff70080"
      V(gg)[V(gg)$name %in% vids_A]$color <- "#0000ff80"
      V(gg)[V(gg)$name %in% vids_B]$color <- "#ff000080"

      coords <- layout_(gg, with_kk(), normalize(xmin=-1,xmax=1,ymin=-1,ymax=1))

      # expand X axis proportional to coreness
      # core <- coreness(gg)
      # V(gg)$dd <- order + (core/max(core)) * delta 

      # expand X axis proportional to probability
      V(gg)$dd <- (V(gg)$CL1_p - 0.6)*delta #+ order 
 
      mean_y_A = mean(coords[V(gg)$name %in% vids_A,2])
      mean_y_B = mean(coords[V(gg)$name %in% vids_B,2])

      for (i in 1:nrow(coords)) {
            dd <- V(gg)[i]$dd
            if (V(gg)[i]$name %in% vids_A) {
                  coords[i,1] <- -abs(coords[i,1]) - dd 
                  coords[i,2] <- coords[i,2]-mean_y_A
            }
            else if (V(gg)[i]$name %in% vids_B) {
                  coords[i,1] <- abs(coords[i,1]) + dd 
                  coords[i,2] <- coords[i,2]-mean_y_B
            }
            else{
                  coords[i,2] <- abs(coords[i,2]) - 5.0 *delta 
            }
                        
            if (abs(coords[i,2]) > 1 ) {
                  coords[i,2] = 1 / coords[i,2]  # compact layout along vertical axis
            }
      }
      

     
         windows();plot(gg,
                  edge.color="#00000048",
                  edge.width=E(gg)$ww*scale_edges,
                  edge.arrow.size= delta/20, 
                  vertex.color= V(gg)$color,
                  vertex.frame.color = "white",
                  vertex.label= NA,
                  vertex.size=sqrt(V(gg)$str)*scale_vids,
                  layout= coords)
      
      
     
}

   
 

show_clusters_AB(a=2,b=3, order=0 , delta = 2, scale_vids = 1)

show_clusters_AB(a=14,b=2, order=0 , delta = 1, scale_vids = 1)
show_clusters_AB(a=2,b=3, order=1 , delta = 1, scale_vids = 1)


# show_clusters_AB(a=2,b=7, order=1 , delta = 2, scale_vids = 2)
# show_clusters_AB(a=9,b=10, order=0 , delta = 2, scale_vids = 5)
# show_clusters_AB(a=3,b=7, order=0 , delta = 3, scale_vids = 3, scale_edges=1)
# show_clusters_AB(a=3,b=7, order=1 , delta = 3, scale_vids = 3, scale_edges=1)
 
   
source('./code/layout_concentric.r')

clusters_graph <- read_graph("./results/_clusters_graph.csv", format="graphml")


plot_concentric(clusters_graph, 
	simplify_graph = TRUE, 
	show_loops=TRUE, 
	top_n_vids=10, 
	outer_circle = 5,
	red_vertex=1)

plot_concentric(clusters_graph, 
	simplify_graph = TRUE, 
	show_loops=FALSE, 
	top_n_vids=10, 
	outer_circle = 6,
	red_vertex=2)


print("Script completed.")



 
