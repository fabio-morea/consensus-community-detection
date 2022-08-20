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


show_clusters_AB <- function(a, b, order = 0, delta = 0, scale_vids=1, scale_edges=1){


      ids_only_A <- which( (V(g)$CL1 == a ))
      ids_only_B <- which( (V(g)$CL1 == b ))

      vids_A <- unique(names(unlist(ego(g, order=0, nodes = ids_only_A))))
      vids_B <- unique(names(unlist(ego(g, order=0, nodes = ids_only_B))))

      neib_A <- unique(names(unlist(ego(g, order=order, nodes = ids_only_A))))
      neib_B <- unique(names(unlist(ego(g, order=order, nodes = ids_only_B))))
      
  
      if(order == 0) {gg <- induced.subgraph(g, append(neib_A,neib_B))} else {gg <- induced.subgraph(g, intersect(neib_A,neib_B))}

      V(gg)$color <- "gray"
      V(gg)[V(gg)$name %in% intersect(neib_A, neib_B)]$color <- "green"
      V(gg)[V(gg)$name %in% vids_A]$color <- "blue"
      V(gg)[V(gg)$name %in% vids_B]$color <- "red"

      coords <- layout_(gg, with_fr(), normalize(xmin=-1,xmax=1,ymin=-1,ymax=1))

      for (i in 1:nrow(coords)) {
            if (V(gg)[i]$name %in% vids_A) {coords[i,1] <- coords[i,1] - delta}
            else if (V(gg)[i]$name %in% vids_B) {coords[i,1] <- coords[i,1] + delta}
            else if (V(gg)[i]$color == "gray") {coords[i,2] <- coords[i,2] - delta}
            
            if (abs(coords[i,2]) > delta ) {
                  coords[i,2] = coords[i,2]/delta # compact layout along vertical axis
            }
      }
      
      windows();plot(gg,
                  edge.color="#00000099",
                  edge.width=E(gg)$ww*scale_edges,
                  edge.arrow.size= delta/10,#E(gg)$ww*scale_edges*.2,
                  vertex.color= V(gg)$color,
                  vertex.frame.color = "black",
                  vertex.label= NA,
                  vertex.size=V(gg)$str*scale_vids,
                  layout= coords)
      
      
     
      }

   
 

show_clusters_AB(a=2,b=7, order=0 , delta = 1, scale_vids = .1)
show_clusters_AB(a=2,b=7, order=1 , delta = 2, scale_vids = .1)

show_clusters_AB(a=9,b=10, order=0 , delta = 1, scale_vids = .5)

show_clusters_AB(a=3,b=7, order=0 , delta = 1, scale_vids = .2, scale_edges=1)

  

      
print("Script completed.")



 
