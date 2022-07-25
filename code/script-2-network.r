# Author: Fabio Morea @ Area Science Park
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains persnoal information
#               Test data is random an contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/greencodes 

# script 2: generate adjacency matrix and network

## debug mode
debug <- TRUE
echo <- TRUE

## load libraries
library(tidyverse)
library(igraph)

#load the links to build the network,
links <- read_csv("./tmp/links.csv")%>% 
            select(date_start2,cf1,cf2,empl,ww) %>%
            rename(dtrans = date_start2) 

print(links%>%head(500))


links %>% # weight is limited between 0 and 1
  mutate(weight = if_else(ww>1,1,ww))%>%
  relocate(cf1,cf2,weight)-> links

gdf <- graph.data.frame(links, directed=T)
adjm <- as_adjacency_matrix(gdf, attr = "ww",sparse = T)

g <- graph_from_adjacency_matrix(adjm, weighted=TRUE,mode='directed') 

hist(E(g)$weight, breaks=100)

print("Process completed")