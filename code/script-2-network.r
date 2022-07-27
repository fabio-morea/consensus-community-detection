# Author: Fabio Morea @ Area Science Park
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package
#               as it contains persnoal information
#               Test data is random an contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/greencodes

# script 2: generate adjacency matrix and network

## debug mode
debug <- FALSE
echo <- TRUE

## load libraries
library(tidyverse)
library(igraph)
library(ggplot2)

#load the links to build the network,
links <- read_csv("./tmp/links.csv") %>% 
            select(date_start2,cf1,cf2,empl,ww) %>%
            rename(dtrans = date_start2) 

#print(links%>%head(500))


links %>% # weight is limited between 0 and 1
  mutate(weight = if_else(ww>1,1,ww))%>%
  relocate(cf1,cf2,weight)-> links

gdf <- graph.data.frame(links, directed=T)
adjm <- as_adjacency_matrix(gdf, attr = "ww",sparse = T)

g <- graph_from_adjacency_matrix(adjm, weighted=TRUE,mode='directed') 

igraph.options(vertex.size=2, 
               vertex.label=NA, 
               vertex.color="#29723e", 
               edge.size=1, 
               edge.color="#ffffff")

#as_long_data_frame(g) %>% write_csv("./results/graph_as_df.csv")

g %>% write_graph("./results/full_graph.csv", format="graphml")
#windows();plot(g, layout = layout_with_mds)

#Degree of the whole graph
degree_g  <- igraph::degree(g, mode = "all", normalized = FALSE)
V(g)$deg <- degree_g

 
filename <- "./img/figure1.png"
png(filename)

hist_degree<- ggplot(data.frame(degree_g), aes(degree_g)) +          
  geom_histogram(bins = 50,color = "black", fill = "green")+scale_y_log10()+
  theme_classic()

print(hist_degree)

dev.off()



print("Process completed, Check img folder for images and result folder for other files.")