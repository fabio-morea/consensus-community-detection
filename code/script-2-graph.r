# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package
#               as it contains persnoal information
#               Test data is random an contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-networks

# script 2: generate adjacency matrix and network

## debug mode
debug <- FALSE
echo <- TRUE

## load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(igraph)
library(ggplot2)
library(infotheo)
library (glue)

# histogram of graph degree
histogram.png <- function(data, filename){
    png(filename)
    fig <- data.frame(data) %>% 
      ggplot(aes(data)) +          
      geom_histogram(bins = 50,color = "black", fill = "#a2aca2")+
      scale_y_log10()+
      theme_classic()
    print(fig)
    dev.off()
    return(1)
}

#load the links to build the network,
links <- read_csv("./tmp/links.csv") %>% 
            select(cf1,cf2,ww,PG,qualif,LOC,sede_op_comune,NACE_group, sede_op_ateco) 
            
# weight is limited between 0 and maxWeight
maxWeight <- 3.0
links <- links %>% 
  mutate(weight = if_else(ww > maxWeight, maxWeight, ww))

g <- graph.data.frame(links, directed=T)

igraph.options( vertex.size=2, 
                vertex.label=NA, 
                vertex.color="#29723e",
                edge.size=1, 
                edge.arrow.size = 0.5,
                edge.color="gray",
                layout=layout_with_mds)

windows();plot(g)
    

# Edges' weights
print("Analysing weight...")
weight_g <- round(E(g)$weight,3)
histogram.png(weight_g, "./results/figures/figure_weight.png")
 
# Nodes' degree
print("Analysing degree...")
#degree_g_n  <- igraph::degree(g, mode = "all", normalized = TRUE)
degree_g    <- igraph::degree(g, mode = "all", normalized = FALSE)
V(g)$deg    <- degree_g
histogram.png(degree_g,   "./results/figures/figure_degree.png")

str_g    <- igraph::strength(g)
V(g)$str    <- str_g
histogram.png(str_g,   "./results/figures/figure_str.png")

# Coreness
print("Analysing coreness...")

coreness_g <- coreness(g) 
V(g)$core <-coreness_g  #coreness of the whole graph including smaller components

histogram.png(coreness_g,   "./results/figures/figure_coreness.png")

mut_inf <- mutinformation(coreness_g,degree_g, method="emp")
entr    <- sqrt(entropy(coreness_g) * entropy(degree_g) )
NMI     <- round(mut_inf/ entr,3)

scatterplot <- as_tibble_col(coreness_g) %>%
                add_column(degree_g) %>%
                mutate(coreness_g=value) %>%
                ggplot() + 
                geom_point(aes(y = degree_g, x = coreness_g))+
                theme_classic()+
                labs(title = "Comparison of degree and coreness of the full network",
                            subtitle = glue("Normalized Mutual Information ", NMI),
                            caption = glue("number of vertices: ",length(V(g))))

png("./results/figures/figure_scatterplot.png")
print(scatterplot)
dev.off()

# saving
print("Saving results...")
g %>% write_graph("./results/graph.csv", format="graphml")
as_long_data_frame(g) %>% write_csv("./results/graph_as_df.csv")



print("Graph completed and saved.")




# print(summary_table)
# summary_table <- 
#   as.data.frame.matrix(with(df_professions,table(vert,prof)))
# summary_table %>% write.csv("tmp.csv", quote=FALSE)

print("Script completed, please check results folder.")