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
library(ggpubr)

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
            
links <- links %>% mutate(weight = 1) %>% select(cf1,cf2, weight)

g1 <- graph.data.frame(links, directed=T)
weight_g1 <- tibble(ww = round(E(g1)$weight,3))
hist1 <- ggplot(weight_g1,aes(x=ww)) + 
    geom_histogram( colour = "white", fill = "black" )+
        coord_cartesian (xlim = c ( 0, 2 ), ylim = c(0,2000)) +
        theme_light()+
        xlab("Weight") + ylab("number of vertices")+
        ggtitle("a) unweighted") 

links <- read_csv("./tmp/links.csv") %>% 
            select(cf1,cf2,ww,PG,qualif,LOC,sede_op_comune,NACE_group, sede_op_ateco) 
maxWeight <- 1.0
links <- links %>% 
  mutate(weight = if_else(ww > maxWeight, maxWeight, ww))%>% 
  select(cf1,cf2, weight,ww)
g2 <- graph.data.frame(links, directed=T)
weight_g2 <- tibble(ww = round(E(g2)$weight,3))
hist2 <- ggplot(weight_g2,aes(x=ww)) + 
    geom_histogram( colour = "white", fill = "black" )+
        coord_cartesian (xlim = c ( 0, 1 ), ylim = c(0,1000)) +
        theme_light()+
        xlab("Weight") + #ylab("number of vertices")+
        ggtitle("b) max weight = 1") 

links <- read_csv("./tmp/links.csv") %>% 
            select(cf1,cf2,ww,PG,qualif,LOC,sede_op_comune,NACE_group, sede_op_ateco) 
maxWeight <- 5.0
links <- links %>% 
  mutate(weight = if_else(ww > maxWeight, maxWeight, ww))%>% 
  select(cf1,cf2, weight,ww)
g3 <- graph.data.frame(links, directed=T)
weight_g3 <- tibble(ww = round(E(g3)$weight,3))
hist3 <- ggplot(weight_g3,aes(x=ww)) + 
    geom_histogram( colour = "white", fill = "black" )+
        coord_cartesian (xlim = c ( 0, 5 ), ylim = c(0,500)) +
        theme_light()+
        xlab("Weight") + #ylab("number of vertices")+
        ggtitle("c) max weight = 5") 

ggarrange(hist1, hist2, hist3,ncol = 3, nrow = 1)
ggsave("./results/figures/histograms3.png",dpi=150,width = 20, height = 7, units = "cm")
dev.off()

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

# Nodes' strength
print("Analysing strength...")
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
                add_column(str_g) %>%
                mutate(coreness_g=value) %>%
                ggplot(aes(y = str_g, x = coreness_g)) + 
                geom_point(size = 6, color = "#2eb25358")+
                theme_classic()+
                scale_x_continuous(breaks=seq(1,max(V(g)$core,1)))+
                theme(panel.grid.major = element_line(color = "gray"))+
                theme(aspect.ratio = 0.5)+
                labs(title = "Comparison of strength and coreness of the full network",
                            subtitle = glue("number of vertices: ",length(V(g))))
windows();plot(scatterplot)
ggsave("./results/figures/figure_scatterplot.png",
  width = 24, heigh = 12, units = "cm")
 

# scatterplot with shapes

shortlist <- read_csv("shortlist_orgs.csv")  
shortlist$az_ragione_soc

scp1 <- as_tibble_col(coreness_g) %>%
                add_column(str_g) %>%
				add_column(degree_g)%>%
				mutate(coreness = coreness_g, strength = str_g, degree = degree_g) %>%
                mutate(coreness_g=value) %>%
                add_column(CF = V(g)$name) %>%
                left_join(shortlist,by="CF") %>%
				mutate(type = replace_na(type,"other"))  %>%
				mutate(short_name = replace_na(short_name," other"))  
colnames(scp1)

windows();scp1 %>% 
	ggplot(aes(x = coreness, y = strength, shape = type, color = short_name)) + theme_classic()+
    geom_point(size = if_else(scp1$short_name==" other",3,6), alpha = 0.5)+
				scale_y_continuous(trans="log10")+
				scale_x_continuous(breaks=seq(1,max(V(g)$core,1)))+
				scale_color_manual(values=c("black", "green","purple","red","brown","#0066ff","#00aeff"))+
				scale_shape_manual(values=c(19,15,18))+
				guides(color = guide_legend(override.aes = list(size = 6)))+
				guides(shape = guide_legend(override.aes = list(size = 6)))+
                theme(panel.grid.major = element_line(color = "gray"))+
                theme(aspect.ratio = 0.5)+
                labs(title = "Comparison of coreness and strength of the network",
                            subtitle = glue("number of vertices: ",length(V(g))))


# windows();scp1 %>% 
# 	ggplot(aes( x = coreness, y = degree, shape = type, color = short_name)) + theme_classic()+
#     geom_point(size = if_else(scp1$short_name==" other",3,6), alpha = 0.5)+
# 				scale_y_continuous(trans="log10")+
# 				scale_x_continuous(breaks=seq(1,max(V(g)$core,1)))+
# 				scale_color_manual(values=c("black", "green","purple","red","brown","#0066ff","#00aeff"))+
# 				scale_shape_manual(values=c(19,15,18))+
# 				guides(color = guide_legend(override.aes = list(size = 6)))+
# 				guides(shape = guide_legend(override.aes = list(size = 6)))+
#                 theme(panel.grid.major = element_line(color = "gray"))+
#                 theme(aspect.ratio = 0.5)+
#                 labs(title = "Comparison of coreness and degree of the network",
#                             subtitle = glue("number of vertices: ",length(V(g))))
 
ggsave("./results/figures/figure_scatterplot1.png",
  width = 24, heigh = 12, units = "cm")

# saving
print("Saving results...")
links <- read_csv("./tmp/links.csv") %>% 
            select(cf1,cf2,ww,PG,qualif,LOC,sede_op_comune,LOC,NACE_group, sede_op_ateco) %>%
            mutate(qualif = substr(qualif,1,5))
E(g)$sede_op_comune <- links$sede_op_comune 
E(g)$LOC            <- links$LOC 
E(g)$sede_op_ateco  <- links$sede_op_ateco 
E(g)$NACE_group     <- links$NACE_group 
E(g)$qualif         <- links$qualif 
E(g)$PG             <- links$PG

g %>% write_graph("./results/graph.csv", format="graphml")
as_long_data_frame(g) %>% write_csv("./results/graph_as_df.csv")



print("Graph completed and saved.")


print("Number of nodes")
print(V(g))
 
print("Script completed, please check results folder.")

 


