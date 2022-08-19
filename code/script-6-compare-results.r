# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

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
echo <- FALSE
debug <- FALSE
if (debug){print("Debug mode")}

## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)
library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)
library(png)
library(grid)

source("./code/functions-cluster-attributes.R")

## load graph
print("Loading graph...")

g <- read_graph("./results/communities_consensus.csv", format="graphml")
g <- induced.subgraph(g, V(g)[ V(g)$CL0 == 1]) 
# clusters_consensus <- read_csv("./results/clusters_consensus.csv")
# add_clusters <- as.tibble(  V(g)$name) %>% 
#       rename(name = value) %>% 
#       left_join(clusters_consensus, by = "name")
# V(g)$CL1 <- add_clusters$cluster
# V(g)$CL1_p <- add_clusters$probability
 

org_names <- read_csv("./tmp/organisations.csv")%>%
      select(CF,az_ragione_soc) %>%
      distinct(CF, .keep_all = TRUE)
info_vids  <- tibble(CF = V(g)$name, core = V(g)$core, str=V(g)$str, p = V(g)$CL1_p) %>%
      inner_join(org_names, by="CF")
info_edges <- tibble(   comune = E(g)$sede_op_comune,   loc = E(g)$LOC, 
                        sector = E(g)$sede_op_ateco, nace   = E(g)$NACE_group,
                        qualif = E(g)$qualif,            pg = E(g)$PG)

reference_pg  <- get.professional.groups(g, cluster_name="reference_pg")
reference_loc <- get.locations(g, cluster_name="reference_loc")
reference_sec <- get.sectors(g, cluster_name="reference_sec")

min_cluster_size_to_plot <- 20
clusters_to_process <- sort(unique(V(g)$CL1))
if (debug){################# DEBUG ##############################################
      clusters_to_process <- clusters_to_process[1:10]
}  
 plot_list <- list()

for (i in clusters_to_process){
      gi <- induced.subgraph(g, V(g)[ V(g)$CL1 == i ])
      cl_size <- length(V(gi)$name)
      if (cl_size>min_cluster_size_to_plot){
            print(paste("processing cluster",i, "  size", cl_size))
            info_vids_gi  <- info_vids  %>% filter(V(g)$name %in% V(gi)$name)
            #info_edges_gi <- info_edges %>% filter(E(g) %in% E(gi))
            
      # row 1 title and network figure

            title_block <- ggplot() +  theme_void() +
                  annotate("text", x = 0, y = 10, label = " ")+ 
                  annotate("text", x = 0, y = 10, label = paste("Community ", i) , 
                        color="black", size=10 , angle=0, fontface="bold")+ 
                  annotate("text", x = 0, y = 9, label = paste("Size ", cl_size) , 
                        color="black", size=8 , angle=0, fontface="italic")+
                  annotate("text", x = 0, y = 0, label = " ")


            cluster_figure_name <- paste0("./tmp/commpnity",i,".png")
            png(cluster_figure_name, 600, 600)
            plot(gi, 
                  edge.color="gray",
                  edge.width=E(gi)$weight,
                  edge.arrow.size= E(gi)$weight,
                  vertex.color="red",
                  vertex.label=NA,
                  vertex.size=V(gi)$core,
                  layout=layout.graphopt) 
            dev.off()
            cluster_figure <- rasterGrob(png::readPNG(cluster_figure_name) )
            
            row1 <- ggarrange(title_block,cluster_figure, ncol = 2, labels = c(" ", "community"))

      # row 2 names and core vs strength ---------------------------------------------------------
      
            scatter_plot <- scatter_strength_core(g,gi)

            top_names <- info_vids_gi %>%
                  select(CF,az_ragione_soc, core, str, p)%>%
                  mutate(az_ragione_soc = substring(az_ragione_soc,1,40))%>%
                  arrange(-str) %>%
                  filter(core > 1) %>%   
                  distinct(CF,.keep_all = TRUE) %>%
                  head(20)
            table_names <- ggplot() +  theme_void() +
                  annotation_custom(tableGrob(top_names))
                  
            row2 <- ggarrange(table_names,scatter_plot, ncol = 2, 
                        labels = c("company names ", ""))

      # row 3 professional groups ------------------------------------------------
            current_pg <- get.professional.groups(gi, cluster_name="current_pg")
            data_pg <- bind_rows(current_pg,reference_pg)
            data_pg<-data_pg %>%
                  select(-Freq)%>%
                  pivot_wider(names_from=cl_name , values_from = rel_freq) %>%
                  mutate(current_pg = if_else(is.na(current_pg), 0, current_pg))%>%
                  mutate(variation =  round(current_pg/reference_pg,3)) %>%
                  arrange(-variation)

            figure_pg <- ggplot(data_pg) + theme_light() + 
                  geom_col(aes(x=prof_groups,y=variation, fill = "#6ccc6c")) + 
                  geom_hline(yintercept=1.0, color = "red") +
                  xlim(reference_pg$prof_groups) 
     
            table_pg <- ggplot() +  theme_void() +
                  annotation_custom(tableGrob(data_pg))

            row3<- ggarrange(table_pg,figure_pg)

      # row 4 locations --------------------------------------------------
            current_loc <- get.locations(gi, cluster_name="current_loc")
            data_loc <- bind_rows(current_loc,reference_loc)
            data_loc<-data_loc%>%
            select(-Freq)%>%
            pivot_wider(names_from=cl_name , values_from = rel_freq) %>%
            mutate(current_loc = if_else(is.na(current_loc), 0, current_loc))%>%
            mutate(variation = round( current_loc/reference_loc,3)) %>%
            arrange(-variation)
            
            figure_loc <- ggplot(data_loc) + theme_light() + 
                  geom_col(aes(x=locs,y=variation, fill = "#9a9ae5")) + 
                  geom_hline(yintercept=1.0, color = "red") +
                  xlim(reference_loc$loc) 

            table_loc <- ggplot() +  theme_void() +
                  annotation_custom(tableGrob(data_loc))

            row4<- ggarrange(table_loc,figure_loc)

      # row 5 sectors --------------------------------------------------
            current_sec <- get.sectors(gi, cluster_name="current_sec")
            data_sec <- bind_rows(current_sec,reference_sec)
            data_sec <- data_sec %>%
                  select(-Freq)%>%
                  pivot_wider(names_from=cl_name , values_from = rel_freq)%>%
                  mutate(current_sec = if_else(is.na(current_sec), 0, current_sec))%>%
                  mutate(variation = round( current_sec / reference_sec,3) )%>%
                  arrange(-variation)
            figure_sec <- ggplot(data_sec) + theme_light() + 
                  geom_col(aes(x=sectors ,y=variation, fill = "#e18bda")) + 
                  geom_hline(yintercept=1.0, color = "#ff0000") +
                  xlim(reference_sec$sectors) 

            table_sec <- ggplot() +  theme_void() +
                  annotation_custom(tableGrob(data_sec))

            row5<- ggarrange(table_sec,figure_sec)

      # page --------------------------------------------------
            if (echo) print(paste("adding to plot list ", i ))
            plot_list[[i+1]] <- ggarrange(row1, row2,row3, row4,row5,   nrow = 5 )
  }
}



#all plots in a pdf, one for each page
ggsave(filename = "./results/figures/comm_detection_results.pdf", 
   plot = marrangeGrob(plot_list, nrow=1, ncol=1), 
   width = 19, height = 29)

print("Script completed.")



