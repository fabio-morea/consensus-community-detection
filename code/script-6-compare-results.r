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
library(png)
library(grid)



source("./code/functions-cluster-attributes.R")

## load graph
print("Loading graph...")
g <- read_graph("./results/communities_consensus.csv", format="graphml")
g <- induced.subgraph(g, V(g)[ CL0 == 1])
if (debug){
    g <- induced.subgraph(gc,which(V(gc)$core>3))
    print("Debug mode")
    }


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

clusters <- 
    as.data.frame(table(V(g)$CL1 )) %>%
    rename(name = Var1)%>%
    rename(size = Freq)

 

reference <- get.professional.groups(g, cluster_name="reference")
plot_list <- list()

for (i in as.integer((clusters$name))){
  gi <- induced.subgraph(g, V(g)[ V(g)$CL1 == i ])
  cl_size <- length(V(gi)$name)

  if (cl_size>0){

      current <- get.professional.groups(gi, cluster_name="current")
      print(paste("processing cluster",i, "  size", cl_size, " numer of professional groups = ",nrow(current)))

      data <- bind_rows(current,reference)
      data<-data%>%
        select(-Freq)%>%
        pivot_wider(names_from=cl_name , values_from = rel_freq) %>%
        mutate(current = if_else(is.na(current), 0, current))%>%
        mutate(variation = ( current/reference) )%>%
        arrange(variation)


      row1 <- ggplot() +
            annotate("text", x = 0, y = 10, label = paste("Community ", i) , 
                      color="black", size=10 , angle=0, fontface="bold")+ 
            annotate("text", x = 0, y = 8, label = paste("Size ", cl_size) , 
                      color="black", size=8 , angle=0, fontface="italic")+ 
            theme_void()

      graph <- paste0("./tmp/commpnity",i,".png")
      png(figname, 600, 600)
      plot(gi, 
        edge.color="gray",
        edge.width=E(gi)$weight,
        edge.arrow.size= E(gi)$weight,
        vertex.color=factor(V(gi)$core),
        vertex.label=NA,
        vertex.size=V(gi)$core,
        layout=layout_with_kk) 
      dev.off()
      graph <- rasterGrob(png::readPNG(figname) )
      

      p1 <- ggplot(data, aes(x=prof_groups,y=variation)) + 
            xlim(reference$prof_groups) +
            ylim(0,3)+
            geom_hline(yintercept=1.0, color = "green") +
            geom_col() + 
            theme_light() + 
            ggtitle(paste("Community", i , " variation of professional groups"))
      
      row2 <- ggarrange(graph,p1, ncol = 2, labels = c("B", "C"))
      
      plot_list[[i]] <- ggarrange(row1, row2,  
            nrow = 6 )
 
  }
}

#all plots in the same png file
# plot_grob <- arrangeGrob(grobs=plot_list)
# png("./results/figures/comm_variation_prof_group.png")
# grid.arrange(plot_grob)
# dev.off()


#all plots in a pdf, one for each page
ggsave(filename = "./results/figures/comm_variation_prof_group.pdf", 
   plot = marrangeGrob(plot_list, nrow=1, ncol=1), 
   width = 19, height = 29)


print("Script completed.")




## ref. https://ggplot2-book.org/annotations.html