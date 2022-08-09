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
  print(i)
  gi <- induced.subgraph(g, V(g)[ V(g)$CL1 == i ])
  current <- get.professional.groups(gi, cluster_name="current")
  print(current)
  if (nrow(current)>0){
      # filename <- paste0("community_",i,".pdf")
      # print(filename)
      data <- bind_rows(current,reference)
      data<-data%>%
        select(-Freq)%>%
        pivot_wider(names_from=cl_name , values_from = rel_freq) %>%
        mutate(current = if_else(is.na(current), 0, current))%>%
        mutate(variation = ( current/reference) )%>%
        arrange(variation)

      p <- ggplot(data, aes(x=prof_groups,y=variation)) + 
            xlim(reference$prof_groups) +
            ylim(0,3)+
            geom_hline(yintercept=1.0, color = "red") +
            geom_col() + 
            theme_light() + 
            ggtitle(paste("Community", i , " variation of professional groups"))
      
      plot_list[[i]] <- p
  }
}

#all plots in the same png file
plot_grob <- arrangeGrob(grobs=plot_list)
png("./results/figures/comm_variation_prof_group.png")
grid.arrange(plot_grob)
dev.off()

#all plots in a pdf, one for each page
ggsave(filename = "./results/figures/comm_variation_prof_group.pdf", 
   plot = marrangeGrob(plot_list, nrow=1, ncol=1), 
   width = 10, height = 10)


print("Script completed.")




