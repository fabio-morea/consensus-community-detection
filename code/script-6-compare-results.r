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


source("./code/functions-cluster-attributes.R")

## load graph
print("Loading graph...")
g <- read_graph("./results/graph.csv", format="graphml")
if (debug){
    g <- induced.subgraph(g,which(V(g)$core>3))
    print("Debug mode")
    }

list_profess <- read_csv("./tmp/contracts.csv") %>%
  select(qualifica, qualifica_codice)%>%
  mutate(q3=substring(qualifica_codice,1,5)) %>%
  mutate(q5=substring(qualifica_codice,1,7)) %>%
  unique() %>%
  group_by(q3) %>%
  arrange(q3,qualifica_codice) %>%
  relocate(q3,q5,qualifica_codice)

list_profess %>% write.csv("./tmp/professions.csv",quote=FALSE,row.names=FALSE)

prof_groups <- read_excel("./tmp/profess_groups.xlsx", sheet="prof_groups")

profess_in_this_graph <- E(g)$qualif
print(profess_in_this_graph)

prof_groups <- prof_groups %>%
   filter(qualifica_codice %in%  profess_in_this_graph)
print(prof_groups)
nn <- (length(profess_in_this_graph))

summary_prof_groups <- 
  as.data.frame(table(prof_groups$group)) %>%
  mutate(rel_freq = Freq/nn) 

print(summary_prof_groups)

summary_prof_groups %>%
  ggplot() + geom_col(aes(x=Var1,y=rel_freq)) + theme_light()
ggsave("./results/figures/fig_professions.png")

prof_groups %>% write_csv("./results/prof_gropus_whole_graph.csv")

list_all_names <- read_csv("./tmp/organisations.csv")
list_all_CF <- read_csv("./results/clusters_CONSENSUS.csv")
## create the subgraph
for (i in 1:10){  

    CFs <- list_all_CF %>%
      filter(mbshp == i)%>%
      select(name) %>%
      pull() 
    selected <- (V(g)$name %in% CFs)
    gi <- induced.subgraph(g,  selected )
    #windows();plot(gi)

    list_community_names <- list_all_names %>%
      select(CF,az_ragione_soc)%>%
      filter(CF %in% CFs) %>%
      unique()
    print(paste("CLUSTER", i))
    print(nrow(list_community_names))
}

print("Script completed.")
