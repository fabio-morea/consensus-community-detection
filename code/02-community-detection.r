# Author: Fabio Morea @ Area Science Park, Trieste, Italy (www.areasciencepark.it)
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano,
# within the frame of PhD in Applied Data Science and Artificial Intelligence at University of Trieste
# Package: Consensus community Detection - version 1.0
# SPDX-License-Identifier: CC-BY-4.0
# GitHab repository: https://github.com/fabio-morea-areasciencepark/consensus-community-detection

# script 4: community detection

## clear terminal
shell("cls")

## debug mode
debug <- FALSE
echo <- TRUE

## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)
library(infotheo)
library(ggpubr)

source("./code/functions-network-analysis.R")

## load graph
print("Loading graph...")
g <- read_graph("./results/graph.csv", format="graphml")
g <- induced.subgraph(g, V(g)[ CL0 == 1])#giant component
if (debug){
    g <- induced.subgraph(g,which(V(g)$core>3))
    print("Debug mode")
    }
# undirected graph to be used for algorithms that do not support directed
g_undirected <- as.undirected(g,mode = "each")

## community detection using Louvian algorithm  *************************************************************
print("Community detection using Louvian algorithm...")
clusters_lv <- cluster_louvain(g_undirected,  resolution = 2.0)

# membership stored in igraph object
V(g)$cl_lv <- membership(clusters_lv)

# saving
if (echo){print("Saving Louvian membership...")}
tibble(membership(clusters_lv)) %>% write_csv("./results/clusters_lv.csv")
describe_communities(g_undirected, clusters_lv, "Louvian")
print("Louvian clustering completed.")
 

## saving results *************************************************************************************************
if (echo){print("Saving giant component with 4 different clusters membership...")}
g %>% write_graph("./results/communities.csv", format="graphml")
as_long_data_frame(g) %>% write_csv("./results/communities_df.csv")

## comparing results of different methods *************************************************************************
print("Summary of communities by size")

minpoints <- 2
non.trivial.communities <- cc %>% filter(c_sizes > minpoints)

figure<- ggplot(non.trivial.communities)+
    geom_line(aes(x=i,y=c_sizes, group=method, col=method))+
    geom_point(size=5, aes(x=i,y=c_sizes, group=method, col=method))+
    theme_light()+theme(aspect.ratio=0.71)+
    facet_grid(. ~ method )
windows();plot(figure)
ggsave (file="./results/figures/figure_comm_size.png", width=20, height=12, dpi=300)


# Louvain clustering depends on random initialisation
# we measure the differences over 1000 trials
# our reference is i_best the trial with highest moularity

# differences in clusters assignation are measured with NMI Normalized Mutual Information
# mutinformation takes two random variables as input 
# and computes the mutual information in nats according to the entropy estimator method. 
# If Y is not supplied and X is a matrix-like argument, the function returns a matrix of mutual information between all pairs of variables in the dataset X.

NMI=c()
all_clusters = c()
minpoints <- 2
n = 1000
results<-tibble(i = 1, m=0.0,n_clust=as.integer(0),NMI = 0.0)%>%head(0)
for (i in 1:n){
    clusters_tmp <- cluster_louvain(as.undirected(g),  resolution = 2.0)
    all_clusters<- cbind(all_clusters,clusters_tmp$membership)
    m <- modularity (g,  clusters_tmp$membership)
    size <- table(membership(clusters_tmp) )%>%
            sort( decreasing = TRUE)%>%
            unname() 
    non_trivial_clusters <- as.integer(length(size[ size > minpoints]))
    results <- results%>%add_row(i=i, m=m, n_clust=non_trivial_clusters )
}

#histogram of modularity
hm <- ggplot(results,aes(x=m)) + 
    geom_histogram( colour = "white", fill = "black",bins = 30)+
    xlab("modularity") +
    theme_light()

hc <- ggplot(results,aes(x=n_clust)) + 
    geom_histogram( colour = "white", fill = "black", binwidth = .5)+
    xlab("number of communities") +
    theme_light()

figure <- ggarrange(hm, hc,                    
             labels = c("a)", "b)"),
             font.label = list(size = 12, color = "black"),
             ncol = 2, nrow = 1)+
             theme(aspect.ratio=.4)

windows();plot(figure)
ggsave("./results/figures/modularity_nclusters.png")


i_best = which.max(results$m)
for (i in 1:n){
  mut_inf <- mutinformation(all_clusters[,i],all_clusters[,i_best], method="emp")
  entr    <- sqrt(entropy(all_clusters[,i_best]) * entropy(all_clusters[,i_best]) )
  NMI     <- mut_inf/ entr
  results$NMI[i] <- NMI
}

#histogram of modularity
hnmi <- ggplot(results,aes(x=NMI)) + 
    geom_histogram( colour = "white", fill = "green",binwidth=0.01)+
        #geom_density()+
        theme_light()+coord_flip()

qqdata <- as.data.frame(qqplot(results$m, results$NMI,plot.it=FALSE)) %>%
    rename(NMI = y) %>%
    rename(modularity = x)
qq <- ggplot(qqdata,aes(x=modularity, y = NMI)) + geom_point(size = 3, colour = "green")+
    theme_light()
plot(qq)

figure <- ggarrange(qq,hnmi,
            labels = c("Q-Qplot modularity-NMI", "Histogram"),
            ncol = 2, nrow = 1)+ theme(aspect.ratio=.4)
plot(figure)
ggsave("./results/figures/QQ-NMI.png")
print("Script completed.")
