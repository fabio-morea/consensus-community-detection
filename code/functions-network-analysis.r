# functions for network analysis


community.size <- function(clusters, mm){
  c_sizes <- table(membership(clusters) )%>%
            sort( decreasing = TRUE)%>%
            unname()

  c_sizes <- tibble(c_sizes) %>% 
    mutate(method=mm)%>%
    mutate(i = 1:n())

  return(c_sizes)
}


describe_communities <- function(g, clusters,mm){
  print(paste("describing clusters obtained by ", mm))

    #clmem <- tibble(membership(clusters)) 
    #print(table(clmem))

    #print(summary(clusters$membership))
    #boxplot(a ~ b, data = dataframe, xlab = "",  ylab = "", main = "")
    #windows();hist(sizes(clusters))
    #sizedist=sort(as.vector(sizes(clusters)))
    #windows();plot(sizedist)

    windows();plot(clusters,g,
        vertex.size=2,
        vertex.label=NA,
        layout=layout.fruchterman.reingold,
        main = mm
        )
}

# TODO improve as per https://stackoverflow.com/questions/18250684/add-title-and-legend-to-igraph-plots


show_subgraphs <- function( g, clusters_membership ) {
    windows()
  
    par(mfrow=c(2,2))


    cluster_summary <- clusters_membership%>% 
      as_tibble_col()%>%
      mutate(companies = V(g)$name)%>%
      group_by(value)%>%
      tally()%>%
      arrange(desc(n))
    
    top_clusters_n <- cluster_summary%>%
      filter(n>=2)%>%
      pull(value)
    
    top_clusters_n <- 4
    
    for (i in unique(top_clusters_n) ){
        gi <- g %>% induced_subgraph(which(clusters_membership==i)) 
        add_graph = FALSE
        if (length(V(gi))> 2){
            plot(gi, 
                add=add_graph, # add more graphs in the same figure
                edge.color="gray",
                edge.width=E(gi)$weight/3,
                vertex.color=factor(V(gi)$prov),
                vertex.label=NA,
                vertex.size=V(gi)$core,
                layout=layout_with_kk) 

            text(x=-2, y=1.2,  glue("induced subgraph of cluster ",i)  ,cex=1.2)
            text(x=-2, y=1.0,  glue("number of nodes: ", length(V(gi)) ),cex=.7)
            add_graph=TRUE
        }
    }
}