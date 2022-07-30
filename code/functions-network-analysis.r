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


show_subgraphs <- function( g, clusters_membership,nrows, ncols ) {

    nsubgraphs <- nrows*ncols
    windows()
    par(mfrow=c(nrows,ncols))

    cluster_summary <- clusters_membership%>% 
		as_tibble_col()%>%
		mutate(companies = V(g)$name)%>%
		group_by(value)%>%
		tally()%>%
		arrange(desc(n))
    
    list_clusters <- cluster_summary %>%
		filter(n>1) %>%
		unique() %>%
		arrange(-n) %>%
		pull(value) %>%
		head(nsubgraphs)
    
    for (i in list_clusters ){
		gi <- g %>% induced_subgraph(which(clusters_membership==i)) 
		plot(gi, 
			edge.color="gray",
			edge.width=E(gi)$weight/2,
      edge.arrow.size= E(gi)$weight/20,
			vertex.color=factor(V(gi)$core),
			vertex.label=NA,
			vertex.size=V(gi)$core,
			layout=layout.fruchterman.reingold) 

		text(x=0, y=1.3,  glue("Community ",i)  ,cex=1.0)
		text(x=0, y=1.2,  glue("number of nodes: ", length(V(gi)) ),cex=.8)
    }
}