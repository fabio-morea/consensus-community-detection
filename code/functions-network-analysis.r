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