# functions for network analysis

describe_communities <- function(g, clusters){
    print("Summary of communities by size")
    
    clmem <- tibble(membership(clusters)) 
    #print(table(clmem))

    #print(summary(clusters$membership))
    #boxplot(a ~ b, data = dataframe, xlab = "",  ylab = "", main = "")
    windows();hist(sizes(clusters))
    sizedist=sort(as.vector(sizes(clusters)))
    windows();plot(sizedist)

    windows();plot(clusters,g,
        vertex.size=2,
        vertex.label=NA
        )
}