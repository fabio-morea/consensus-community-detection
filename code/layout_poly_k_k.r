# Author: Fabio Morea @ Area Science Park
# each cluster is assigned layout_kawanda_kamai
# and translated to the vertex of a polygon
# cluster 0 is in the center,
# the first k elements are in the inner circle
# all other elements are in the outer circle

# libraries
options(warn=-1)
library(tidyverse)
library(igraph)
 
## load graph
print("Loading graph...")
g <- read_graph("./results/communities_consensus.csv", format="graphml")

print("Generate a poly_kk layout: each cluster is extracted, coordinates are calculated with layout_k_k then translated in polar coordinates to a polygon")
selected_clusters = c( 2,11,12,13,14,15)

for (i in selected_clusters) stopifnot(i %in% sort(unique(V(g)$CL1)))
g <- induced.subgraph(g, V(g)[ V(g)$CL1 %in% selected_clusters]) 
V(g)$labels <- seq(1:gorder(g))
		
g <- simplify(g,
		remove.multiple = TRUE,
		remove.loops = FALSE)

windows();plot(g, 
      vertex.color = as.factor(V(g)$CL1),
	  vertex.label=NA,
	  vertex.size = 4,
      layout = layout_with_kk)

d = 10.0
alpha <- 0
dalpha <- 2 * pi / (length(selected_clusters))

newlay <- tibble(labels = as.integer(1),x = 0.0, y = 0.0)%>% 
		head(0)

for (i in selected_clusters) {
	print(i)
	print(alpha)
	temp <- induced.subgraph(g, V(g)$CL1 == i)
	labels <- V(temp)$labels
	coords <- layout_with_kk(temp)
	coords[,1] <- coords[,1] + d * cos(alpha) #add here translation in polar coords
	coords[,2] <- coords[,2] + d * sin(alpha)
	alpha <- alpha + dalpha
	coords_tibble <- tibble(labels = labels, x = coords[,1], y = coords[,2])
	newlay <- rbind(newlay,coords_tibble)
	
}
coords <- newlay %>%
    	arrange(labels)%>%
		select(x,y) %>%
		as.matrix()
gorder(g)
windows();plot(g, 
      vertex.color = as.factor(V(g)$CL1),
	  vertex.label=NA,
	  vertex.size = sqrt(V(g)$str),
	  edge.width = E(g)$weight*4,
	  edge.color = "#00000072",
	  edge.arrow.size = 0,
      layout = coords)

print("Done")
