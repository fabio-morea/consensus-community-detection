# Author: Fabio Morea @ Area Science Park
# a double circle layout
# cluster 0 is in the center,
# the first k elements are in the inner circle
# all other elements are in the outer circle

# libraries
options(warn=-1)
library(tidyverse)
library(igraph)

layout_concentric <- function(n,k,r){
	# generates a layout composed of a center (index == 0)
	# an inner circle of radius r/2 with the first k elements
	# and an outer circle of radius k with the remaining (n-k+1) elements

	# argument checking
	stopifnot (exprs = { 
		n > k
		k > 2 })
	# initialize
	alpha <- 0
	beta <- 0
	dalpha <- 2 * pi / (k)
	dbeta <- 2 * pi / (n-k)
	coords = tibble(x=0.0, y=0.0) %>% head(0)
	for (i in 0:n){
		if (i==0){
			coords <- coords %>% add_row(x=0, y=0)
		} else if (i <= k) {
			coords <- coords %>% add_row(x=0.5*r*cos(alpha), y=0.5*r*sin(alpha))
			alpha <- alpha + dalpha
		} else {
			coords <- coords %>% add_row(x=r*cos(beta), y=r*sin(beta))
			beta <- beta + dbeta	
		}
	}
	return(coords)
}

#coords <- layout_concentric (n = 24, k = 12, r = 3)

ggplot(layout_dc)+geom_point(aes(x=x,y=y))

clusters_graph <- read_graph("./results/_clusters_graph.csv", format="graphml")

length(V(clusters_graph)$name)

V(clusters_graph)$core <- graph.coreness(clusters_graph)
V(clusters_graph)$strength <- strength( clusters_graph, loops = FALSE) 
edgew <- (E(clusters_graph)$weight/max(E(clusters_graph)$weight)*100)
edgec <- ifelse(is.loop(clusters_graph), "#5759d800","#07d84d6d")
edgec <- ifelse(edgew > 1, edgec,"#ffffff00")# colour for weak links 
vertexs <- V(clusters_graph)$strength * 200 

coords <- layout_concentric (
	n = length(V(clusters_graph)$name)-1, 
	k = 6, 
	r = 1)

plot(clusters_graph,
		layout=as.matrix(coords),
		edge.color=edgec,
		edge.width=edgew,
		vertex.size=vertexs,
		vertex.color = "#04b0ff",
		vertex.label.font=1,
		vertex.label.color="black")

print("Done")