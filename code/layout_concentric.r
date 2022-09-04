# Author: Fabio Morea @ Area Science Park
# a double circle layout
# cluster 0 is in the center,
# the first k elements are in the inner circle
# all other elements are in the outer circle

# libraries
options(warn=-1)
library(tidyverse)

library(igraph)

# 
#coords <- layout_concentric (n = 24, k = 12, r = 3)
#ggplot(coords)+geom_point(aes(x=x,y=y))
#clusters_graph <- read_graph("./results/_clusters_graph.csv", format="graphml")


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
			coords <- coords %>% add_row(x=2*r*cos(alpha), y=2*r*sin(alpha))
			alpha <- alpha + dalpha
		} else {
			coords <- coords %>% add_row(x=r*cos(beta), y=r*sin(beta))
			beta <- beta + dbeta	
		}
	}
	return(coords)
}

plot_concentric <- function(graph, 
		simplify_graph, 
		show_loops, 
		top_n_vids, 
		outer_circle,
		red_vertex){

	top_clusters <- V(clusters_graph)$name[1:top_n_vids]

	ggg <- induced.subgraph(graph,vids = top_clusters)
 
 	if(	simplify_graph){
		ggg <- simplify(ggg,
		remove.multiple = TRUE,
		remove.loops = FALSE,
		edge.attr.comb = "sum")
	}

	coords <- layout_concentric (
		n = length(V(ggg)$name)-1, 
		k = outer_circle, 
		r = 1)

	red_edges <- E(ggg)[from(red_vertex+1) | to(red_vertex+1)]
	edgec <- if_else(E(ggg) %in% red_edges, "red","#bebebe54")  

	vertexc <- if_else(V(ggg)$name == red_vertex, "red", "#bebebe1b")
	vertexs <- V(ggg)$strength * 300 

	V(ggg)$core <- graph.coreness(ggg)
	V(ggg)$strength <- strength( ggg, loops = FALSE) 
	edgew <- E(ggg)$weight/max(E(ggg)$weight)*10

	
	# weak edges color
	edgec <- if_else(edgew > .2, edgec,"#cccccc00")# colour for weak links 

	if (show_loops == FALSE){
		edgec <- if_else(is.loop(ggg), "#cccccc00",edgec)
	}

	plot(ggg,
			layout=as.matrix(coords),
			edge.color=edgec,
			edge.width=edgew,
			vertex.size=vertexs,
			vertex.color = vertexc,
			vertex.label.font=1,
			vertex.label.color="#3641e1")

} 

# plot_concentric(clusters_graph, 
# 	simplify_graph = TRUE, 
# 	show_loops=TRUE, 
# 	top_n_vids=30, 
# 		outer_circle = 5,
# 	red_vertex=1)

# plot_concentric(clusters_graph, 
# 	simplify_graph = TRUE, 
# 	show_loops=TRUE, 
# 	top_n_vids=30, 
# 	outer_circle = 5,
# 	red_vertex=2)

# windows();plot_concentric(clusters_graph, 
# 	simplify_graph = TRUE, 
# 	show_loops=TRUE, 
# 	top_n_vids=30, 
# 	outer_circle = 5,
# 	red_vertex=0)

# print("Done")
