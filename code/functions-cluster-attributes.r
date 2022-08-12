# functions-cluster-attributes

get.professional.groups <- function(g){
  profess_in_this_graph <- E(g)$group
  nn <- (length(profess_in_this_graph))
  summary_prof_groups <- 
    as.data.frame(table(profess_in_this_graph$group)) %>%
    mutate(rel_freq = Freq/nn) 
  return(summary_prof_groups)
}

scatter_strength_core <- function(g,gi){
    library(infotheo)
    data <- tibble(
            core =  as.integer(V(g)$core), 
            stre = as.integer(V(g)$str), 
            name = V(g)$name)%>%
            mutate(community = if_else(name %in% V(gi)$name, 1, 0))


    mut_inf <- mutinformation(data$core,data$stre, method="emp")
    entr    <- sqrt(entropy(data$core) * entropy(data$stre) )
    NMI     <- round(mut_inf/ entr,3)

   scatterplot <- ggplot(data) + theme_classic()+ 
              geom_point(aes(y = stre, x = core, 
                        colour = as.factor(community),
                        alpha = as.factor(community), 
                        size = as.factor(community)))+
                        scale_colour_manual(values=c("gray","red"))+
                        scale_shape_manual(values=c(1,19))+
                        scale_size_manual(values=c(3,5))+
                        scale_alpha_manual(values=c(.3,1.0))+
                        scale_x_continuous(breaks=seq(1,max(V(g)$core,1)))+
                        theme(panel.grid.major = element_line(color = "gray"))+
                        labs(title = "Comparison of strength and coreness",
                              subtitle = glue("Normalized Mutual Information ", NMI),
                              caption = glue("number of vertices: ",length(V(g))))

    return(scatterplot)
}