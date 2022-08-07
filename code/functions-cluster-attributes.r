# functions-cluster-attributes

get.professional.groups <- function(g){
  profess_in_this_graph <- E(g)$group
  nn <- (length(profess_in_this_graph))
  summary_prof_groups <- 
    as.data.frame(table(profess_in_this_graph$group)) %>%
    mutate(rel_freq = Freq/nn) 
  return(summary_prof_groups)
}