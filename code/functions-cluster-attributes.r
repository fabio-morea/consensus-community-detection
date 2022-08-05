# functions-cluster-attributes


transitions.table <- read_csv("./tmp/transitions.csv")

links <- transitions.table %>%
    select(empl, cf1, cf2, date_end1, date_start2, gap, ww, qualif)%>% 
    mutate(q5=substring(qualif,1,7))%>%
    mutate(q3=substring(qualif,1,5))%>%
    unique()%>%
    arrange(date_start2) 

links %>% write_csv("./tmp/links.csv")