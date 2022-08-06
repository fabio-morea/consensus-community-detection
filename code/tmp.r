## load libraries
library(tidyverse)
library(readxl)
library(lubridate)  #dates
library(kableExtra) #tables
library(xtable)     #tables
 
## load functions
source("./code/parameters.R")
source("./code/functions-labour-market-data.R")
 
####################################################################à
# # update the preparation of Links with GROUP
# transitions.table <- read_csv("./tmp/transitions.csv")
# qualif_groups <- read_excel("./tmp/profess_groups.xlsx", sheet="prof_groups") %>%
#     rename(qualif = qualifica_codice )%>%
#     select(qualif,group)
# links <- transitions.table %>%
#     select(empl, cf1, cf2, date_end1, date_start2, gap, ww, qualif)%>% 
#     merge( qualif_groups, by="qualif")%>%
#     arrange(date_start2) %>%
#     mutate(yy = year(date_start2))%>% select(-date_start2,-date_end1)%>%
#     relocate(cf1,cf2,ww,group)
# links <- links %>%
#     filter(yy>=2014) %>% # transitions from 2013 registered in early 2014
#     filter(yy<=2021)     # not a complete year
# print(links%>%head(10))
# links %>% write_csv("./tmp/links.csv")

##### DONE ###############################################à






# print("Computing professional groups")
 

# prof_groups <- read_excel("./tmp/profess_groups.xlsx", sheet="prof_groups")

# list_profess <- read_csv("./tmp/contracts.csv") %>%
#   select(qualifica, qualifica_codice)%>%
#   mutate(q3=substring(qualifica_codice,1,5)) %>%
#   mutate(q5=substring(qualifica_codice,1,7)) %>%
#   unique() %>%
#   group_by(q3) %>%
#   arrange(q3,qualifica_codice) %>%
#   relocate(q3,q5,qualifica_codice)

# # list_profess %>% write.csv("./tmp/professions.csv",quote=FALSE,row.names=FALSE)
 
# print("Done")
