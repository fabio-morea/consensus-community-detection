# main

## load libraries
library(tidyverse)
library(lubridate)  #dates
library(kableExtra) #tables
library(xtable)     #tables
library(igraph)

## load functions
source("./code/parameters.R")
source("./code/functions-labour-market-data.R")

path <- get.data.folder.path()

columns <- c(   "id_cittadino","data_nascita",  "iso3","genere",
                "data_inizio","data_fine","tipo_contratto","qualifica_2_digit",
                "qualifica","qualifica_codice","SLL_codice", "comune_istat", 
                "professione", "saldo")

data <- load_data(path, columns, debug=TRUE)  
 
## filter by profession
# 2.1 - Specialisti in scienze matematiche, informatiche, chimiche, fisiche e naturali
# 2.2 - Ingegneri, architetti e professioni assimilate
# 2.3 - Specialisti nelle scienze della vita
# 2.6 - Specialisti della formazione e della ricerca - docenti e ricercatori universitari
to_keep = c("2.1","2.2","2.3","2.6") 

#exceptions:
# 2.4 - Specialisti della salute
# 2.5 - Specialisti in scienze umane, sociali, artistiche e gestionali
# 2.6.3 - Professori di scuola secondaria, post-secondaria e professioni assimilate
# 2.6.4 - Professori di scuola primaria, pre–primaria e professioni assimilate
# 2.6.5 - Altri specialisti dell'educazione e della formazione
to_remove3<-c("2.6.3","2.6.4","2.6.5")

# 2.3.1.3 - Agronomi e forestali
# 2.3.1.4 - Veterinari
# 2.3.1.5 - Farmacisti
# 2.6.1.4 - Docenti universitari in scienze dell'antichità, filologico-letterarie e storico-artistiche
# 2.6.1.5 - Docenti universitari in scienze storiche, filosofiche, pedagogiche e psicologiche
# 2.6.1.6 - Docenti universitari in scienze economiche e statistiche
# 2.6.2.4 - Ricercatori e tecnici laureati nelle scienze dell'antichità, filologico-letterarie e storico-artistiche
# 2.6.2.5 - Ricercatori e tecnici laureati nelle scienze storiche, filosofiche, pedagogiche e psicologiche
# 2.6.2.6 - Ricercatori e tecnici laureati nelle scienze economiche e statistiche
# 2.6.2.7 - Ricercatori e tecnici laureati nelle scienze giuridiche, politiche e sociali
to_remove4<-c("2.3.1.3","2.3.1.4","2.3.1.5","2.6.1.4","2.6.1.5","2.6.1.6","2.6.2.4", "2.6.2.5" ,
"2.6.2.6","2.6.2.7")

# 2.1.1.1.2 - Astronomi ed astrofisici
# 2.1.1.6.2 - Paleontologi
# 2.1.1.6.4 - Meteorologi
to_remove5<-c("2.1.1.1.2","2.1.1.6.2", "2.1.1.6.4" )

employees <- data %>%  
        mutate(across(where(is.character), toupper))%>%
        filter(qualifica_2_digit %in% to_keep)%>%
        mutate(q3=substring(qualifica_codice,1,5))%>%filter(!q3 %in% to_remove3)%>%
        mutate(q4=substring(qualifica_codice,1,7))%>%filter(!q4 %in% to_remove4)%>%
        mutate(q5=substring(qualifica_codice,1,9))%>%filter(!q5 %in% to_remove5)%>%
        arrange(data_nascita) 

print(employees%>%group_by(qualifica)%>%tally())

tmp <-  make.ids.conversion.table(employees)

tmp %>%  write_csv("./tmp/ids_conversion_table.csv")

