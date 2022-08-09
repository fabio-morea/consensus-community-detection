# Author: Fabio Morea @ Area Science Park
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
#               Original data is not included in the package as it contains persnoal information
#               Test data is random an contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network 



# main

## debug mode
debug <- FALSE
echo <- TRUE

## load libraries
library(tidyverse)
library(lubridate)  #dates
library(readxl)
 
## load functions
source("./code/parameters.R")
source("./code/functions-labour-market-data.R")

path <- get.data.folder.path()

columns <- c(   "id_cittadino","data_nascita",  "iso3","genere",
                "data_inizio","data_fine","tipo_contratto","qualifica_2_digit",
                "qualifica","qualifica_codice", "comune_istat", 
                "professione", "saldo",
                "CF", "az_ragione_soc",  
                "rl_ateco", "rl_ateco_macro", "rl_ateco_settore", 
                "sede_op_ateco", "sede_op_comune", "sede_op_indirizzo", "sede_op_provincia", 
                "SLL_codice", "SLL_nome", "tipo_contratto" )


data <- load_data(path, columns, echo, debug)  
 
## filter by profession
# 2.1 - Specialisti in scienze matematiche, informatiche, chimiche, fisiche e naturali
# 2.2 - Ingegneri, architetti e professioni assimilate
# 2.3 - Specialisti nelle scienze della vita
# 2.4 - Specialisti della salute
# 2.5 - Specialisti in scienze umane, sociali, artistiche e gestionali
# 2.6 - Specialisti della formazione e della ricerca - docenti e ricercatori universitari

to_keep = c("2.1","2.2","2.3","2.4","2.5","2.6") 

#exceptions:
# 2.6.3 - Professori di scuola secondaria, post-secondaria e professioni assimilate
# 2.6.4 - Professori di scuola primaria, pre–primaria e professioni assimilate
# 2.6.5 - Altri specialisti dell'educazione e della formazione
#to_remove3<-c("2.6.3","2.6.4","2.6.5")
to_remove3<-c()

# 2.6.1.4 - Docenti universitari in scienze dell'antichità, filologico-letterarie e storico-artistiche
# 2.6.1.5 - Docenti universitari in scienze storiche, filosofiche, pedagogiche e psicologiche
# 2.6.2.4 - Ricercatori e tecnici laureati nelle scienze dell'antichità, filologico-letterarie e storico-artistiche
# 2.6.2.5 - Ricercatori e tecnici laureati nelle scienze storiche, filosofiche, pedagogiche e psicologiche
# 2.6.2.7 - Ricercatori e tecnici laureati nelle scienze giuridiche, politiche e sociali
#to_remove4<-c("2.6.1.4","2.6.1.5","2.6.2.4", "2.6.2.5" ,"2.6.2.7")
to_remove4<-c()

# 2.1.1.1.2 - Astronomi ed astrofisici
# 2.1.1.6.2 - Paleontologi
# 2.1.1.6.4 - Meteorologi
# to_remove5<-c("2.1.1.1.2","2.1.1.6.2", "2.1.1.6.4" )
to_remove5<-c()


data <- data %>%  
        mutate(across(where(is.character), toupper))%>%
        mutate(q2=substring(qualifica_codice,1,3))%>%filter( q2 %in% to_keep)%>%
        mutate(q3=substring(qualifica_codice,1,5))%>%filter(!q3 %in% to_remove3)%>%
        mutate(q4=substring(qualifica_codice,1,7))%>%filter(!q4 %in% to_remove4)%>%
        mutate(q5=substring(qualifica_codice,1,9))%>%filter(!q5 %in% to_remove5)
        

print(data%>%group_by(q2)%>%tally())
idct <-  make.ids.conversion.table(data, echo )
idct %>%  
    write_csv("./tmp/ids_conversion_table.csv") # only for debug

employees<-data%>%
    left_join(idct, by="id_cittadino")%>%
    select(-id_cittadino)

employees <- employees %>% 
    group_by(idempl)%>%
    mutate(date_in = min(data_inizio))%>%
    mutate(date_out =max(data_fine))%>%
    select(idempl, data_nascita, genere, iso3, date_in, date_out) %>%
    rename(dob=data_nascita, sex=genere, country=iso3) %>%
    drop_na(idempl) %>%
    arrange(idempl) %>%
    unique()

# TODO: fuzzify dat eof birth and transform into age
employees %>% 
    write_csv("./tmp/employees.csv")


## identify contracts
contracts <- data  %>%
    mutate(across(where(is.character), toupper)) %>%
    filter(id_cittadino %in% idct$id_cittadino) %>%
    mutate(dd= replace_na(data_fine, ymd(today())))%>%
    mutate(durat = time_length(dd - data_inizio, 'years'))

contracts <- contracts %>%
    left_join(idct, by="id_cittadino") %>%
    relocate(idempl, CF) %>%
    select(idempl, CF, az_ragione_soc, data_inizio, data_fine, durat, professione, qualifica, qualifica_codice, rl_ateco, rl_ateco_macro, rl_ateco_settore, saldo, sede_op_ateco, sede_op_comune, sede_op_indirizzo, sede_op_provincia, SLL_nome, tipo_contratto )

contracts %>% write_csv("./tmp/contracts.csv")

transitions.table <- make.transitions.table(contracts, echo)
transitions.table %>% write_csv("./tmp/transitions.csv")

# links <- transitions.table %>%
#     select(empl, cf1, cf2, date_end1, date_start2, gap, ww, qualif)%>% 
#     mutate(q3=substring(qualif,1,5))%>%
#     mutate(q5=substring(qualif,1,5))%>%
#     unique()%>%
#     arrange(date_start2) 

qualif_groups <- read_excel("profess_groups.xlsx", sheet="prof_groups") %>%
    rename(qualif = qualifica_codice )%>%
    select(qualif,group)

links <- transitions.table %>%
    select(empl, cf1, cf2, date_end1, date_start2, gap, ww, qualif)%>% 
    merge( qualif_groups, by="qualif")%>%
    arrange(date_start2) %>%
    mutate(yy = year(date_start2))%>% select(-date_start2,-date_end1)%>%
    relocate(cf1,cf2,ww,group)

links <- links %>%
    filter(yy>=2014) %>% # transitions from 2013 registered in early 2014
    filter(yy<=2021)     # not a complete year

links %>% write_csv("./tmp/links.csv")
 

selected.organisations <- toupper(unique( c(links$cf1,links$cf2) ))
orgs <- make.organisations.table(contracts,selected.organisations )
orgs %>% write_csv("./tmp/organisations.csv")


## Debug: SOLVED - check again with new data - add a preliminary check that the dates are consistent
## we expect that all transitions are within the range of selected years 2014-2015 but there are more
## eg employee 437 - links:
## 1983-07-04,437,CF_00164830309,CF_04030970968,31.822039698836413,1
## CONTROLLARE LINKS CON LE DOPPIE DATE: C'è QUALCOSA CHE NON QUADRA
## LA DATA 1 E DATA 2 NON SONO NEL RANGE PREVISTO
## E IN ALCUNI CASI SONO INVERTITE

print("Script completed.")
