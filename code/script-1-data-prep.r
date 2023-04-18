# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano,
# within the frame of PhD in Applied Data Science and Artificial Intelligence at University of Trieste
# Package: Labour Market Network - version 1.0
# Description: R program to extract information from labour market data.
# Original data is not included in the package as it contains persnoal information
# Test data is random an contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/labour-market-network 

## debug mode
debug <- FALSE
echo <- TRUE

## load libraries
library(tidyverse)
library(lubridate) #dates
library(readxl)
 
## load functions
source("./params/parameters.R")
source("./code/functions-labour-market-data.R")

path <- get.data.folder.path()

columns <- c("id_cittadino","data_nascita", "iso3","genere",
 "data_inizio","data_fine","tipo_contratto","qualifica_2_digit",
 "qualifica","qualifica_codice", "comune_istat", 
 "professione", "saldo",
 "CF", "az_ragione_soc", 
 "rl_ateco", "rl_ateco_macro", "rl_ateco_settore", 
 "sede_op_ateco", "sede_op_comune", "sede_op_indirizzo", "sede_op_provincia", 
 "SLL_codice", "SLL_nome", "tipo_contratto")

data <- load_data(path, columns, echo, debug)

all_organisations <- data %>% 
    select(CF, az_ragione_soc)  %>% 
    distinct(.keep_all = TRUE)
all_organisations %>% write_csv("./tmp/all_employers.csv")

 
## filter by profession
# 2.1 - Specialisti in scienze matematiche, informatiche, chimiche, fisiche e naturali
# 2.2 - Ingegneri, architetti e professioni assimilate
# 2.3 - Specialisti nelle scienze della vita
# 2.4 - Specialisti della salute
# 2.6 - Specialisti della formazione e della ricerca - docenti e ricercatori universitari
# 2.5.1	Specialisti delle scienze gestionali, commerciali e bancarie
# 2.5.2	Specialisti in scienze giuridiche

to_keep <- c("1.2","1.3","2.1","2.2","2.3","2.4","2.6") 

#exceptions:
# 2.5 - Specialisti in scienze umane, sociali, artistiche e gestionali
# 2.6.3 - Professori di scuola secondaria, post-secondaria e professioni assimilate
# 2.6.4 - Professori di scuola primaria, pre–primaria e professioni assimilate
# 2.6.5 - Altri specialisti dell'educazione e della formazione
# 2.4.1 - medici
to_remove3 <- c("2.4.1", "2.6.3", "2.6.4", "2.6.5")

# 2.5.1.1 PA
# 2.3.1.4 2.3.1.5 veterinari e farmacisti
# 2.6.1.2 2.6.2.2 - docenti e ricercatori universitari medicina
# 2.6.1.4 - Docenti universitari in scienze dell'antichità, filologico-letterarie e storico-artistiche
# 2.6.1.5 - Docenti universitari in scienze storiche, filosofiche, pedagogiche e psicologiche
# 2.6.2.4 - Ricercatori e tecnici laureati nelle scienze dell'antichità, filologico-letterarie e storico-artistiche
# 2.6.2.5 - Ricercatori e tecnici laureati nelle scienze storiche, filosofiche, pedagogiche e psicologiche
# 2.6.2.7 - Ricercatori e tecnici laureati nelle scienze giuridiche, politiche e sociali
to_remove4<-c("2.1.1.4","2.1.1.5",#software ICT
"2.3.1.4","2.3.1.5",
"2.6.1.2","2.6.2.2","2.6.1.4","2.6.1.5","2.6.2.4", "2.6.2.5" ,"2.6.2.7")

# 2.1.1.1.2 - Astronomi ed astrofisici
# 2.1.1.6.2 - Paleontologi
# 2.1.1.6.4 - Meteorologi
to_remove5 <- c()

data <- data %>% 
 mutate(across(where(is.character), toupper)) %>% 
 mutate(q2 = substring(qualifica_codice,1, 3)) %>% filter(q2 %in% to_keep) %>% 
 mutate(q3 = substring(qualifica_codice,1, 5)) %>% filter(!q3 %in% to_remove3) %>% 
 mutate(q4 = substring(qualifica_codice,1, 7)) %>% filter(!q4 %in% to_remove4) %>% 
 mutate(q5 = substring(qualifica_codice,1, 9)) %>% filter(!q5 %in% to_remove5)

print(data %>% group_by(q2) %>% tally())

list_profess <- data %>%
 select(qualifica, qualifica_codice) %>%
 mutate(q3=substring(qualifica_codice,1,5)) %>%
 mutate(q5=substring(qualifica_codice,1,7)) %>%
 unique() %>%
 group_by(q3) %>%
 arrange(q3,qualifica_codice) %>%
 relocate(q3,q5,qualifica_codice)

list_profess %>% write.csv("./tmp/professions.csv",quote=FALSE,row.names=FALSE)

idct <- make.ids.conversion.table(data, echo)
idct %>% write_csv("./tmp/ids_conversion_table.csv") # only for debug

employees<-data %>% 
 left_join(idct, by="id_cittadino") %>%
 select(-id_cittadino)

employees <- employees %>%
 group_by(idempl) %>%
 mutate(date_in = min(data_inizio)) %>%
 mutate(date_out = max(data_fine)) %>%
 select(idempl, data_nascita, genere, iso3, date_in, date_out) %>%
 rename(dob=data_nascita, sex=genere, country=iso3) %>%
 drop_na(idempl) %>%
 arrange(idempl) %>%
 unique()

# TODO: fuzzify date of birth and transform into age
employees %>% 
 write_csv("./tmp/employees.csv")


## identify contracts
contracts <- data %>% 
 mutate(across(where(is.character), toupper)) %>%
 filter(id_cittadino %in% idct$id_cittadino) %>%
 mutate(dd= replace_na(data_fine, ymd(today()))) %>%
 mutate(durat = time_length(dd - data_inizio, 'years'))

contracts <- contracts %>% 
 left_join(idct, by="id_cittadino") %>% 
 relocate(idempl, CF) %>% 
 select(idempl, CF, az_ragione_soc, data_inizio, data_fine, durat, professione, qualifica, qualifica_codice, rl_ateco, rl_ateco_macro, rl_ateco_settore, saldo, sede_op_ateco, sede_op_comune, sede_op_indirizzo, sede_op_provincia, SLL_nome, tipo_contratto)

contracts %>% write_csv("./tmp/contracts.csv")
#contracts <- read_csv("./tmp/contracts.csv") #for debug only

transitions.table <- make.transitions.table(contracts, echo)
transitions.table %>% write_csv("./tmp/transitions.csv")
transitions.table <- read_csv("./tmp/transitions.csv") #for debug only
links <- transitions.table %>% 
    select(empl, cf1, cf2, date_end1, date_start2, gap, ww, qualif, sede_op_comune, sede_op_ateco) %>% 
    arrange(date_start2) %>% 
    mutate(yy = year(date_start2)) %>%
    select(-date_start2, -date_end1)

print(links %>% head(10))

# professional group is a property of the link, so we add it to links.csv as "PG"
profess_groups <- read_excel("groups.xlsx", sheet = "professions") %>%
 rename(qualif = qualifica_codice) %>%
 select(qualif, PG)
links <- links %>% merge(profess_groups, by = "qualif")
print(table(links$PG))

# location is a property of the link, so we add it to links.csv
location_groups <- read_excel("groups.xlsx", sheet = "locations") %>%
 select(sede_op_comune,LOC)
links <- links %>% merge(location_groups, by = "sede_op_comune")
print(location_groups)
print(table(links$LOC))

# nace sector is a property of the link, so we add it to links.csv
nace_sector_groups <- read_excel("groups.xlsx", sheet = "sectors") %>%
 select(sede_op_ateco, NACE, NACE_group)
links <- links %>% merge(nace_sector_groups, by = "sede_op_ateco")
print(table(links$NACE_group))

links <- links %>%
 filter(yy >= 2014) %>% # transitions from 2013 registered in early 2014
 filter(yy <= 2021) # not a complete year

links %>%
 relocate(cf1, cf2, ww, PG, LOC, NACE_group) %>%
 distinct() %>%
 write_csv("./tmp/links.csv")

selected_organisations <- toupper(unique(c(links$cf1,links$cf2)))
orgs <- make.organisations.table(contracts, selected_organisations)
orgs %>% write_csv("./tmp/organisations.csv")

print("Script completed.")
