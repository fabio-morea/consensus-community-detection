# main

## load libraries
library (tidyverse)  
library (lubridate)  #dates
library (kableExtra) #tables
library (xtable)     #tables
library (igraph)

## load functions 
source("./code/fun-labour-market-data.R")


path <- "C:/Users/morea/OneDrive - Area Science Park/General - iifvg lavoro/DatiElaborati/Riservati"
 
#columns = c("CF","anno","az_ragione_soc","data","data_fine","data_fine_prev","data_inizio","data_nascita","eta","genere","id_cittadino",
#"mese","professione","qualifica","qualifica_codice","rl_ateco","rl_ateco_macro","rl_ateco_settore",
#"saldo","sede_op_ateco","sede_op_comune","sede_op_indirizzo","sede_op_provincia","somm","tipo_contratto","tipo_orario","cittadinanza","iso3","sigla_prov","comune_istat","codice_istat","SLL_codice","SLL_nome","contratto")

columns = c(
    "CF","anno","az_ragione_soc","data","data_fine","data_fine_prev","data_inizio",
    "id_cittadino","data_nascita","eta","genere","cittadinanza","iso3",
    "professione","qualifica","qualifica_codice","contratto",
    "saldo","sede_op_ateco","sede_op_comune","sede_op_indirizzo","sede_op_provincia",
    "somm","tipo_contratto","tipo_orario",
    "sigla_prov","comune_istat","codice_istat","SLL_codice","SLL_nome")


data <- load_data(path, columns)  
print(nrow(data))
print(ncol(data))
