## load libraries
library(tidyverse)
library(lubridate)  #dates
library(kableExtra) #tables
library(xtable)     #tables
 
## load functions
source("./code/parameters.R")
source("./code/functions-labour-market-data.R")
contracts<- read_csv("./tmp/contracts.csv")
links<- read_csv("./tmp/links.csv")


selected.organisations <- toupper(unique( c(links$cf1,links$cf2) ))
orgs <- make.organisations.table(contracts,selected.organisations )
orgs %>% write_csv("./tmp/organisations.csv")
print("Done")
