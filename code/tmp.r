# Author: Fabio Morea @ Area Science Park

# Package: italian organisations in Horizon 2020 and Horizon Europe
# Description: a script to load and filter italian city names and codes

# SPDX-License-Identifier: CC-BY-4.0

# GitLab: https://gitlab.com/fabio-morea-areasciencepark/

# options: 
# if you need the latest version, set download_cordis_data = TRUE 
# note that download may take several minutes. 
# if you prefer to download data manually, set download_cordis_data = FALSE
download_cordis_data = FALSE 

# libraries
options(warn=-1)
library(tidyverse)
library(infotheo) #using NMI to match names

 

# source of data
# reference page in Italian: https://www.istat.it/it/archivio/6789
# reference page in English: https://www.istat.it/en/archive/6804 
permalink <- 'https://www.istat.it/storage/codici-unita-amministrative/Elenco-comuni-italiani.csv'
raw_data <- read_delim(permalink, locale = locale(encoding = "windows-1252"), quote = "\"",  delim = ";")

data <- raw_data %>% 
  select("Denominazione in italiano", "Codice Comune formato alfanumerico",
  "Codice NUTS3 2021", "Codice Regione", "Denominazione Regione") %>%
  rename(name = "Denominazione in italiano") %>%
  rename(ISTAT_code = "Codice Comune formato alfanumerico") %>%
  rename(NUTS3 = "Codice NUTS3 2021") %>%
  rename(region = "Denominazione Regione")%>%
  rename(region_code = "Codice Regione") %>%
  mutate(region_code = as.integer(region_code))%>%
  mutate(cityname = toupper(name))%>%
  arrange(NUTS3,ISTAT_code)

  
print(data%>%head(10))

#download cordis dataset and unzip (large file, may take several minutes)

if (download_cordis_data == TRUE)
{
  cordis_address <- "https://cordis.europa.eu/data/cordis-h2020projects-csv.zip" 
  temp <- tempfile()
  download.file( cordis_address,temp)
  data <- read.table(unz(temp, "organisations.csv"))
  orgs <- unlink(temp)
  print(orgs)
} else {
  local_cordis_folder <- "./cordis_data/"
  filename <- paste0(local_cordis_folder, "organization.csv")
  orgs <- read_delim(filename, delim = ";", quote = "\"")
}

orgs <- orgs%>%
  filter(country == "IT")%>%
  select("organisationID", "name", "shortName", "postCode", "city", "geolocation", )

replace_accents <- setNames(
  c(" ", "a", "e", "i", "o", "u"),
  c("'", "à", "è", "ì", "ò", "ù"))

 
city_names <- orgs %>%
  mutate(cityname = tolower(city))%>%
  mutate(cityname = sub("\\(.*", " ", cityname)) %>%
  mutate(cityname = sub(" - .*", " ", cityname)) %>%
  mutate(cityname = sub('-', " ", cityname)) %>%
  mutate(cityname = sub(',.*', "", cityname)) %>%
  mutate(cityname = sub("[^[:alnum:]]", " ", cityname)) %>%
  mutate(cityname = sub("[[:punct:]]",  " ", cityname)) %>%
  mutate(cityname = sub("s ",  "SAN ", cityname)) %>%
  mutate(cityname = sub("   ",  " ", cityname)) %>%
  mutate(cityname = sub("  ",  " ", cityname)) %>%
  mutate(nn = nchar(cityname))%>%
  mutate(test = substr(cityname, nn-2, nn-2))%>%
  mutate(cityname = if_else(test==" ", substr(cityname,1,nn-3), cityname))%>%
  mutate(cityname = sub('\\s+$', '', cityname))%>%
  mutate(cityname = toupper(cityname))%>%
  mutate(lon = as.numeric(sub(",.*", "", geolocation)))%>%
  mutate(lat = as.numeric(sub(".*,", "", geolocation)))%>%
  filter(lat > 0 )%>% filter(lat < 25 )%>%
  filter(lon > 35 )%>% filter(lon < 48 )%>%

  
  group_by(cityname, city, organisationID, shortName,lon, lat)%>%
  tally()

  
  ggplot(city_names) + geom_point(aes(x=lat, y=lon))



city_names %>%
  write_csv("citylist.csv")

data <-   data %>%
  merge(city_names, by="cityname")%>%
  arrange(region_code)%>%
  
  select(organisationID, shortName, ISTAT_code, NUTS3,region_code,region, lon,lat)%>%
    write_csv("geoloc_organisations.csv")

print("Done")
