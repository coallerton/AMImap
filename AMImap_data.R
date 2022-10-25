library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(sf)
library(highcharter)
library(readxl)
library(RSocrata)
library(DBI)
library(RPostgreSQL)
library(RPostgres)


ntas <- read_sf("https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson")
ntas <- ntas %>%
  select(nta2020, ntaname, geometry)

affordableprojectsdata <- read.socrata(
  "https://data.cityofnewyork.us/resource/hg8x-zxpr.json",
  app_token = "S16sBzqeXtOEHIDbwJrvmPBgq",
  email     = "coallerton@gmail.com",
  password  = "pLZ8ZurAbpxXE9K"
)
download.file("https://www1.nyc.gov/assets/planning/download/office/planning-level/nyc-population/acs/econ_20162020_acs5yr_nta.xlsx", "ACS_data.xlsx", mode = "wb")
ACS_data <- read_excel("ACS_data.xlsx")
ACS_data <- ACS_data %>%
  select(GeoName, GeoID, Borough, MdHHIncE) %>%
  mutate(affordable_rent = round(MdHHIncE/12*0.3, 0))

affordableprojects_sf <- affordableprojectsdata %>%
  mutate(extremely_low_income_units = as.numeric(extremely_low_income_units)) %>%
  mutate(very_low_income_units = as.numeric(very_low_income_units)) %>%
  mutate(low_income_units = as.numeric(low_income_units)) %>%
  mutate(moderate_income_units = as.numeric(moderate_income_units)) %>%
  mutate(middle_income_units = as.numeric(middle_income_units)) %>%
  mutate(counted_rental_units = as.numeric(counted_rental_units)) %>%
  filter(!is.na(longitude)) %>%
  mutate(longitude = as.numeric(longitude)) %>%
  mutate(latitude = as.numeric(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

affordableprojects <- st_join(affordableprojects_sf, ntas)

affordableprojects_byNTA <- affordableprojects %>%
  group_by(nta2020, ntaname) %>%
  summarise(eli_units = sum(extremely_low_income_units), vli_units = sum(very_low_income_units), li_units = sum(low_income_units), moi_units = sum(moderate_income_units), mii_units = sum(middle_income_units))

nta_AH <- left_join(ACS_data, affordableprojects_byNTA, by = c("GeoID" = "nta2020"))
nta_AH <- nta_AH %>%
  select(-c(ntaname, geometry))
nta_AH <- nta_AH %>%
  mutate(total_units = rowSums(nta_AH[6:10]))
nta_AH <- nta_AH %>%
  mutate(affordable_NTA = ifelse(nta_AH$affordable_rent > 3603, nta_AH$total_units,
                                  ifelse(nta_AH$affordable_rent > 2402, rowSums(nta_AH[6:9]),
                                          ifelse(nta_AH$affordable_rent > 1501, rowSums(nta_AH[6:8]),
                                                  ifelse(nta_AH$affordable_rent > 900, rowSums(nta_AH[6:7]), nta_AH$eli_units)))))
nta_AH <- nta_AH %>%
  mutate(percent_affordable = round(affordable_NTA/total_units*100, 1))
nta_AH <- left_join(nta_AH, ntas, by = c("GeoID" = "nta2020"))
nta_AH <- st_as_sf(nta_AH)

AH_nta_year <- affordableprojects %>%
  tibble() %>%
  filter(!is.na(project_completion_date)) %>%
  mutate(year = as.numeric(str_sub(project_completion_date, 1, 4))) %>%
  mutate(all_counted_units = as.numeric(all_counted_units))
AH_nta_year <- AH_nta_year %>%
  group_by(ntaname, year) %>%
  summarise(sum = sum(all_counted_units))

AH_avg <- AH_nta_year %>%
  group_by(year) %>%
  summarise(mean = round(mean(sum), 0))


con <- dbConnect(Postgres(), dbname = "postgres",
                 host = "67.81.37.92", port = 5432,
                 user = "postgres", password = "cma365163")

dbWriteTable(con, name = Id(schema = "AMI", table = "AH_nta_year"), 
             value = AH_nta_year, overwrite = TRUE)
dbWriteTable(con, name = Id(schema = "AMI", table = "AH_avg"),
             value = AH_avg, overwrite = TRUE)
dbWriteTable(con, name = Id(schema = "AMI", table = "nta_AH"),
             value = nta_AH, overwrite = TRUE)

dbDisconnect(con)



