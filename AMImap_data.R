

#adding packages
library(tidyverse)
library(sf)
library(readxl)
library(RSocrata)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(dotenv)


# load .env file
load_dot_env(".env")


#pulling the data
ntas <- read_sf("https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson")
ntas <- ntas %>%
  select(nta2020, ntaname, geometry)

affordableprojectsdata <- read.socrata(
  "https://data.cityofnewyork.us/resource/hg8x-zxpr.json",
  app_token = Sys.getenv("APPTOKEN"),
  email     = Sys.getenv("EMAIL"),
  password  = Sys.getenv("PASSWORD2")
)
download.file("https://www1.nyc.gov/assets/planning/download/office/planning-level/nyc-population/acs/econ_20162020_acs5yr_nta.xlsx", "ACS_data.xlsx", mode = "wb")
ACS_data <- read_excel("ACS_data.xlsx")


#cleaning and manipulating the data
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

affordableprojects_geom <- st_join(affordableprojects_sf, ntas)

affordableprojects_byNTA <- affordableprojects_geom %>%
  group_by(nta2020, ntaname) %>%
  summarise(eli_units = sum(extremely_low_income_units), vli_units = sum(very_low_income_units), li_units = sum(low_income_units), moi_units = sum(moderate_income_units), mii_units = sum(middle_income_units))

nta_affordability <- left_join(ACS_data, affordableprojects_byNTA, by = c("GeoID" = "nta2020"))
nta_affordability <- nta_affordability %>%
  select(-c(ntaname, geometry))
nta_affordability <- nta_affordability %>%
  mutate(total_units = rowSums(nta_affordability[6:10]))
nta_affordability <- nta_affordability %>%
  mutate(affordable_NTA = ifelse(nta_affordability$affordable_rent > 3603, nta_affordability$total_units,
                                  ifelse(nta_affordability$affordable_rent > 2402, rowSums(nta_affordability[6:9]),
                                          ifelse(nta_affordability$affordable_rent > 1501, rowSums(nta_affordability[6:8]),
                                                  ifelse(nta_affordability$affordable_rent > 900, rowSums(nta_affordability[6:7]), nta_affordability$eli_units)))))
nta_affordability <- nta_affordability %>%
  mutate(percent_affordable = round(affordable_NTA/total_units*100, 1))
nta_affordability <- left_join(nta_affordability, ntas, by = c("GeoID" = "nta2020"))
nta_affordability <- st_as_sf(nta_affordability)


con <- dbConnect(Postgres(), dbname = "housenyc",
                 host = "67.81.41.208", port = 5432,
                 user = "conor", password = "cma365163")

dbWriteTable(con, name = Id(schema = "amimap", table = "nta_affordability"),
             value = nta_affordability, overwrite = TRUE)

dbDisconnect(con)



