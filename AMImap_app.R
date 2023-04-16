
#adding packages
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(sf)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(dotenv)


# load .env file
load_dot_env(".env")


# pulling the data
con <- dbConnect(Postgres(), dbname = "housenyc",
                 host = Sys.getenv("DBHOST"), port = 5432,
                 user = Sys.getenv("USER"), Sys.getenv("PASSWORD1"))

nta_affordability <- st_read(con, layer = Id(schema = "amimap", table = "nta_affordability"))

dbDisconnect(con)


# creating map
pal <- colorQuantile(palette = "Purples", domain = 0:100, n = 5)

amimap <- leaflet(data = nta_affordability) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -74.0, lat = 40.7, zoom = 11) %>%
  addPolygons(color = "White", opacity = 1, weight = 2, fillOpacity = .75, fillColor = ~pal(nta_affordability$percent_affordable),
              popup = ~paste(strong("Neighborhood:"), nta_affordability$GeoName, br(),
                             strong("Median Household Income:"), nta_affordability$MdHHIncE, br(),
                             strong("Number of Recorded Units:"), nta_affordability$total_units, br(),
                             strong("Percent Affordable to Neighborhood:"), nta_affordability$percent_affordable)
  ) %>%
  addLegend(position = "bottomright", pal = pal, values = nta_affordability$percent_affordable,
            title = "Percentage Affordable to Neighborhood",
            opacity = 0.5, na.label = "No Affordable Housing Recorded")


# export to html
saveWidget(amimap, file="docs/amimap.html")



