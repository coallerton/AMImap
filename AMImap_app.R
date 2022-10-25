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

con <- dbConnect(Postgres(), dbname = "postgres",
                 host = "67.81.37.92", port = 5432,
                 user = "guest_user", password = "PASSWORD UPON REQUEST")


AH_nta_year <- dbReadTable(con, name = Id(schema = "AMI", table = "AH_nta_year"))
AH_avg <- dbReadTable(con, name = Id(schema = "AMI", table = "AH_avg"))
nta_AH <- st_read(con, layer = Id(schema = "AMI", table = "nta_AH"))

dbDisconnect(con)


ui <- navbarPage(
  "Housing Affordability Analysis in NYC", theme = shinytheme("cosmo"),
  header = tagList(useShinydashboard()),
  tabPanel("Affordability Map",
           leafletOutput("AMImap", height = 700)
  ),
  tabPanel("Neighborhood Data", br(),
           fluidRow(
             column(3, selectInput("borough", label = NULL, choices = unique(nta_AH$Borough), selected = "Manhattan")),
             column(3, selectInput("neighborhood", label = NULL, choices = unique(nta_AH$GeoName)))
           ),
           fluidRow(tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                    column(4, valueBoxOutput("data", width = NULL), valueBoxOutput("data2", width = NULL), valueBoxOutput("data3", width = NULL)),
                    column(6, highchartOutput("chart"))
           )
  )
)

server <- function(input, output, session) {
  pal <- colorQuantile(palette = "Purples", domain = 0:100, n = 5)
  output$AMImap <- renderLeaflet({
    leaflet(data = nta_AH) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -74.0, lat = 40.7, zoom = 11) %>%
      addPolygons(color = "White", opacity = 1, weight = 2, fillOpacity = .75, fillColor = ~pal(nta_AH$percent_affordable),
                  label = ~ifelse(is.na(nta_AH$percent_affordable), paste("No Affordable Housing Built in", nta_AH$GeoName), 
                                  paste(nta_AH$percent_affordable, "Percent Affordable to", nta_AH$GeoName))) %>%
      addLegend(position = "bottomright", pal = pal, values = nta_AH$percent_affordable,
                title = "Percentage Affordable to Neighborhood",
                opacity = 0.5, na.label = "No Affordable Housing Built")
  })
  observe({
    updateSelectInput(session, "neighborhood", choices = unique(nta_AH$GeoName[nta_AH$Borough == input$borough])
    )
  })
  observe({
    filterdata <- nta_AH[nta_AH$GeoName == input$neighborhood,]
    if(input$neighborhood %in% nta_AH$GeoName) {
      output$data <- renderValueBox({
        valueBox(value = paste0("$", format(filterdata$MdHHIncE, big.mark = ",")), icon = icon("dollar-sign"), subtitle = "Median Household Income",
                 width = NULL, color = "purple")
      })
      output$data2 <- renderValueBox({
        valueBox(value = paste0(format(filterdata$total_units, big.mark = ","), " units"), icon = icon("building"), subtitle = "Total Affordable Units Built",
                 width = NULL, color = "purple")
      })
      output$data3 <- renderValueBox({
        valueBox(value = paste0(filterdata$percent_affordable, "%"), icon = icon("percent"), subtitle = "Affordable to Local Community",
                 width = NULL, color = "purple")
      })
      output$chart <- renderHighchart({
        highchart() %>%
          hc_title(text = "Affordable Units Built Each Year") %>%
          hc_add_series(data = AH_avg, type = 'spline', hcaes(x = year, y = mean), name = "Citywide Avg", color = "grey") %>%
          hc_add_series(data = AH_nta_year %>% filter(ntaname == input$neighborhood),
                        type = 'spline', hcaes(x = year, y = sum), name = input$neighborhood, color = "purple")
      })
    }
  })
  
}

shinyApp(ui, server)


