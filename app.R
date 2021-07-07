

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
setwd("~/Covid_Analysis/")
all_modzcta <- readRDS("all_modzcta.RDS")


ui <- fluidPage(

   
    titlePanel("Covid-19 NYC Trends by Modified ZCTA"),


    sidebarLayout(
        sidebarPanel(
            tags$a(href = "https://github.com/nychealth/coronavirus-data", "Data Repository", target = "_blank"),
                   h5("All data are aggregated by week.
                      Tested possitive is percentage of people that tested possitive for COVID-19
                      All data are sourced fro NYC department of health"),
                 selectInput("date",
                             "Select a date(week ending in):",
                             choices = unique(all_modzcta$week_ending)
                   )
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Case Rate", leafletOutput("cases")),
                tabPanel("Test Rate", leafletOutput("tests")),
                tabPanel("Percent Positive", leafletOutput("pctpos"))
            )
        )
    ))


# Define server logic required to draw a histogram
server <- function(input, output) {

    week_zcta <- reactive({
        w <- all_modzcta %>% filter(week_ending == input$date)
        return(w)
    })
    output$cases <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases per 100,000 people",
            week_zcta()$MODZCTA, week_zcta()$caserate) %>%
            lapply(htmltools::HTML)
        
        pal <- colorBin(palette = "YlGn", 9, domain = all_modzcta$caserate) #paletter
        
        week_zcta() %>%
            st_transform(crs = "+init=epsg:4326") %>% #coordinate system
            leaflet() %>% #map
            addProviderTiles(provider = "CartoDB.Positron") %>%#type of map
            setView(-73.6,40.7, zoom = 10) %>%
            addPolygons(label = labels,
                        stroke = FALSE,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~ pal(week_zcta()$caserate),
                        highlightOptions = highlightOptions(weight = 5,
                                                            fillOpacity = 1,
                                                            color = "black",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>%
            
            addLegend("bottomright",
                      pal = pal,
                      values = ~ caserate,
                      title = "Case per 100,000",
                      opacity = 0.7)
        
    })
    output$tests <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases per 100,000 people",
            week_zcta()$MODZCTA, week_zcta()$testrate) %>%
            lapply(htmltools::HTML)
        
        pal <- colorBin(palette = "PuBu", 9, domain = all_modzcta$testrate) #paletter
        
        week_zcta() %>%
            st_transform(crs = "+init=epsg:4326") %>% #coordinate system
            leaflet() %>% #map
            addProviderTiles(provider = "CartoDB.Positron") %>%#type of map
            setView(-73.6,40.7, zoom = 10) %>%
            addPolygons(label = labels,
                        stroke = FALSE,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~ pal(week_zcta()$testrate),
                                          highlightOptions = highlightOptions(weight = 5,
                                                                              fillOpacity = 1,
                                                                              color = "black",
                                                                              opacity = 1,
                                                                              bringToFront = TRUE)) %>%
                            
                            addLegend("bottomright",
                                      pal = pal,
                                      values = ~ testrate,
                                      title = "Case per 100,000",
                                      opacity = 0.7)
    })
    output$pctpos <- renderLeaflet({
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases per 100,000 people",
            week_zcta()$MODZCTA, week_zcta()$percentpos) %>%
            lapply(htmltools::HTML)
        
        pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$percentpos) #paletter
        
        week_zcta() %>%
            st_transform(crs = "+init=epsg:4326") %>% #coordinate system
            leaflet() %>% #map
            addProviderTiles(provider = "CartoDB.Positron") %>%#type of map
            setView(-73.6,40.7, zoom = 10) %>%
            addPolygons(label = labels,
                        stroke = FALSE,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~ pal(week_zcta()$percentpos),
                                          highlightOptions = highlightOptions(weight = 5,
                                                                              fillOpacity = 1,
                                                                              color = "black",
                                                                              opacity = 1,
                                                                              bringToFront = TRUE)) %>%
                            
                            addLegend("bottomright",
                                      pal = pal,
                                      values = ~ percentpos,
                                      title = "Case per 100,000",
                                      opacity = 0.7)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
