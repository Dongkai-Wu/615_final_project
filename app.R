library(shiny)
library(ggplot2)
library(maps)
library(dplyr)
library(leaflet)
library(readr)


#data prep
stop<- read.delim("/Users/dongkaiwu/Desktop/615/615_Final/MBTA_GTFS/stops.txt", ",", header = TRUE)
stops <- stop
stops<- select(stops, stop_name, zone_id, stop_code, location_type, municipality,stop_url, stop_lon, stop_lat)
stops <-stops[complete.cases(stops),]
stops$stop_code <- as.numeric(stops$stop_code)


Travel_Times_Fall_2019 <- read_csv("Average_Rapid_Transit_Travel_Times_Fall_2019.csv")
newtravel <- Travel_Times_Fall_2019
colnames(newtravel)[4] <- "stop_id"
travelweek <- filter(newtravel, day_type == "weekday")
travelsat <- filter(newtravel, day_type == "saturday")
travelsun <- filter(newtravel, day_type == "sunday")

table_week <- merge(x = travelweek, y = stop, by = "stop_id",
                    all.x = TRUE)
table_sat <- merge(x = travelsat, y = stop, by = "stop_id",
                   all.x = TRUE)
table_sun <- merge(x = travelsun, y = stop, by = "stop_id",
                   all.x = TRUE)

stops <- list(table_week, table_sat, table_sun)
stops <- Reduce(function(x, y) merge(x, y, all=TRUE), stops)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("selectroute", label = "Select Route",
                            choices = unique(stops$route_id), selected = "Red"),
                selectInput("selectcity", label = "Select City",
                            choices = unique(stops$municipality), selected = "Boston"),
                radioButtons("selectday", label = "Select Day Type",
                             choices = unique(stops$day_type), selected = "weekday")
  )
)





server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet(stops %>% 
              dplyr::filter(stops$route_id == input$selectroute,
                            stops$municipality == input$selectcity,
                            stops$day_type == input$selectday
              )) %>% 
      addTiles() %>%
      addMarkers(lat = ~stop_lat, lng = ~stop_lon, popup =paste0("Stop Name: "
                                                                 , stops$stop_name
                                                                 , "<br>"
                                                                 , "From Stop: "
                                                                 , stops$from_stop_name
                                                                 , "<br>"
                                                                 , "Travel Time(sec): "
                                                                 , stops$average_sec
                                                                 
      ))
  })
  
}



shinyApp(ui, server)


