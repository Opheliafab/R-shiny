# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")

test_weather_data_generation<-function(){
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}
test_weather_data_generation()

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII, LAT, LNG, BIKE_PREDICTION,
             BIKE_PREDICTION_LEVEL, LABEL, DETAILED_LABEL, FORECASTDATETIME, TEMPERATURE) %>%
    summarise(BIKE_PREDICTION = max(BIKE_PREDICTION))
  
  
  # Observe drop-down event
  observeEvent(input$Cities, {
    # If All was selected from dropdown, then render a leaflet map with circle markers
    # and popup weather LABEL for all five cities
    if(input$Cities != 'All') {
    output$city_bike_map <- renderLeaflet({
    leaflet(data = cities_max_bike) %>%
      addTiles() %>%
      addCircleMarkers(data = cities_max_bike,
                       lng = ~cities_max_bike$LNG, 
                       lat = ~cities_max_bike$LAT,
                       popup = ~cities_max_bike$DETAILED_LABEL,
                       radius= ~ifelse(BIKE_PREDICTION_LEVEL == "small", 6, 10),
                       color = ~color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL))
  })
    }
    else {
      # If just one specific city was selected, then render a leaflet map with one marker
      # on the map and a popup with DETAILED_LABEL displayed
      filtered_map <- cities_max_bike %>%
              filter(CITY_ASCII == input$Cities) %>%
              group_by(CITY_ASCII, LAT, LNG, BIKE_PREDICTION, BIKE_PREDICTION_LEVEL,
                       LABEL, DETAILED_LABEL, FORECASTDATETIME, TEMPERATURE)
                  
      output$city_bike_map <- renderLeaflet({
        leaflet(data = filtered_map) %>%
          addTiles() %>%
          addCircleMarkers(data = filtered_map,
                           lng = ~filtered_map$LNG,
                           lat = ~filtered_map$LAT,
                           popup = ~filtered_map$DETAILED_LABEL,
                           radius =~ifelse(filtered_map$BIKE_PREDICTION_LEVEL == "small", 6, 10),
                           color = ~color_levels(filtered_map$BIKE_PREDICTION_LEVEL))
      })
        
      # Then render output plots with an id defined in ui.R  
      # If just one specific city was selected, then render a leaflet map with one marker
      # on the map and a popup with DETAILED_LABEL displayed
    }
  })
  temp_plot_df <- city_weather_bike_df %>%
    filter(CITY_ASCII == input$Cities) %>%
    group_by(CITY_ASCII, LAT, LNG, BIKE_PREDICTION, BIKE_PREDICTION_LEVEL,
             LABEL, DETAILED_LABEL, FORECASTDATETIME, TEMPERATURE)
  
  output$temp_line <- renderPlot({
    temp_trend_plot <- ggplot(data=temp_plot_df, aes(x=TEMPERATURE,
                                                     y = TEMPERATURE)) +
      geom_line(color= "yellow", size = 1) +
      labs(x= "Time(3 hours ahead)", y= "Temperature (C)") +
      geom_point() +
      geom_text(aes(label = paste(TEMPERATURE, "C"), hjust= 0,
                    vjust = 0)) + 
      ggtitle("Temperature Chart")
    temp_trend_plot
  })
  
  
})
