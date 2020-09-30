# Shiny App to show our data visualization

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. See http://shiny.rstudio.com/
library(shiny)
library(ggplot2)
library(dplyr)

# all code that only needs to be run once goes here
# load data to map
MapData<-readRDS("../../data/full_data.RDS")
MapData$Year = as.integer(MapData$Year)

# background plot of CA
plot_CA <- ggplot() + 
  geom_polygon(data = ggplot2::map_data("state")  %>% # united states data
                          filter(., region == "california"), aes(x = long, y = lat, group = group), alpha = 0.25) + #plot of California
  theme_classic() + 
  geom_text()

# ui controls the inputs and outputs and
# layout of our shiny webpage
ui <- fluidPage(
      # title
      titlePanel("California Prison Population Through Time"),
      # add side-by-side main plot and sliding bar
      sidebarLayout(
        position = "right",
        sidebarPanel( # slider input from user
          sliderInput(inputId = "Year", 
                      label = "Change Year", 
                      min = 1995, max = 2020, 
                      value= 1995, # default (where slider starts)
                      animate = T, # add a 'play' button for animating through years
                      sep = "" # no comma in dates at thousands spot
                      )), 
        mainPanel( # main panel = plot
          plotOutput("myMap"))
        )
    )


# server renders the plot
server <- function(input, output) {
    #observeEvent(input$Year == "2019", {input$Year <- 2020}) # my attempt to skip years with no data - didn't work
    output$myMap <- renderPlot(plot_CA + #plot of California
                                    geom_point(data = MapData %>%  # add bubbles for each prison
                                                 dplyr::filter(Year == input$Year), 
                                               aes(x = long, y = lat, 
                                                   fill = percent_bin, 
                                                   size = Inmates), 
                                               alpha = 0.8, 
                                               shape = 21) + 
                                    annotate("text", # add text for total prison population
                                             label = paste0("Total Prison Population: ", 
                                                            sum(MapData %>% filter(Year == input$Year) %>% 
                                                                  dplyr::select(Inmates), na.rm = T)), 
                                             x = -117.0, 
                                             y = 40.0, 
                                             size = 3, 
                                             colour = "black") + 
                                    geom_text() +
                                    annotate("text", 
                                             label = paste0("Average prison occupancy: ", 
                                                            round(mean(as.numeric(unlist(MapData %>% filter(Year == input$Year) %>% 
                                                                                           dplyr::select(Percent_occupancy_fix)),
                                                                                  na.rm = T), digits = 5)) ,"%"), 
                                             x = -117.0, 
                                             y = 41.0, 
                                             size = 4, 
                                             colour = "black"))
}

# shinyApp runs the app!
shinyApp(ui = ui, server = server)
