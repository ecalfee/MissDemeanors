# Shiny App to show our data visualization

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. See http://shiny.rstudio.com/
require(shiny)
require(ggplot2)
require(maps)
require(dplyr)
require(here) # paths relative to project directory "MissDemeanors/Shiny"

# all code that only needs to be run once goes here
source(here("scripts/plot_CA_prisonpops.R"))
# load data to map
map_data <- readRDS(here("data/full_data.RDS")) %>%
  mutate(Year = as.integer(Year))
CA_polygon = ggplot2::map_data("state")  %>% # united states data
  filter(., region == "california") 

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
  output$myMap <- renderPlot(plot_map(set_year = input$Year, state_polygon = CA_polygon, data = map_data))
}

# shinyApp runs the app!
shinyApp(ui = ui, server = server)
