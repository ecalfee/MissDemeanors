# Shiny App to show our data visualization

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. See http://shiny.rstudio.com/
require(shiny)
require(ggplot2)
require(maps)
require(dplyr)
library(gridExtra) # for combining multiple plots together
library(cowplot) # for extracting legend from plots
require(here) # paths relative to project directory "MissDemeanors/Shiny"

# all code that only needs to be run once goes here
source(here("scripts/plot_CA_prisonpops.R"))
source(here("scripts/plot_lines_historical_counts.R"))

# function to combine plots
combine_plots <- function(set_year){
  # make map
  map_prisons <- plot_map(set_year = set_year, state_polygon = CA_polygon, data = map_data)
  # make line plot
  line_plot <- plot_lines(set_year = set_year)
  # get events/legislation names
  events_text <- plot_events(set_year = set_year)
  # put plots together
  plots_together = gridExtra::grid.arrange(grobs = list(ggplotGrob(map_prisons + theme(legend.position = "none")),
                                                        cowplot::get_legend(map_prisons),
                                                        ggplotGrob(line_plot),
                                                        ggplotGrob(events_text)),
                                           layout_matrix = rbind(c(2, 1, 4),
                                                                 c(NA, NA, NA), # added white space
                                                                 c(3, 3, 3)),
                                           heights = c(5.1, 0.1, 2),
                                           widths = c(1.5, 3.2, 2))
  return(plots_together)
}


ui <- fluidPage(
  # title
  titlePanel("California Prison Population Through Time"), #places the title
  fluidRow(plotOutput("myMap")), #places the main map plot
  hr(),hr(),hr(),hr(), #adds space so that the slider will be under the plot
  fluidRow( #creates slider
   column(6, offset = 0.5,
          wellPanel(sliderInput(inputId = "Year", 
                        label = "Change Year", 
                        min = 1995, 
                        max = 2020, 
                        value = 1995, # default (where slider starts)
                        # add a 'play' button for animating through years
                        animate = animationOptions(interval = 3000), # default is 1000, here we slow down animations
                        sep = "" # no comma in dates at thousands spot
   )))
 )
)


# server renders the plot
server <- function(input, output) {
  output$myMap <- renderPlot(combine_plots(set_year = input$Year),
                             # fixing height and width of plot
                             width = 1100, height = 500)
}



# shinyApp runs the app!
shinyApp(ui = ui, server = server)
