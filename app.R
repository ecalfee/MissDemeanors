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
# load data to map
map_data <- readRDS(here("data/full_data.RDS")) %>%
  mutate(Year = as.integer(Year))
CA_polygon = ggplot2::map_data("state")  %>% # united states data
  filter(., region == "california") 

# function to combine plots
combine_plots <- function(set_year){
  # make map
  map_prisons <- plot_map(set_year = set_year, state_polygon = CA_polygon, data = map_data)
  # make line plot
  line_plot <- plot_lines(set_year = set_year)
  # get events/legislation names
  events_text <- plot_events(set_year = set_year)
  # put plots together
  # plots_together = gridExtra::grid.arrange(grobs = list(ggplotGrob(map_prisons + theme(legend.position = "none")),
  #                                                       cowplot::get_legend(map_prisons),
  #                                                       ggplotGrob(line_plot)),
  #                                          layout_matrix = rbind(c(1,2),
  #                                                                c(3, 3)),
  #                                          heights = c(4, 1),
  #                                          widths = c(4, 1))
  # plots_together = gridExtra::grid.arrange(grobs = list(ggplotGrob(map_prisons), 
  #                                                       ggplotGrob(line_plot)),
  #                                          # arrange plot 1 in row 1 and plot 2 in row 2 (only 1 column here)
  #                                          layout_matrix = rbind(c(1), c(2)),
  #                                          # set relative heights and widths of plots in the layout matrix
  #                                          heights = c(5, 3),
  #                                          widths = c(1))
  plots_together = gridExtra::grid.arrange(grobs = list(ggplotGrob(map_prisons), 
                                                        ggplotGrob(line_plot),
                                                        ggplotGrob(events_text)),
                                           # arrange plot 1 in row 1 and plot 2 in row 2 (only 1 column here)
                                           layout_matrix = rbind(c(1,3), c(2,3)),
                                           # set relative heights and widths of plots in the layout matrix
                                           heights = c(5, 3),
                                           widths = c(4, 1))
  
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
                             width = 1000, height = 500)
}



# shinyApp runs the app!
shinyApp(ui = ui, server = server)
