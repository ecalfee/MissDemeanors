#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
#library(agridat) 


#Now we load all the data we need
#Barley <- as.data.frame(beaven.barley)
MapData<-readRDS("../../data/full_data.RDS")
#MapData<-as.data.frame(MapData)
MapData$Year = as.integer(MapData$Year)

#This fits the layout of the webapp to our figure
#titlePanel() indicates that we would like a separate panel at the top of the page in which we can put the title.
#sidebarLayout() indicates that we want our Shiny app to have the sidebar layout, one of many layouts we saw above. Within sidebarLayout we have:
#sidebarPanel() indicates that we want a sidebar panel included in our app. Sidebar panels often contain input widgets like sliders, text input boxes, radio buttons etc.
#mainPanel() indicates that we want a larger main panel. Main panels often contain the output of the app, whether it is a table, map, plot or something else.
# ui.R ----
ui <- 
    fluidPage(
        titlePanel("California Prison Population Through Time"),
        sidebarLayout(
            position = "right",
            sidebarPanel(h3("Shift By Year"), 
                         sliderInput(inputId = "Year", "3. Change Year", min=1995, max=2020, value= c(1995)),
                         br()),
            mainPanel(
                plotOutput("myMap"))
        )
    )
    
# ui <- fluidPage(
#     titlePanel("Barley Yield"),
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(inputId = "gen",  # Give the input a name "genotype"
#                         label = "1. Select genotype",  # Give the input a label to be displayed in the app
#                         choices = c("A" = "a","B" = "b","C" = "c","D" = "d","E" = "e","F" = "f","G" = "g","H" = "h"), selected = "a"),  # Create the choices that can be selected. e.g. Display "A" and link to value "a"
#             selectInput(inputId = "colour", 
#                         label = "2. Select histogram colour", 
#                         choices = c("blue","green","red","purple","grey"), selected = "grey"),
#             sliderInput(inputId = "bin", 
#                         label = "3. Select number of histogram bins", 
#                         min=1, max=25, value= c(10)),
#             textInput(inputId = "text", 
#                       label = "4. Enter some text to be displayed", "")
#         ),
#         mainPanel()
#     )
# )

#In this function is where we put the code to make the plot
# server.R ----
server <- function(input, output) {
    output$myMap <- renderPlot(ggplot() + geom_polygon(data = ggplot2::map_data("state")  %>% # united states data
                                                            filter(., region == "california"), aes(x = long, y = lat, group = group), alpha = 0.25) + #plot of California
                                    geom_point(data = MapData %>% dplyr::filter(Year == input$Year), aes(x = long, y = lat, fill = percent_bin, size = Inmates), alpha = 0.8, shape = 21) + #add bubbles
                                    theme_classic() + 
                                    #scale_color_discrete(values = colors_percent) +
                                    #scale_fill_gradient(low="blue", high="red") + 
                                    #scale_colour_gradientn(colours = myPalette(100), limits=c(0, 3600)) +
                                    geom_text() +
                                    annotate("text", label = paste0("Total Prison Population: ", 
                                                                    sum(MapData %>% filter(Year == input$Year) %>% 
                                                                          dplyr::select(Inmates), na.rm = T)), 
                                             x = -117.0, y = 40.0, size = 3, colour = "black") + 
                                    geom_text() +
                                    annotate("text", label = paste0("Average prison occupancy: ", 
                                                                    round(mean(as.numeric(unlist(MapData %>% filter(Year == input$Year) %>% 
                                                                                                   dplyr::select(Percent_occupancy_fix)), 
                                                                                          na.rm = T), digits = 5)) ,"%"), 
                                             x = -117.0, y = 41.0, 
                                             size = 4, colour = "black") )
                                
        
        #ggplot(Barley, aes(x = yield)) + 
                                    #geom_histogram(bins = input$bin, fill = input$col, group=input$gen, 
                                                   #data=Barley[Barley$gen == input$gen,],
                                                   #colour = "black"))
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
