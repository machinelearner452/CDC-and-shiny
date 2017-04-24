rm(list=ls())                # free up memory for the download of the data sets

library(shiny)
library(leaflet)
library(RColorBrewer)
library(DT)

options(shiny.reactlog=TRUE) # Cltr + Fn + F3 to to toggle on while program running

data_c <- read.csv(file = "500_Cities__Local_Data_for_Better_Health Outcomes_Shiny.csv",
                 header = TRUE, 
                 sep = "," )

ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(top = 10, right = 10,
                sliderInput("range", "Health Outcomes", min(data_c$healthoutcome), max(data_c$healthoutcome),
                value = range(data_c$healthoutcome), step = 0.001
                      ),
                      selectInput("colors", "Color Scheme",
                                  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                      ),

#   multiple = TRUE 

                      selectInput("state", "State Abbr", data_c$state, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
# this code needs to cause a zooming of the map

                      selectInput("measureid", "Health Outcome", data_c$measureid_label, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
# this code needs to cause a refresh of the number of radius on the map
                      
                      checkboxInput("legend", "Show legend", TRUE),

                      submitButton("Submit")
        )
)

#
#
# https://www.cdc.gov/500cities/methodology.htm
# this is the site to understand the measurement defianations
#
#
#Cities:
#        The project will deliver data for the 497 largest American cities and will include data from the largest cities in Vermont (Burlington - population: 42,417), West Virginia (Charleston - population: 51,400) and Wyoming (Cheyenne - population: 59,466) to ensure inclusion of cities from all the states; bringing the total to 500 cities.
#The number of cities per state ranges from 1 to 121.
#The cities range in population from 42,417 in Burlington, Vermont to 8,175,133 in New York City, New York.
#Among these 500 cities, there are approximately 28,000 census tracts, for which data will be provided. The tracts range in population from less than 50 to 28,960, and in size from less than 1 square mile to more than 642 square miles. The number of tracts per city ranges from 8 to 2,140.
#The project includes a total population of 103,020,808, which represents 33.4% of the total United States population of 308,745,538.
#Largest 500 US Cities Map[PDF-314KB]
#
#
server <- function(input, output, session) {
 

        # Reactive expression for the data subsetted to what the user selected
        filteredData1 <- reactive({
                data_c[data_c$healthoutcome >= input$range[1] & data_c$healthoutcome <= input$range[2],]
        })

#        filterData2 <- reactiveValues({data_c %>% input$state})  # having a hard  time to find the correct dytext to have pull downs select a section of the data table
        
        
        filteredData <-filteredData1 

        # This reactive expression represents the palette function,
        # which changes as the user makes selections in UI.
        colorpal <- reactive({
                colorNumeric(input$colors, data_c$healthoutcome)
        })
        
        output$map <- renderLeaflet({
                # Use leaflet() here, and only include aspects of the map that
                # won't need to change dynamically (at least, not unless the
                # entire map is being torn down and recreated).
                leaflet(data_c) %>% addTiles() %>%

                        fitBounds(~min(long+30), ~min(lat), ~max(long+20), ~max(lat))
                        # the +30 and +20 are to change the centering of the USA in the visable section of the displayed area
        })
        
        # Incremental changes to the map (in this case, replacing the
        # circles when a new color is chosen) should be performed in
        # an observer. Each independent set of things that can change
        # should be managed in its own observer.

        observe({
                pal <- colorpal()
                
                #this needs to reflect the healthputcomes selected - check working
                
                leafletProxy("map", data = filteredData()) %>%
                        clearShapes() %>%
                        addCircles(radius = ~healthoutcome*700000, weight = 1, color = "#777777",
                                   fillColor = ~pal(healthoutcome), fillOpacity = 0.1, popup = ~paste(metropop)
                        )
        })
        
        # Use a separate observer to recreate the legend as needed.
        observe({
                proxy <- leafletProxy("map", data = data_c)
                
                # Remove any existing legend, and only if the legend is
                # enabled, create a new one.
                proxy %>% clearControls()
                if (input$legend) {
                        pal <- colorpal()
                        proxy %>% addLegend(position = "bottomright",
                                            pal = pal, values = ~healthoutcome
                        )
                }
        })
}

shinyApp(ui, server)
