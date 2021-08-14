#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinycssloaders)
library(shinydashboard)


# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Victorian Shiny Tracks"),


    
    # Application title
    #titlePanel("Victoria Geology information for GPS tracks"),
    
    # Sidebar with a slider input for number of bins
    #sidebarLayout(
        dashboardSidebar(
        #sidebarPanel(
            
            sidebarMenu(
                menuItem("File Upload", tabName = "Files", icon = icon("file-upload")),
                menuItem("Geology", tabName = "Geology", icon = icon("globe-asia")),
                menuItem("Vegetation", tabName = "Vegetation", icon = icon("tree")),
                menuItem("Statistics",
                         menuSubItem("Geology", tabName = "GeoStats"),
                         menuSubItem("Vegetation", tabName = "VegStats"),
                         tabName = "Statistics", icon = icon("chart-bar")
                )
                
            )
        ),
        
        # Show a plot of the generated distribution
        #mainPanel(
        dashboardBody(
            leafletOutput("main_map", height = 300),
            column(width = 10, 
               sliderInput("range", 
                    label = "Range of interest:",
                    min = 0, max = 100, value = c(0, 100), width= '100%'
               ), 
               offset = 1
            ),
            tabItems(
                tabItem(tabName = "Files", 
                        fileInput("gpx", "Upload gpx file", multiple = FALSE, accept = c(".gpx")),
                        withSpinner(uiOutput("info", inline = TRUE), size = 0.5, type = 8, proxy.height = 150),
                        radioButtons("x_axis", label = "x-axis", choiceNames = c("distance (m)", "time"), choiceValues = c('d', 't'), inline = TRUE)
                ),
                tabItem(tabName = "Geology",
                    withSpinner(plotOutput("geoPlot", width='100%', brush = "plot_brush", dblclick = "geo_click"), type = 8),
                    tags$b("Double click plot for more details"), 
                    tags$br(),
                    tags$a(href="https://discover.data.vic.gov.au/dataset/geological-units-represented-as-two-dimensional-polygons-1-250-000", "Data Source: discover.data.vic.gov.au/dataset/geological-units-represented-as-two-dimensional-polygons-1-250-000", target="_blank"),
                    tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", "CCA-4.0", target = "_blank")
                ),
                tabItem(tabName = "Vegetation",
                        withSpinner(plotOutput("vegPlot", width='100%', brush = "plot_brush", dblclick = "veg_click"), type = 8),
                        tags$b("Double click plot for more details"), 
                        tags$br(),
                        tags$a(href="https://discover.data.vic.gov.au/dataset/native-vegetation-modelled-2005-ecological-vegetation-classes-with-bioregional-conservation-sta", "Data Source: discover.data.vic.gov.au/dataset/native-vegetation-modelled-2005-ecological-vegetation-classes-with-bioregional-conservation-sta", target="_blank"),
                        tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", "CCA-4.0", target = "_blank")
                ),
                tabItem(tabName = "Statistics"),
                tabItem(tabName = "GeoStats", plotOutput("geoStats")),
                tabItem(tabName = "VegStats", plotOutput("vegStats"))
            )
            
        )
    )
)