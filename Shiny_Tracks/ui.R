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
library(shinyjs)
library(shinyWidgets)
library(DT)



# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Victorian Shiny Tracks"),

        dashboardSidebar(
            sidebarMenu( id = "tabs",
                menuItem("Current Location", tabName = "Location", icon = icon("compass"), selected = TRUE),
                menuItem("Files", tabName = "Files", icon = icon("file-upload")),
                menuItem("Statistics",
                    menuSubItem("Geology", tabName = "GeoStats"),
                    menuSubItem("Vegetation", tabName = "VegStats"),
                    tabName = "Statistics", icon = icon("chart-bar")
                ),
                menuItem("Search", tabName = "Search", icon = icon("search")),
                menuItem("About", tabName = "About", icon = icon("info-circle"))
                
            )
        ),
        
        dashboardBody(
            useShinyjs(),
            tabItems(
                tabItem(tabName = "Location",
                    leafletOutput("location_map", height = 400),
                    tabsetPanel(
                        tabPanel("Local Geology",
                            uiOutput("local_geology"), 
                            value = "geo"       
                        ),
                        tabPanel("Local Vegetation",
                            uiOutput("local_vegetation"), 
                            value = "veg"
                        ),
                        fluidRow(
                            column(width = 6, 
                                   numericInput("lng", label = "Longitude", value = 0, step = 0.1, width = 120)
                                   ),
                            column(width = 6, 
                                   numericInput("lat", label = "Latitude", value = 0, step= 0.1, width = 120)
                                   )
                        ),
                        id = "location"
                    ), 
                    actionButton("go", label = "Go"),
                    actionButton("gps", label = "Current Location")
                ),
                tabItem(tabName = "Files",
                    leafletOutput("file_map", height = 300),
                    column(width = 10, 
                           sliderInput("range", 
                                       label = "Range of interest:",
                                       min = 0, max = 100, value = c(0, 100), width= '100%'
                           ),
                           offset = 1
                    ),
                    column(width = 12,
                        tabsetPanel(
                            tabPanel("Upload",
                                column(5,
                                    fileInput("gpx", "Upload gpx file", multiple = FALSE, accept = c(".gpx")),
                                    withSpinner(uiOutput("info", inline = TRUE), size = 0.5, type = 8, proxy.height = 150),
                                    radioButtons("x_axis", 
                                                 label = "x-axis", 
                                                 choiceNames = c("distance (m)", "time"), 
                                                 choiceValues = c('d', 't'), 
                                                 inline = TRUE),
                                    textInput("name", "Save name", value = "", placeholder = "name"),
                                    withSpinner(uiOutput("save", inline = TRUE), size = 0.5, type = 8, proxy.height = 150)
                                    #actionButton("save", "Save")
                                ),
                                column(5, 
                                       selectizeInput("select", "load saved track", choices = NULL), 
                                       actionButton("load", "Load")
                                )
                            ),
                            tabPanel("Geology",
                                withSpinner(plotOutput("geoPlot", 
                                                       width='100%', 
                                                       brush = "geo_plot_brush", 
                                                       dblclick = "geo_dbl_click", 
                                                       click = "geo_click"),
                                            type = 8),
                                tags$b("Double click plot for more details"), 
                                tags$br(),
                                tags$a(href="https://discover.data.vic.gov.au/dataset/geological-units-represented-as-two-dimensional-polygons-1-250-000", 
                                       "Data Source: discover.data.vic.gov.au/dataset/geological-units-represented-as-two-dimensional-polygons-1-250-000", 
                                       target="_blank"),
                                tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", 
                                       "CCA-4.0", 
                                       target = "_blank")
                            ),
                            tabPanel("Vegetation",
                                withSpinner(plotOutput("vegPlot", 
                                                       width='100%', 
                                                       brush = "veg_plot_brush", 
                                                       dblclick = "veg_dbl_click", 
                                                       click = "veg_click"), 
                                            type = 8),
                                tags$b("Double click plot for more details"), 
                                tags$br(),
                                tags$a(href="https://discover.data.vic.gov.au/dataset/native-vegetation-modelled-2005-ecological-vegetation-classes-with-bioregional-conservation-sta", 
                                       "Data Source: discover.data.vic.gov.au/dataset/native-vegetation-modelled-2005-ecological-vegetation-classes-with-bioregional-conservation-sta", 
                                       target="_blank"),
                                tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", 
                                       "CCA-4.0", 
                                       target = "_blank")
                            )
                        )
                    )
                ),
                tabItem(tabName = "Statistics"),
                tabItem(tabName = "GeoStats", withSpinner(plotOutput("geoStats"), type = 8)),
                tabItem(tabName = "VegStats", withSpinner(plotOutput("vegStats"), type = 8)),
                tabItem(tabName = "Search", 
                        tabsetPanel(
                            tabPanel("Geology",
                                     searchInput(
                                         inputId = "geo_search", label = "Keywords",
                                         placeholder = "Enter Keywords here",
                                         btnSearch = icon("search"),
                                         btnReset = icon("remove"),
                                         width = "50%"
                                     ),
                                     fluidRow(
                                        column(3,
                                            radioButtons("geo_text", "Search Type:", choices = c("Words", "Phrase", "Formal query"), inline = TRUE)
                                        ), column(8,
                                        checkboxGroupButtons("geo_fields", 
                                                          "Search fields", 
                                                          choiceNames = c("Name", "Description", "Lithology", "Age", "Rank", "Formation", "Observation Method"), 
                                                          choiceValues = c("name", "desc", "lithology", "geolhist", "rank", "geolut", "obsmeth")
                                                        )
                                        ), column(1,
                                            checkboxInput("g_select_all", "select all")
                                        )
                                     ),
                                    DTOutput("geo_results")
                                ),
                            tabPanel("Vegetation",
                                     searchInput(
                                         inputId = "veg_search", label = "Keywords",
                                         placeholder = "Enter Keywords here",
                                         btnSearch = icon("search"),
                                         btnReset = icon("remove"),
                                         width = "50%"
                                     ),
                                     fluidRow(
                                         column(4,
                                            radioButtons("veg_text", "Search Type:", choices = c("Words", "Phrase", "Formal query"), inline = TRUE)
                                         ), column(6,
                                            checkboxGroupButtons("veg_fields", 
                                                          "Search fields", 
                                                          choiceNames = c("Name", "Bioregion", "Status", "Group", "Subgroup"), 
                                                          choiceValues = c("x_evcname", "bioregion", "evcbcsdesc", "xgroupname", "xsubggroup")
                                            )
                                        ), column(2,
                                            checkboxInput("v_select_all", "select all")
                                        )
                                     ),
                                     DTOutput("veg_results")
                                ),
                            tabPanel("Results Map",
                                     leafletOutput("results_map", height = 600),
                                     DTOutput("geo_selected"),
                                     DTOutput("veg_selected")
                            ),
                            id = "search_tabs"
                        )
                ),
                tabItem(tabName = "About", 
                    column(width = 8,
                        "Author: Lachlan Dryburgh 2021 ", 
                        tags$a(href= "lachlan.d@gmail.com", "lachlan.d@gmail.com", target = "_blank"), tags$br(), tags$br(),
                        "Written in R using the shiny web development framework, features leaflet maps, ggplot2, Simple Features, DataTables packages.", tags$br(), tags$br(),
                        "Data provided by open data vic under creative commons attribution international 4.0 ", 
                        tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", 
                               "CCA-4.0", 
                               target = "_blank"), 
                        tags$br(),
                        tags$a(href="https://discover.data.vic.gov.au/dataset/geological-units-represented-as-two-dimensional-polygons-1-250-000", 
                               "discover.data.vic.gov.au/dataset/geological-units-represented-as-two-dimensional-polygons-1-250-000", 
                               target="_blank"), 
                        tags$br(),
                        tags$a(href="https://discover.data.vic.gov.au/dataset/native-vegetation-modelled-2005-ecological-vegetation-classes-with-bioregional-conservation-sta", 
                               "discover.data.vic.gov.au/dataset/native-vegetation-modelled-2005-ecological-vegetation-classes-with-bioregional-conservation-sta", 
                               target="_blank"), 
                        tags$br(), 
                        tags$br(),
                        "Deployed on shinyapps.io, and AWS using PostgreSQL with the PostGIS extension.", 
                        offset = 1
                    )
                )
            )
            
        )
    )
)