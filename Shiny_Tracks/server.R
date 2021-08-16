#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lwgeom)
library(leaflet)
library(sf)
library(grid)
library(gridExtra)
library(viridis)
library(zoo)
library(DBI)
library(config)
library(RPostgres)

conn_args <- config::get("dataconnection")
con <- dbConnect(RPostgres::Postgres(),
                 host = conn_args$server,
                 port = conn_args$port,
                 dbname = conn_args$database,
                 user = conn_args$uid,
                 password = conn_args$pwd,
                 bigint = "integer")

#Set system variable
sf_use_s2(FALSE)
origin <- "1970-01-01" #R Epoch
crs<-st_crs(7844)

shinyServer(function(input, output, session) {
    #Main map initialise on Vic
    map <- leaflet() %>%
        addTiles() %>%
        fitBounds(lng1 = 140.1, lng2 = 150.2, lat1 = -34.9, lat2 = -38.5)
    
    #Map for pop up windows
    pop_map <- leaflet() %>% 
        addTiles()
    
    #Inputs work better as globals 
    x <- 0
    xmin <- 0
    xmax <- 100
    x_axis <- 'd'
    track <- reactiveValues()
    
    
    query <- paste('SELECT ids, name FROM tracks')
    names <- dbGetQuery(con, query)
    n <- names$ids
    names(n)<-names$name
    updateSelectizeInput(session, "select", choices = n, selected = NULL)
    
    observeEvent(input$load, {
        validate(need(input$select, message = FALSE))
        
        query <- paste("SELECT * FROM points WHERE track_fid =", input$select)
        t <- st_read(con, query = query, geometry_column = "geometry")
        
        track$trail <- t
        
        
        bb <- as.numeric(st_bbox(t))
        
        t <- t %>%
            st_combine() %>%
            st_cast(to = "LINESTRING") %>%
            st_sf()
        
        
        
        if(!all(is.na(t$time))) {
            t$time <- na.approx(as.numeric(track$time))
            updateRadioButtons(
                session,
                "x_axis",
                selected = x_axis,
                choiceNames = c("distance (m)", "time"),
                choiceValues = c('d', 't'),
                inline = TRUE
            )
        } else {
            x_axis <<- 'd'
            updateRadioButtons(
                session,
                "x_axis",
                selected = 'd',
                choiceNames = c("distance (m)"),
                choiceValues = c('d'),
                inline = TRUE
            )
        }
    })
    
    
    
    # The selected file, if any
    userFile <- reactive({
        validate(need(input$gpx, message = FALSE))
        
        updateTextInput(session, "name", label = "Save name", value = input$gpx$name)
        
        input$gpx
    })
    
    
    observeEvent(input$save, {
        validate(need(trail(), message = "No file to uploaded"))
        validate(need(input$name, message = "track needs a name to save"))
        
        t<-trail()
        bb<-st_bbox(t)
        
        d<-data.frame(name = input$name, min.ele = min(t$ele), max.ele = max(t$ele), start = as.POSIXct(t$time[1], tz= "Australia/Victoria", origin = origin), end = as.POSIXct(t$time[nrow(t)], tz= "Australia/Victoria", origin = origin))
        sfc <- st_sfc(st_point(c(bb[1],bb[2])),st_point(c(bb[3],bb[4])))
        st_geometry(d)<-st_combine(sfc)
        st_crs(d)<-crs
        st_write(d, con, "tracks", append = TRUE)
        
        Sys.sleep(0.5)
        
        query<- paste("SELECT ids FROM tracks WHERE name ='", input$name, "'", sep = "")
        id <- dbGetQuery(con, query)
        
        t$track_fid<-id[1,]
        t$time<-as.POSIXct(t$time, tz= "Australia/Victoria", origin = origin)
        
        st_write(t, con, layer = "points", append = TRUE)
        
        query <- paste('SELECT ids, name FROM tracks')
        names <- dbGetQuery(con, query)
        n <- names$ids
        names(n)<-names$name
        updateSelectizeInput(session, "select", choices = n, selected = id[1,])
    })
    
    #Load gpx file and calculate the cumulative distance
    observe({
        validate(need(userFile(), message = FALSE))

        trail <- data.frame()
        trail <- sf::st_read(userFile()$datapath, layer = "track_points", quiet = TRUE)
        
        trail<-st_transform(trail, crs)

        d<-sf::st_distance(trail)
        trail$Dis <- as.numeric(cumsum(d[1,]))/1000
        if(!all(is.na(trail$time))) {
            trail$time <- na.approx(as.numeric(trail$time))
            updateRadioButtons(
                session,
                "x_axis",
                selected = x_axis,
                choiceNames = c("distance (m)", "time"),
                choiceValues = c('d', 't'),
                inline = TRUE
            )
        } else {
            x_axis <<- 'd'
            updateRadioButtons(
                session,
                "x_axis",
                selected = 'd',
                choiceNames = c("distance (m)"),
                choiceValues = c('d'),
                inline = TRUE
            )
        }
        track$trail<-trail
    })
    
    trail <- reactive({
        validate(need(track$trail, message = FALSE))
        
        track$trail
    })
    
    
    #debounce trail to allow ui to update before doing more calculations
    trail_d <- debounce(trail, 500)
    
    start <- reactive({
        validate(need(trail(), message = FALSE))
        t<-trail()$time
        if(all(is.na(t)))
        {
            start <- "time missing from gpx file"
        } else {
            start <- as.character(as.POSIXct(min(t, na.rm = TRUE), origin = origin))
        }
        start
    })
    
    
    
    end <- reactive({
        validate(need(trail(), message = FALSE))
        t<-trail()$time
        if(all(is.na(t)))
        {
            end <- " "
        } else {
            end <- as.character(as.POSIXct(max(t, na.rm = TRUE), origin = origin))
        }
        end
    })
    
    
    
    total_distance <- reactive({
        validate(need(trail(), message = FALSE))
        t<-trail()

        t$Dis[nrow(t)]
    })
    
    
    
    geo <- reactive({
        bb <- st_bbox(trail_d())

        query <- paste('SELECT *',
                       'FROM "SG_GEOLOGICAL_UNIT_250K"',
                       'WHERE "geometry" && ST_MakeEnvelope(',bb[1], ', ', bb[2], ', ', bb[3], ', ', bb[4],');'
        )

        geo <-st_read(con, query = query, geometry_column = "geometry")

        #geo <- st_transform(geo, st_crs(trail()))

        geo
    })
    
    
    
    veg <- reactive({
        bb <- st_bbox(trail_d())

        query <- paste('SELECT *',
                       'FROM "NV2005_EVCBCS"',
                       'WHERE "geometry" && ST_MakeEnvelope(',bb[1], ', ', bb[2], ', ', bb[3], ', ', bb[4],');'
        )

        veg <-st_read(con, query = query, geometry_column = "geometry")

        #veg <- st_transform(veg, st_crs(trail()))

        veg
    })
    
    
   
    #Find the polygons that the gpx file pass through and group them into blocks
    trail_geo <- reactive({
        validate(need(trail <- trail_d(), message = FALSE))
        validate(need(gsf <- geo(), message = FALSE))

        suppressMessages(trail<-sf::st_join(trail, gsf, join = st_nearest_feature))

        trail$GEO_GRP<-1

        d<-trail[1,]$id
        grp<-1
        for(i in 1:nrow(trail)-1){
            if(!(d %in% trail[i+1,]$id)) {
                grp<-grp+1
                d<-trail[i+1,]$id
            }
            trail[i+1,]$GEO_GRP<-grp
        }

        trail
    })
    
    
    
    #Find the polygons that the gpx file pass through and group them into blocks
    trail_veg <- reactive({
        validate(need(trail <- trail_d(), message = FALSE))
        validate(need(vsf <- veg(), message = FALSE))

        suppressMessages(trail<-sf::st_join(trail, vsf, join = st_nearest_feature))

        trail$VEG_GRP<-1

        d<-trail[1,]$id
        grp<-1
        for(i in 1:nrow(trail)-1){
            if(!(d %in% trail[i+1,]$id)) {
                grp<-grp+1
                d<-trail[i+1,]$id
            }
            trail[i+1,]$VEG_GRP<-grp
        }

        trail
    })
    
    
    
    geo_cl <- reactive({
        validate(need(trail_geo(), message = FALSE))
        t <- trail_geo()
        
        v <- viridis(length(unique(t$name.y)), option = "turbo")
        names(v) <- unique(t$name.y)
        
        v
    })
    
    veg_cl <- reactive({
        validate(need(trail_veg(), message = FALSE))
        t <- trail_veg()
        
        v <- viridis(length(unique(t$x_evcname)), option = "turbo")
        names(v) <- unique(t$x_evcname)
        
        v
    })
                            

    observeEvent(input$x_axis,{
        x_axis <<- input$x_axis

        t <- trail()

        if(x_axis == 'd') {
            d <- t$Dis
            xmin <<- 0
            xmax <<- ceiling(max(d))
            updateSliderInput(session, "range", value = c(xmin, xmax), min = 0, max = xmax)
        } else {
            xmin <<- min(t$time, na.rm = TRUE)
            xmax <<- max(t$time, na.rm = TRUE)
            updateSliderInput(session,
                              "range",
                              value = as.POSIXct(c(xmin, xmax), origin = origin),
                              min = as.POSIXct(xmin, origin = origin),
                              max = as.POSIXct(xmax, origin = origin))
        }

    })
    
    
    

    #When the gpx file is loaded update the map
    observe({
       t <- trail()
       bb <- as.numeric(st_bbox(t))

       t <- t %>%
           st_combine() %>%
           st_cast(to = "LINESTRING") %>%
           st_sf()

        leafletProxy("main_map", session) %>%
            clearShapes() %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            addPolylines(data = t)
    }, priority = 1)
    
    
    
    # Pop up info window when the plot is double clicked
    observeEvent(input$geo_click, {
        validate(need(input$geo_click, message = FALSE))
        
        x <<- input$geo_click$x
        showModal(geoModal())
    })
    
    # Pop up info window when the plot is double clicked
    observeEvent(input$veg_click, {
        validate(need(input$veg_click, message = FALSE))
        
        x <<- input$veg_click$x
        showModal(vegModal())
    })
    
    # Update the the slider to match the input file and x-axis selection
    observe({
        t <- trail()

        if(x_axis == 'd') {
            d <- t$Dis
            xmin <<- 0
            xmax <<- ceiling(max(d))
            updateSliderInput(session, "range", value = c(xmin, xmax), min = 0, max = xmax)
        } else {
            xmin <<- min(t$time, na.rm = TRUE)
            xmax <<- max(t$time, na.rm = TRUE)
            updateSliderInput(session,
                              "range",
                              value = as.POSIXct(c(xmin, xmax), origin = origin),
                              min = as.POSIXct(xmin, origin = origin),
                              max = as.POSIXct(xmax, origin = origin))
        }
    })
    
    
    
    
    #Zoom in when plot is brushed 
    observeEvent(input$plot_brush,{
        val<-input$plot_brush
        t<-trail_d()

        if(x_axis == 'd') {
            xmin <<- max(val$xmin, 0)
            xmax <<- min(val$xmax, max(t$Dis))
            updateSliderInput(session, "range",  value = c(xmin,xmax))
        } else {
            r <- range(t$time, na.rm = TRUE)
            xmin <<- max(as.numeric(val$xmin), min(t$time, na.rm = TRUE))
            xmax <<- min(as.numeric(val$xmax), max(t$time, na.rm = TRUE))
            updateSliderInput(session, "range", value = as.POSIXct(c(xmin, xmax), origin = origin))
        }
    })
    
    
    

    #Update the plot when the selected range changes
    observeEvent(input$range, {
        validate(need(trail <- trail(), message = FALSE),need(input$range, message = FALSE))
        t <- trail()

        if(x_axis == 'd'){
            xmin <<- max(input$range[1], 0)
            xmax <<- min(input$range[2], max(t$Dis))

            start <- st_coordinates(t[t$Dis >= xmin,][1,])
            end <- st_coordinates(t[t$Dis >= xmax,][1,])
        } else {

            xmin <<- max(as.numeric(input$range[1]), min(t$time, na.rm = TRUE))
            xmax <<- min(as.numeric(input$range[2]), max(t$time, na.rm = TRUE))

            start <- st_coordinates(t[t$time >= xmin,][1,])
            end <- st_coordinates(t[t$time >= xmax,][1,])
        }


        # Also show the start and end points on map
        leafletProxy("main_map", session) %>%
            clearMarkers() %>%
            addCircleMarkers(lng = start[1], lat = start[2], color = "green", fillOpacity = 0.5) %>%
            addCircleMarkers(lng = end[1], lat = end[2], color = "red", fillOpacity = 0.5)

    })
    
    
    # Render the geo plot
    output$geoPlot <- renderPlot({
        validate(need(trail_geo(), message = FALSE))
        validate(need(input$range, message = FALSE))
        
        session$resetBrush("plot_brush")
        
        t <- trail_geo()
        
        if(x_axis == 'd') {
            gg <- ggplot(data = t, aes(fill = name.y, group = GEO_GRP)) + 
                geom_ribbon(aes(x=Dis, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "horizontal") + 
                labs(x ="Distance (m)", y = "Elevation (m)") +
                xlim(xmin, xmax) + 
                scale_fill_manual(values = geo_cl(), name = "") + 
                guides(color=guide_legend(nrow = 3))
        } else {
            geo <- t[!is.na(t$time),]
            gg <- ggplot(data = t, aes(fill = name.y, group = GEO_GRP)) + 
                geom_ribbon(aes(x = as.POSIXct(time, origin = origin, tz = "Australia/Victoria"), ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "horizontal") + 
                labs(x ="Time", y = "Elevation (m)") +
                xlim(as.POSIXct(xmin, origin = origin, tz = "Australia/Victoria"), as.POSIXct(xmax, origin = origin, tz = "Australia/Victoria"))  + 
                scale_fill_manual(values = geo_cl(), name = "") + 
                guides(color=guide_legend(nrow = 3))
        }
        gg
    })
    
    output$vegPlot <- renderPlot({
        validate(need(trail_veg(), message = FALSE))
        validate(need(input$range, message = FALSE))
        
        session$resetBrush("plot_brush")
        
        t <- trail_veg()
        
        if(x_axis == 'd') {
            gg <- ggplot(data = t, aes(fill = x_evcname, group = VEG_GRP)) + 
                geom_ribbon(aes(x=Dis, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "horizontal") + 
                labs(x ="Distance (m)", y = "Elevation (m)") +
                xlim(xmin, xmax) + 
                scale_fill_manual(values = veg_cl(), name = "") +
                guides(color=guide_legend(nrow = 3))
        } else {
            geo <- t[!is.na(t$time),]
            gg <- ggplot(data = t, aes(fill = x_evcname, group = VEG_GRP)) + 
                geom_ribbon(aes(x = as.POSIXct(time, origin = origin, tz = "Australia/Victoria"), ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "horizontal") + 
                labs(x ="Time", y = "Elevation (m)" , name = "") +
                xlim(as.POSIXct(xmin, origin = origin, tz = "Australia/Victoria"), as.POSIXct(xmax, origin = origin, tz = "Australia/Victoria"))  + 
                scale_fill_manual(values = veg_cl(), name = "") + 
                guides(color=guide_legend(nrow = 3))
        }
        gg
    })
    
    # Pop window
    geoModal <- function() {
        t <- trail_geo()

        if(x_axis == 'd'){
            dat <- t[t$Dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        
        leaf_out <- leafletOutput("geo_map")
        
        if(!is.null(leaf_out)){
            modalDialog(
                tags$div(tags$b("Name: "), dat$name.y, tags$br(), 
                         tags$b("Type: "), dat$geolut, tags$br(),
                         tags$b("Rank: "), dat$rank, tags$br(),
                         tags$b("Lithology: "), dat$lithology, tags$br(), 
                         tags$b("Description: "), dat$desc.y, tags$br(), 
                         tags$b("History: "), dat$geolhist),
                leaf_out,
                
                tags$a(href=dat$replit_uri, "Lithology", target="_blank"),
                tags$a(href=dat$geolut_uri, "Formation", target="_blank"),
                tags$a(href=dat$repage_uri, "Age", target="_blank"),
                easyClose = TRUE,
                footer = NULL
        )}
    }
    
    # Pop window
    vegModal <- function() {
        t <- trail_veg()

        if(x_axis == 'd'){
            dat <- t[t$Dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        
        leaf_out <- leafletOutput("veg_map")
        
        if(!is.null(leaf_out)){
            modalDialog(
                tags$div(tags$b("Name: "), dat$x_evcname, tags$br(), 
                         tags$b("Subgroup: "), dat$xsubggroup, tags$br(),
                         tags$b("Group: "), dat$xgroupname, tags$br(),
                         tags$b("Bioregion: "), dat$bioregion, tags$br(), 
                         tags$b("Status: "), dat$evcbcsdesc),
                leaf_out,
                easyClose = TRUE,
                footer = NULL
            )}
    }
    
    
    output$geo_map <- renderLeaflet({
        c <- input$geo_click
        
        t <- trail_geo()
        
        if(x_axis == 'd'){
            dat <- t[t$Dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        dat <- t[t$GEO_GRP==dat$GEO_GRP,]
        bb <- as.numeric(st_bbox(dat))
        
        line <- dat %>% 
            sf::st_combine() %>%
            sf::st_cast(to = "LINESTRING", warn = FALsE) %>%
            sf::st_sf()
        
        gsf <- geo()
        
        
        poly <- gsf[gsf$id %in% dat$id,]$geometry
        
        poly <- poly %>%
            st_cast(to = "POLYGON") %>%
            st_sf()
        
        pop_map %>%
            clearShapes() %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            addPolygons(data = poly, fillColor = as.character(geo_cl()[dat$name.y[1]]), fillOpacity =  0.3, stroke = FALSE) %>%
            addPolylines(data = line)  
    })
    
    output$veg_map <- renderLeaflet({
        c <- input$veg_click
        
        t <- trail_veg()
        
        if(x_axis == 'd'){
            dat <- t[t$Dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        dat <- t[t$VEG_GRP==dat$VEG_GRP,]
        bb <- as.numeric(st_bbox(dat))
        
        line <- dat %>% 
            sf::st_combine() %>%
            sf::st_cast(to = "LINESTRING", warn = FALsE) %>%
            sf::st_sf()
        
        vsf <- veg()
        
        
        poly <- vsf[vsf$id %in% dat$id,]$geometry
        
        poly <- poly %>%
            st_cast(to = "POLYGON") %>%
            st_sf()
        
        pop_map %>%
            clearShapes() %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            addPolygons(data = poly, fillColor = as.character(veg_cl()[dat$x_evcname[1]]), fillOpacity =  0.3, stroke = FALSE) %>%
            addPolylines(data = line)  
    })
    
    output$info <- renderUI({ 
        validate(need(start(), message = FALSE), need(end(), message = FALSE), need(total_distance(), message = FALSE))
        
        tags$div(tags$b("Start: "), start(), tags$br(), 
                 tags$b("End: "), end(), tags$br(), 
                 tags$b("Total distance: "), round(total_distance()), "m")
    })
    
    output$main_map <- renderLeaflet(map)
    
    output$geoStats <- renderPlot({
        validate(need(t <- trail_geo(), message = FALSE), need(r <- input$range, message = FALSE))
        
        if(x_axis == 'd'){
            dat <- t[t$Dis>=xmin,]
            dat <- dat[dat$Dis<=xmax,]
        } else {
            dat <- t[t$time>=xmin,]
            dat <- dat[dat$time<=xmax,]
        } 
        
        #g <- max(t$GEO_GRP)
        
        #c <- rep(0, length(unique(t$name.y)))
        #names(c) <- unique(t$name.y)
        
        #for(i in 1:g){
        #     d<-t[t$GEO_GRP==g,]
        #     
        #     s <- max(d$Dis)-min(d$Dis)
        #     
        #     c[d[1,]$name.y] = c[d[1,]$name.y] + 1
        # }
        
        ggplot(dat, aes(x=name.y, fill = name.y)) + 
            geom_bar(stat="count") + 
            theme(axis.text.x = element_text(angle = 90), legend.position="none") +
            xlab(element_blank()) +
            scale_fill_manual(values = geo_cl())
    })
        
    output$vegStats <- renderPlot({
        validate(need(t <- trail_veg(), message = FALSE), need(r <- input$range, message = FALSE))
        
        
        if(x_axis == 'd'){
            dat <- t[t$Dis>=xmin,]
            dat <- dat[dat$Dis<=xmax,]
        } else {
            dat <- t[t$time>=xmin,]
            dat <- datt[dat$time<=xmax,]
        }    
        #g <- max(t$GEO_GRP)
            
        #c <- rep(0, length(unique(t$name.y)))
        #names(c) <- unique(t$name.y)
            
        #for(i in 1:g){
        #     d<-t[t$GEO_GRP==g,]
        #     
        #     s <- max(d$Dis)-min(d$Dis)
        #     
        #     c[d[1,]$name.y] = c[d[1,]$name.y] + 1
        #}
            
        ggplot(dat, aes(x=x_evcname, fill = x_evcname)) + 
            geom_bar(stat="count") + 
            theme(axis.text.x = element_text(angle = 90), legend.position="none") +
            xlab(element_blank()) +
            scale_fill_manual(values = veg_cl())
    })
    
})


    
