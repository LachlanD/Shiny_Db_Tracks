#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(ggplot2)
library(leaflet)
library(sf)
library(grid)
library(gridExtra)
library(viridis)
library(zoo)
library(DBI)
library(config)
library(RPostgres)
library(leaflet.extras)


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
crs <- st_crs(7844) #Coordinate reference system used in my db

shinyServer(function(input, output, session) {
    #Main map initialise on Vic
    map <- leaflet() %>%
        addTiles() %>%
        fitBounds(lng1 = 140.1, lng2 = 150.2, lat1 = -34.9, lat2 = -38.5) %>%
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                                autoCenter = TRUE, maxZoom = 15, 
                                                setView = TRUE))
        

    leafletProxy("location_map", session) %>%
        activateGPS()
    
    output$location_map <- renderLeaflet(map)
    output$file_map  <- renderLeaflet(map)
    
    
    #Map for pop up windows
    pop_map <- leaflet() %>% 
        addTiles()
    
    # Start at current location
    observeEvent(input$location_map_center, { 
        
        updateNumericInput(session = session,
                           inputId = "lat",
                           value = round(input$location_map_center$lat, 3))
        updateNumericInput(session = session,
                          inputId = "lng",
                          value = round(input$location_map_center$lng,3))
    }, once = TRUE)
        
    
    #Inputs work better as globals 
    x <- 0
    xmin <- 0
    xmax <- 100
    x_axis <- 'd'
    track <- reactiveValues()
    

    query <- paste('SELECT id, name FROM tracks')
    names <- dbGetQuery(con, query)
    n <- names$id
    names(n)<-names$name
    updateSelectizeInput(session, "select", choices = n, selected = NULL)
    
    
    
    observeEvent(input$go, {
        leafletProxy("location_map", session) %>%
            flyTo(input$lng, input$lat, zoom = 12)
    })
    
    
    observeEvent(input$gps, {
        leafletProxy("location_map", session) %>%
            deactivateGPS()
            
        leafletProxy("location_map", session) %>%
            activateGPS()
        
        
        Sys.sleep(1)
        updateNumericInput(session = session,
                           inputId = "lng",
                           value = round(input$location_map_center$lng, 3))
        updateNumericInput(session = session,
                           inputId = "lat",
                           value = round(input$location_map_center$lat, 3))
    })
    
    observeEvent(input$location, {
        if(input$location == "geo") {
            geom <- local_geo()$geometry 
            cl <- "red" 
        } else {
            geom <- local_veg()$geometry
            cl <- "green"
        }
        poly <- geom %>%
            st_cast(to = "POLYGON") %>%
            st_sf()
        
        leafletProxy("location_map", session) %>%
            clearShapes() %>%
            addPolygons(data = poly, color = cl)
    })
    
    observeEvent(input$load, {
        validate(need(input$select, message = FALSE))
        
        query <- paste('SELECT track_seg_point_id, track_fid, ele, dis, id, time, geology, vegetation, geometry',
                        'FROM points WHERE track_fid =', input$select,
                       'ORDER BY track_seg_point_id')
        t <- st_read(con, query = query, geometry_column = "geometry")
        
        track$track_fid <- t$track_fid[1]
        track$track_seg_point_id <- t$track_seg_point_id
        track$ele <- t$ele
        track$dis <- t$dis
        track$id <- t$id
        track$time <- as.POSIXct(t$time, origin = origin, tz = "Australia/Victoria")
        track$geology <- t$geology
        track$vegetation <- t$vegetation
        track$geometry <- t$geometry
        
        
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

        updateTextInput(session, "name", label = "Save name:", value = input$gpx$name)
        
        input$gpx
    })
    
    
    observeEvent(input$save, {
        validate(need(input$name, message = "track needs a name to save"))
        
        start <-as.character(as.POSIXlt(track$time[1], tz= "Australia/Victoria", origin = origin))
        end <- as.character(as.POSIXlt(track$time[length(track$time)], tz= "Australia/Victoria", origin = origin))
        
        bb<-st_bbox(track$geometry)
        
        #db_track<-data.frame(name = input$name, min_ele = min(track$ele), max_ele = max(track$ele), start = as.POSIXct(track$time[1], tz= "Australia/Victoria", origin = origin), end = as.POSIXct(track$time[nrow(t)], tz= "Australia/Victoria", origin = origin))
        sfc <- st_sfc(st_point(c(bb[1],bb[2])),st_point(c(bb[3],bb[4])))
        geom <- st_combine(sfc)
        st_crs(geom)<-crs
        
        q <- gsub("'  '", "NULL", gsub("NA", "",paste('INSERT INTO tracks("name", "min_ele", "max_ele", "start", "end", "geometry")',
              'VALUES (', 
              paste(paste("'",input$name,"'", sep=''), 
                    min(track$ele, na.rm = TRUE), max(track$ele, na.rm = TRUE), 
                    paste("'", start, "'", spe=''), 
                    paste("'", end, "'", spe=''),  
                    paste("'",st_astext(geom),"'",sep=''), sep = ', '),
              '::geometry)', 
              'RETURNING id;')))
        
        
        id <- dbGetQuery(con, q)
        
        
        track$track_fid<-id[1,]
        #time<-as.POSIXct(track$time, tz= "Australia/Victoria", origin = origin)
        
        n <- length(track$track_seg_point_id)
        
        db_points <- data.frame(track_seg_point_id = track$track_seg_point_id, 
                                ele = track$ele,
                                dis = track$dis,
                                track_fid = rep(id[1,], n),
                                time = as.POSIXct(track$time, tz = "Australia/Victoria", origin = origin)
                                )
        
        
        # if(is.na(track$time))
        #     db_points$time <- rep(NA,n)
        # else
        #     db_points$time <- track$time
        
        if(is.na(track$geology))
            db_points$geology <- rep(NA, n)
        else
            db_points$geology <- track$geology
        
        if(is.na(track$vegetation))
            db_points$vegetation <- rep(NA,n)
        else
            db_points$vegetation <- track$vegetation
        
        st_geometry(db_points) <- track$geometry
        
        
        st_write(db_points, con, layer = "points", append = TRUE)
        
        query <- paste('SELECT id, name FROM tracks;')
        names <- dbGetQuery(con, query)
        n <- names$id
        names(n)<-names$name
        updateSelectizeInput(session, "select", choices = n, selected = id)
    })
    
    #Load gpx file and calculate the cumulative distance
    observe({
        validate(need(userFile(), message = FALSE))
        u <- userFile()

        trail <- data.frame()
        trail <- sf::st_read(u$datapath, layer = "track_points", quiet = TRUE)
        
        trail<-st_transform(trail, crs)

        d<-sf::st_distance(trail)
        trail$dis <- as.numeric(cumsum(d[1,]))/1000
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
        
        track$track_fid <- NA
        track$track_seg_point_id <- trail$track_seg_point_id
        track$ele <- trail$ele
        track$dis <- trail$dis
        track$id <- NA
        track$time <- trail$time
        track$geology <- NA
        track$vegetation <- NA
        track$geometry <- trail$geometry
    })
    
    #When the geometry changes update the map
    observe({
        validate(need(track$geometry, message = FALSE))
        
        bb <- as.numeric(st_bbox(track$geometry))
        
        t <- track$geometry %>%
            st_combine() %>%
            st_cast(to = "LINESTRING") %>%
            st_sf()

        leafletProxy("file_map", session) %>%
            clearShapes() %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            addPolylines(data = t)
        
    }, priority = 1)
    
    
    start <- reactive({
        
        t<-track$time
        if(all(is.na(t)))
        {
            start <- "Time missing from file"
        } else {
            start <- as.character(as.POSIXct(min(t, na.rm = TRUE), origin = origin))
        }
        start
    })
    
    end <- reactive({
        
        t<-track$time
        if(all(is.na(t)))
        {
            end <- " "
        } else {
            end <- as.character(as.POSIXct(max(t, na.rm = TRUE), origin = origin))
        }
        end
    })
    

    total_distance <- reactive({
        validate(need(track$dis, message = FALSE))

        track$dis[length(track$dis)]
    })
    
    
    #Find the polygons that the gpx file pass through and group them into blocks
    geo_track <- reactive({
        validate(need(track$track_seg_point_id, message = FALSE))
        
        if(all(is.na(track$geology))){
            if(is.na(track$track_fid)){
                n <- length(track$track_seg_point_id)
                
                db_points <- data.frame(track_seg_point_id = track$track_seg_point_id, 
                                        ele = track$ele,
                                        dis = track$dis,
                                        time = as.POSIXct(track$time, tz = "Australia/Victoria", origin = origin)
                )
                
                st_geometry(db_points) <- track$geometry
                st_write(db_points, con, "#temptrack")
                
                q <- paste('ALTER TABLE "#temptrack"',
                           'ADD geology BIGINT;')
                dbSendQuery(con, q)
                           
                q <- paste('UPDATE "#temptrack" AS tt',
                           'SET geology = ge.id',
                           'FROM "SG_GEOLOGICAL_UNIT_250K" AS ge',
                           'WHERE ST_Intersects(tt.geometry::geometry, ge.geometry);')
                dbSendQuery(con, q)
                
                q <- paste('SELECT geology',
                           'FROM "#temptrack";')
                tid <- dbGetQuery(con, q)
                
                track$geology <- tid$geology
            } else {
                #If saved but unlinked Link points to polygons in database
                q <- paste('UPDATE points',
                           'SET geology = ge.id',
                           'FROM "SG_GEOLOGICAL_UNIT_250K" AS ge',
                           'WHERE points.track_fid =', track$track_fid,
                           'AND ST_Intersects(points.geometry::geometry, ge.geometry);')
                
                dbSendQuery(con, q)
                
                q <- paste('SELECT geology',
                           'FROM points',
                           'WHERE track_fid =', track$track_fid,';')
                
                tid <- dbGetQuery(con, q)
                
                track$geology <- tid$geology
            }
        }
        
        q<-paste('SELECT g.id, g.name, g.desc, g.lithology, g.rank, g.geolut, g.geolhist, g.geolut_uri, g.repage_uri, g.replit_uri',
                 'FROM "SG_GEOLOGICAL_UNIT_250K" AS g',
                 'WHERE id IN (', paste(na.omit(unique(track$geology)), collapse = ', '), ');')
        
        geo <- dbGetQuery(con,q)
    
        geology_data <- left_join(data.frame(geology = track$geology), geo, by = c("geology" = "id"))
        
        n <- nrow(geology_data)
        geology_data$grp <- 1

        d<-geology_data$name[1]
        grp<-1
        for(i in 1:(n-1)){
            if(!(d %in% geology_data$name[i+1])) {
                grp <- grp+1
                d <- geology_data$name[i+1]
            }
            geology_data$grp[i+1] <- grp
        }
        
        geology_data$dis <- track$dis
        geology_data$ele <- track$ele
        geology_data$time <- track$time
        geology_data$name <- as.factor(geology_data$name) 
        
        geology_data
    })
    
    geo_cl <- reactive({
        validate(need(geo_track(), message = FALSE))
        
        g <- geo_track()
        
        v <- viridis(length(levels(g$name)), option = "turbo")
        names(v) <- levels(g$name)
        
        v
    })
    
    
    #Find the polygons that the gpx file pass through and group them into blocks
    veg_track <- reactive({
        validate(need(track$track_seg_point_id, message = FALSE))
        
        if(all(is.na(track$vegetation))){
            if(is.na(track$track_fid)){
                n <- length(track$track_seg_point_id)
                
                db_points <- data.frame(track_seg_point_id = track$track_seg_point_id, 
                                        ele = track$ele,
                                        dis = track$dis,
                                        time = as.POSIXct(track$time, tz = "Australia/Victoria", origin = origin)
                )
                
                st_geometry(db_points) <- track$geometry
                st_write(db_points, con, "#temptrack")
                
                q <- paste('ALTER TABLE "#temptrack"',
                           'ADD vegetation BIGINT;')
                dbSendQuery(con, q)
                
                q <- paste('UPDATE "#temptrack" AS tt',
                           'SET vegetation = ve.id',
                           'FROM "NV2005_EVCBCS" AS ve',
                           'WHERE ST_Intersects(tt.geometry::geometry, ve.geometry);')
                dbSendQuery(con, q)
                
                q <- paste('SELECT vegetation',
                           'FROM "#temptrack";')
                tid <- dbGetQuery(con, q)
                
                track$vegetation <- tid$vegetation
            } else {
                # Link points to polygons in database
                q <- paste('UPDATE points',
                           'SET vegetation = ve.id',
                           'FROM "NV2005_EVCBCS" AS ve',
                           'WHERE points.track_fid =', track$track_fid,
                           'AND ST_Intersects(points.geometry::geometry, ve.geometry);')
                dbSendQuery(con, q)
                
                q <- paste('SELECT vegetation',
                           'FROM points',
                           'WHERE track_fid =', track$track_fid[1],';')
                
                tid <- dbGetQuery(con, q)
                
                track$vegetation <- tid$vegetation
            }
        }

        q<-paste('SELECT v.id, v.x_evcname, v.xgroupname, v.xsubggroup, v.evcbcsdesc, v.bioregion, v.veg_code',
                 'FROM "NV2005_EVCBCS" AS v',
                 'WHERE id IN (', paste(na.omit(unique(track$vegetation)), collapse = ', '), ');')
        
        veg <- dbGetQuery(con,q)
        
        vegetation_data <- left_join(data.frame(vegetation = track$vegetation), veg, by = c("vegetation" = "id"))
        
        n <- nrow(vegetation_data)
        vegetation_data$grp <- 1
        
        d<-vegetation_data$vegetation[1]
        grp<-1
        for(i in 1:n-1){
            if(!(d %in% track$vegetation[i+1])) {
                grp<-grp+1
                d<-track$vegetation[i+1]
            }
            vegetation_data$grp[i+1] <- grp
        }
        
        vegetation_data$dis <- track$dis
        vegetation_data$ele <- track$ele
        vegetation_data$time <- track$time
        vegetation_data$x_evcname <- as.factor(vegetation_data$x_evcname) 

        vegetation_data
    })
    

    veg_cl <- reactive({
        validate(need(veg_track(), message = FALSE))
        t <- veg_track()
        
        v <- viridis(length(unique(t$x_evcname)), option = "turbo")
        names(v) <- unique(t$x_evcname)
        
        v
    })
                            

    observeEvent(input$x_axis,{
        x_axis <<- input$x_axis
        
        if(x_axis == 'd') {
            validate(need(d <- total_distance(), message = FALSE))
            xmin <<- 0
            xmax <<- ceiling(max(d))
            updateSliderInput(session, "range", value = c(xmin, xmax), min = 0, max = xmax)
            
        } else {
            validate(need(track$time, message = FALSE))
            xmin <<- min(track$time, na.rm = TRUE)
            xmax <<- max(track$time, na.rm = TRUE)
            updateSliderInput(session,
                              "range",
                              value = as.POSIXct(c(xmin, xmax), origin = origin),
                              min = as.POSIXct(xmin, origin = origin),
                              max = as.POSIXct(xmax, origin = origin))
        }
    })
    
    
    # Pop up info window when the plot is double clicked
    observeEvent(input$geo_dbl_click, {
        validate(need(input$geo_dbl_click, message = FALSE))
        
        x <<- input$geo_dbl_click$x
        showModal(geoModal())
    })
    
    
    # Pop up info window when the plot is double clicked
    observeEvent(input$veg_dbl_click, {
        validate(need(input$veg_dbl_click, message = FALSE))
        
        x <<- input$veg_dbl_click$x
        showModal(vegModal())
    })
    
    observeEvent(input$geo_click, {
        validate(need(input$geo_click, message = FALSE),
                 need(t <- geo_track(), message = FALSE))
        
        x <<- input$geo_click$x
        
        if(x_axis == 'd')
        {
            s <- track$geology[track$dis>=x]    
        } else {
            s <- track$geology[track$time>=x]   
        }
        
        if(!is.na(s[1])){
            q <- paste('SELECT geometry, name',
                       'FROM "SG_GEOLOGICAL_UNIT_250K"',
                       'WHERE id =', s[1],';')
            
            g <- st_read(con, query = q)
            
            poly <- g %>%
                 st_cast(to = "POLYGON") %>%
                 st_sf()

            line <- track$geometry %>% 
                sf::st_combine() %>%
                sf::st_cast(to = "LINESTRING", warn = FALsE) %>%
                sf::st_sf()
            
            leafletProxy("file_map", session) %>%
                clearShapes() %>%
                addPolylines(data = line) %>%
                addPolygons(data = poly, color = as.character(geo_cl()[g$name]))
        }
    })
    
    observeEvent(input$veg_click, {
        validate(need(input$veg_click, message = FALSE),
                 need(t <- veg_track(), message = FALSE))
        
        x <<- input$veg_click$x
        
        if(x_axis == 'd')
        {
            s <- track$vegetation[track$dis>=x]    
        } else {
            s <- track$vegetation[track$time>=x]   
        }
        
        if(!is.na(s[1])){
            q <- paste('SELECT geometry, X_evcname',
                       'FROM "NV2005_EVCBCS"',
                       'WHERE id =', s[1],';')
            
            g <- st_read(con, query = q)
            
            poly <- g %>%
                st_cast(to = "POLYGON") %>%
                st_sf()
            
            line <- track$geometry %>% 
                sf::st_combine() %>%
                sf::st_cast(to = "LINESTRING", warn = FALsE) %>%
                sf::st_sf()
            
            leafletProxy("file_map", session) %>%
                clearShapes() %>%
                addPolylines(data = line) %>%
                addPolygons(data = poly, color = as.character(veg_cl()[g$x_evcname]))
        }
    })
    
    
    # Update the the slider to match the input file and x-axis selection
    observe({
        validate(need(track$dis, message = FALSE))

        if(x_axis == 'd') {
            d <- track$dis
            xmin <<- 0
            xmax <<- ceiling(max(d))
            updateSliderInput(session, "range", value = c(xmin, xmax), min = 0, max = xmax)
        } else {
            xmin <<- min(track$time, na.rm = TRUE)
            xmax <<- max(track$time, na.rm = TRUE)
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
        
        if(x_axis == 'd') {
            xmin <<- max(val$xmin, 0)
            xmax <<- min(val$xmax, max(track$dis))
            updateSliderInput(session, "range",  value = c(xmin,xmax))
            
        } else {
            r <- range(t$time, na.rm = TRUE)
            xmin <<- max(as.numeric(val$xmin), min(track$time, na.rm = TRUE))
            xmax <<- min(as.numeric(val$xmax), max(track$time, na.rm = TRUE))
            updateSliderInput(session, "range", value = as.POSIXct(c(xmin, xmax), origin = origin))
            
        }
    })
    

    #Update the plot when the selected range changes
    observeEvent(input$range, {
        validate(need(input$range, message = FALSE), need(track$geometry, message = FALSE))
        
        if(x_axis == 'd'){
            xmin <<- max(input$range[1], 0)
            xmax <<- min(input$range[2], max(track$dis))

            start <- st_coordinates(track$geometry[track$dis >= xmin][1])
            end <- st_coordinates(track$geometry[track$dis >= xmax][1])
        } else {

            xmin <<- max(as.numeric(input$range[1]), min(track$time, na.rm = TRUE))
            xmax <<- min(as.numeric(input$range[2]), max(track$time, na.rm = TRUE))

            start <- st_coordinates(track$geometry[track$time >= xmin][1])
            end <- st_coordinates(track$geometry[track$time >= xmax][1])
        }
        


        # Also show the start and end points on map
        leafletProxy("file_map", session) %>%
            clearMarkers() %>%
            addCircleMarkers(lng = start[1], lat = start[2], color = "green", fillOpacity = 0.5) %>%
            addCircleMarkers(lng = end[1], lat = end[2], color = "red", fillOpacity = 0.5)

    })
    
    
    # Render the geo plot
    output$geoPlot <- renderPlot({
        validate(need(geo_track(), message = FALSE))
        validate(need(input$range, message = FALSE))
        
        session$resetBrush("plot_brush")
        
        t <- geo_track()

        if(x_axis == 'd') {
           gg <- ggplot(data = t, aes(group = grp, fill = name)) + 
                geom_ribbon(aes(x=dis, ymax = ele, ymin =-10)) + 
                #geom_line(aes(y=ele, color = factor(name), width)) +
                theme(legend.position="bottom", legend.direction = "horizontal") + 
                labs(x ="Distance (m)", y = "Elevation (m)") +
                xlim(xmin, xmax) + 
                scale_fill_manual(values = geo_cl(), name = "") + 
                guides(color=guide_legend(nrow = 3))
        } else {
            
            gg <- ggplot(data = t, aes(fill = name, group = grp)) + 
                geom_ribbon(aes(x = as.POSIXct(time, origin = origin, tz = "Australia/Victoria"), ymin = -10, ymax = ele)) + 
                theme(legend.position="bottom", legend.direction = "horizontal") + 
                labs(x ="Time", y = "Elevation (m)") +
                xlim(as.POSIXct(xmin, origin = origin, tz = "Australia/Victoria"), as.POSIXct(xmax, origin = origin, tz = "Australia/Victoria"))  + 
                scale_fill_manual(values = geo_cl(), name = "") + 
                guides(color=guide_legend(nrow = 3))
        }
        gg
    })
    
    
    output$vegPlot <- renderPlot({
        validate(need(veg_track(), message = FALSE))
        validate(need(input$range, message = FALSE))
        
        session$resetBrush("plot_brush")
        
        t <- veg_track()
        
        if(x_axis == 'd') {
            gg <- ggplot(data = t, aes(fill = x_evcname, group = grp)) + 
                geom_ribbon(aes(x = dis, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "horizontal") + 
                labs(x ="Distance (m)", y = "Elevation (m)") +
                xlim(xmin, xmax) + 
                scale_fill_manual(values = veg_cl(), name = "") +
                guides(color=guide_legend(nrow = 3))
        } else {
            geo <- t[!is.na(t$time),]
            gg <- ggplot(data = t, aes(fill = x_evcname, group = grp)) + 
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
        t <- geo_track()

        if(x_axis == 'd'){
            dat <- t[track$dis>=x,][1,]
        } else {
            dat <- t[track$time>=x,][1,]
        }
        
        leaf_out <- leafletOutput("geo_pop_map")
        
        if(!is.null(leaf_out)){
            modalDialog(
                tags$div(tags$b("Name: "), dat$name, tags$br(), 
                         tags$b("Type: "), dat$geolut, tags$br(),
                         tags$b("Rank: "), dat$rank, tags$br(),
                         tags$b("Lithology: "), dat$lithology, tags$br(), 
                         tags$b("Description: "), dat$desc, tags$br(), 
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
        t <- veg_track()

        if(x_axis == 'd'){
            dat <- t[t$dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        leaf_out <- leafletOutput("veg_pop_map")
        
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
    
    
    output$geo_pop_map <- renderLeaflet({
        c <- input$geo_click
        
        t <- geo_track()
        
        if(x_axis == 'd'){
            dat <- t[track$dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        line <- track$geometry[t$grp==dat$grp]
        
        bb <- as.numeric(st_bbox(line))
        
        line <- line %>% 
            sf::st_combine() %>%
            sf::st_cast(to = "LINESTRING", warn = FALsE) %>%
            sf::st_sf()
        
        # gsf <- geo()
        # 
        # 
        # poly <- gsf[gsf$id %in% dat$id,]$geometry
        # 
        # poly <- poly %>%
        #     st_cast(to = "POLYGON") %>%
        #     st_sf()
        
        pop_map %>%
            clearShapes() %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            #addPolygons(data = poly, fillColor = as.character(geo_cl()[dat$name.y[1]]), fillOpacity =  0.3, stroke = FALSE) %>%
            addPolylines(data = line)  
    })
    
    output$veg_pop_map <- renderLeaflet({
        c <- input$veg_click
        
        t <-veg_track()
        
        if(x_axis == 'd'){
            dat <- t[track$dis>=x,][1,]
        } else {
            dat <- t[track$time>=x,][1,]
        }
        
        line <- track$geometry[t$grp==dat$grp]
        
        bb <- as.numeric(st_bbox(line))
        
        line <- line %>% 
            sf::st_combine() %>%
            sf::st_cast(to = "LINESTRING", warn = FALsE) %>%
            sf::st_sf()
        
        
        # vsf <- st_read()
        # 
        # 
        # poly <- vsf[vsf$id %in% dat$id,]$geometry
        # 
        # poly <- poly %>%
        #     st_cast(to = "POLYGON") %>%
        #     st_sf()
        
        pop_map %>%
            clearShapes() %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            #addPolygons(data = poly, fillColor = as.character(veg_cl()[dat$x_evcname[1]]), fillOpacity =  0.3, stroke = FALSE) %>%
            addPolylines(data = line)  
    })
    
    output$info <- renderUI({ 
        #validate(need(start(), message = FALSE), 
        #         need(end(), message = FALSE), 
        #        need(total_distance(), message = FALSE), 
        #        )
        
        tags$div(tags$b("Start: "), start(), tags$br(), 
                 tags$b("End: "), end(), tags$br(), 
                 tags$b("Total distance: "), round(total_distance()), "m")
        
        
    })
    
    output$save <- renderUI({
        validate(need(track$geometry, message = FALSE))
        
        actionButton("save", "Save")
    })
    
    
    
    output$geoStats <- renderPlot({
        validate(need(t <- geo_track(), message = "Load a track file to see statistics"))
        r <- input$range
        
        if(x_axis == 'd'){
            dat <- t[track$dis>=xmin & track$dis<=xmax ,]
        } else {
            dat <- t[track$time>=xmin & track$time<=xmax,]
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
        
        ggplot(dat, aes(x=name, fill = name)) + 
            geom_bar(stat="count") + 
            theme(axis.text.x = element_text(angle = 90), legend.position="none") +
            xlab(element_blank()) +
            scale_fill_manual(values = geo_cl())
    })
        
    output$vegStats <- renderPlot({
        validate(need(t <- veg_track(), message = "load a track file to see statistics"))
        r <- input$range
        
        if(x_axis == 'd'){
            dat <- t[track$dis>=xmin & track$dis<=xmax,]
        } else {
            dat <- t[track$time>=xmin & track$time<=xmax,]
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
    
    
    local_geo <- reactive({
        q <- paste('SELECT *',
                   'FROM "SG_GEOLOGICAL_UNIT_250K" AS ge',
                   "WHERE ST_INTERSECTS(ge.geometry, 'SRID=7844;POINT(", input$lng, input$lat, ")'::geometry)"
        )
        
        st_read(con, query = q)
    })
    
    
    local_veg <- reactive({
        q <- paste('SELECT *',
                   'FROM "NV2005_EVCBCS" AS ve',
                   "WHERE ST_INTERSECTS(ve.geometry, 'SRID=7844;POINT(", input$lng, input$lat, ")'::geometry)"
        )
        
        veg <- st_read(con, query = q)
    })
    
    
    output$local_geology <- renderUI({
        g <- local_geo()
        
        tags$div(tags$b("Name: "), g$name, tags$br(),
                    tags$b("Description: "), g$desc, tags$br(),
                    tags$b("Formation: "), g$geolut,tags$br(),
                    tags$b("Lithology: "), g$lithology,tags$br(),
                    tags$b("History: "), g$geolhist,tags$br()
                           
        )
    })
    
    
    output$local_vegetation <- renderUI({
        v <- local_veg()
        
        name <- v$x_evcname
        if(length(name) == 0)
           name <- "No native vegetation recorded here"
        
        tags$div(tags$b("Name: "), name, tags$br(), 
                 tags$b("Subgroup: "), v$xsubggroup, tags$br(),
                 tags$b("Group: "), v$xgroupname, tags$br(),
                 tags$b("Bioregion: "), v$bioregion, tags$br(), 
                 tags$b("Status: "), v$evcbcsdesc)
                 
        
    })
    
    
    observeEvent(input$location_map_click,{
        updateNumericInput(session = session,
                           inputId = "lng",
                           value = round(input$location_map_click$lng, 3))
        updateNumericInput(session = session,
                           inputId = "lat",
                           value = round(input$location_map_click$lat, 3))
    })
    
    
    observeEvent(input$lng | input$lat, {
        if(input$location == "geo") {
            geom <- local_geo()$geometry 
            cl <- "red" 
        } else {
            geom <- local_veg()$geometry
            cl <- "green"
        }
        poly <- geom %>%
            st_cast(to = "POLYGON") %>%
            st_sf()
        
        leafletProxy("location_map", session) %>%
            clearShapes() %>%
            addPolygons(data = poly, color = cl)
    })
    
})


    
