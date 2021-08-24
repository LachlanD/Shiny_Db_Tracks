# [Victoria Shiny Tracks](https://lachland.shinyapps.io/Shiny_Tracks/)
[https://lachland.shinyapps.io/Shiny_Tracks/](https://lachland.shinyapps.io/Shiny_Tracks/)

Website shows the geology and native vegetation of Victoria. 
### Current Location
If location is availbe the website can show the geology or vegeation of the current location.  Alternatively enter and longitude and latitude or click on the map to see the geology or vegetation at that location.

![location](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/location.PNG?raw=true)

### GPX Files
If you have gpx file from a gps device or downloaded from a website, it can be uploaded and the website will show a summary of the geology and vegetation which the track crosses.

This is displayed and an elevantion plot where different units are dsipalyed as different blocks.  These block can be zoom in on, clicked on to show the unit on the map.

![zoom](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/zooming.PNG?raw=true)
![zoomed](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/highlighted.PNG?raw=true)

Double clicking on one of the blocks will show the details of the geological/vegation unit.

![details](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/details.PNG?raw=true)

### Statistics

Incomplete. The statistic page will show the number of track point which fall within each type of geology/vegetation for the selected range.

### Search

The search function allows searching the text fields of the database and display the results on the map.

![search](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/search.PNG?raw=true)
![results](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/results.PNG?raw=true)


### Code
Written in R using the shiny web development framework hosted on shinyapps.io and postgreSQL/postGIS hosted on AWS.
[Victoria Shiny Tracks](https://lachland.shinyapps.io/Shiny_Tracks/)
