# [Victoria Shiny Tracks](https://lachland.shinyapps.io/Shiny_Tracks/)
[https://lachland.shinyapps.io/Shiny_Tracks/](https://lachland.shinyapps.io/Shiny_Tracks/)

Website shows the geology and native vegetation of Victoria. 
### Current Location
If location is availbe the website can show the geology or vegeation of the current location.  Alternatively enter and longitude and latitude or click on the map to see the geology or vegetation at that location.

![location](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/location.PNG?raw=true)

### GPX Files
If you have gpx file from a gps device or downloaded from a website, it can be uploaded and the website will show a summary of the geology and vegetation which the track crosses.
If the gpx file contains time the x-axis can be changed to elapsed time.

![zoom](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/zooming.PNG?raw=true)
![zoomed](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/highlighted.PNG?raw=true)
This is displayed and an elevantion plot where different units are dsipalyed as different blocks.  These block can be zoom in on, clicked on to show the unit on the map.

![details](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/details.PNG?raw=true)
Double clicking on one of the blocks will show the details of the geological/vegation unit.

### Statistics
Incomplete. The statistic page will show the number of track point which fall within each type of geology/vegetation for the selected range.

### Search
The search function allows searching the text fields of the database using either keywords, frases or formal query text.  

![search](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/search.PNG?raw=true)
Selecting a result by clicking on it will then be shown on the results in the map tab.

![results](https://github.com/LachlanD/Shiny_Db_Tracks/blob/main/img/results.PNG?raw=true)
clicking the results below the map navigates to the position.


### Code
Written in R using the shiny web development framework hosted on shinyapps.io and postgreSQL/postGIS hosted on AWS.

[https://lachland.shinyapps.io/Shiny_Tracks/](https://lachland.shinyapps.io/Shiny_Tracks/)
