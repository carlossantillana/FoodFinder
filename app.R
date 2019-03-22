file.edit("~/.Renviron")
library(keyring)
library (plyr)
library(dplyr)
library(ggmap)
library(RColorBrewer)
library(jsonlite)
library(httr)
library(tidyr)
library(reshape)
library(shinythemes)
library("crul")
register_google(Sys.getenv("google"))
# Define UI ----
ui <- navbarPage("Food Finder",
   tabPanel(textInput(inputId = "location",
             label = "City to search:",
             value = "Los Angeles")
   ),
   
   tabPanel(textInput(inputId = "type",
             label = "Type Of Food:",
             value = "")
   ),
   
   tabPanel(
   sliderInput("zoom", "Zoom:",
               min = 9, max = 15, value = 12)
   ),
   
   tabPanel(actionButton("search", "Search")),
   
   theme = shinytheme("cerulean"),
   
   tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
   ),
    mainPanel(plotOutput(outputId = "map", height = "80vh")),
    collapsible = FALSE
)

# Define server logic ----
server <- function(input, output) {
  first <- reactiveVal(TRUE)
  mapLon <- reactiveVal(NULL)
  mapLat <- reactiveVal(NULL)

  observeEvent(input$search, {
    first(FALSE)
  })
  reactiveQuery <- eventReactive(input$search, {
    location <- geocode(input$location, output = "latlon")
    cat(file=stderr(), toString(location), "\n")
    mapLon(location$lon)
    mapLat(location$lat)
    map <- get_map(location = c(lon = mapLon(), lat = mapLat()), source = "google", maptype = "roadmap", zoom = input$zoom)
    mapAt <- attr(map, 'bb')
    mapbb <- list("city"= "Los Angeles", 
                  "lowerLeft" = c(mapAt$ll.lat, mapAt$ll.lon), "lowerRight"=c(mapAt$ll.lat, mapAt$ur.lon), "upperLeft" =c(mapAt$ur.lat, mapAt$ll.lon), "upperRight"=c(mapAt$ur.lat, mapAt$ur.lon))
    mapbb
    cols <- seq(mapbb$lowerLeft[2], mapbb$lowerRight[2], length.out = 3)
    rows <- seq(mapbb$lowerLeft[1], mapbb$upperRight[1], length.out = 3)
    mapGrid <- expand.grid(x = cols, y = rows)
    numQueries <- 9
    foodType <- "mexican"
    offset <- 0;
    crul_settings(TRUE)
    set_headers(`Authorization` = Sys.getenv("yelp"))
    
    url <- sapply(1:numQueries, function(x) paste0("https://api.yelp.com/v3/businesses/search?term=", input$type ,"&latitude=", mapGrid$y[x] ,"&longitude=", mapGrid$x[x],"&limit=50&radius=4828&sort_by=rating"))
    url <- sapply (1:numQueries, function (x) URLencode(url[x]))
    (cc <- Async$new(
      urls = c(
        url[2],
        url[4],
        url[5],
        url[6],
        url[8]
      )))
    res <- cc$get()

    shiny::validate(
      need(fromJSON(res[[1]]$parse("UTF-8"))$total != 0, "Please a valid city or food")
    )
    cleanResp <- as.data.frame(fromJSON(res[[1]]$parse("UTF-8"), flatten = TRUE))
    cleanResp2 <- as.data.frame(fromJSON(res[[2]]$parse("UTF-8"), flatten = TRUE))
    cleanResp3 <- as.data.frame(fromJSON(res[[3]]$parse("UTF-8"), flatten = TRUE))  
    cleanResp4 <- as.data.frame(fromJSON(res[[4]]$parse("UTF-8"), flatten = TRUE))
    cleanResp5 <- as.data.frame(fromJSON(res[[5]]$parse("UTF-8"), flatten = TRUE))

    foodDF <- rbind(cleanResp, cleanResp2, cleanResp3, cleanResp4, cleanResp5)
    foodDF <- foodDF %>% distinct(businesses.location.address1, .keep_all = TRUE)
    foodDF <- foodDF %>% filter((abs(businesses.coordinates.latitude) >= abs(mapbb$lowerRight[1])) & (abs(businesses.coordinates.latitude) <= abs(mapbb$upperLeft[1])) & ( abs(businesses.coordinates.longitude) >= abs(mapbb$lowerRight[2])) & (abs(businesses.coordinates.longitude) <= abs(mapbb$lowerLeft[2])) )
    businessWeighted <- with(foodDF, foodDF[rep(1:nrow(foodDF), businesses.rating*50),])
  })  
  
  reactiveMap <- eventReactive(input$search, {
    get_map(location = c(lon = mapLon(), lat = mapLat()), source = "google", maptype = "roadmap", zoom = input$zoom)
  })

  output$map <- renderPlot({
    if (first()){
      map <- get_map(location = c(lon = -118.243683, lat = 34.052235), source = "google", maptype = "roadmap", zoom = 12)
      ggmap(map, extent = "device") +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
        xlab('') +
        ylab('')
    }
    else {

    queryData <- reactiveQuery()
    ggmap(reactiveMap(), extent = "normal") +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
      xlab('') +
      ylab('') +
      stat_density2d(data = queryData, aes(x = businesses.coordinates.longitude, y = businesses.coordinates.latitude, fill = ..level.., alpha = ..level..),
                     geom = "polygon", size = 0.01, bins = 50,show.legend = FALSE) +
      scale_fill_gradient(low = "yellow", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = FALSE)
    }
  },
  bg="transparent")
}

# Run the app ----
shinyApp(ui = ui, server = server)
