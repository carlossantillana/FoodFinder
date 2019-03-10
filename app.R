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
library("crul")
register_google(Sys.getenv("google"))
# Define UI ----
ui <- fluidPage(

  
  sidebarLayout(position = "right",
                sidebarPanel(
                  textInput(inputId = "location",
                            label = "City to search:",
                            value = "Chino"),
                  textInput(inputId = "type",
                            label = "Type Of Food:",
                            value = "mexican"),
                  sliderInput("zoom", h3("Zoom"),
                              min = 1, max = 20, value = 12),
                  submitButton("Submit")
                  
                ),
                mainPanel(
                  plotOutput(outputId = "map",
                             width = "100%",
                             height = "100vh",
                             click = FALSE)
                )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  reactiveQuery <- reactive({
    numQueries <- 4
    foodType <- input$type
    foodLocation <- input$location
    offset <- 0;
    crul_settings(TRUE)
    set_headers(`Authorization` = Sys.getenv("yelp"))
    url <- sapply(1:numQueries, function(x) paste0("https://api.yelp.com/v3/businesses/search?term=",foodType ,"&location=", foodLocation ,"&limit=50&offset=", (x-1)*50))
    url <- sapply (1:numQueries, function (x) URLencode(url[x]))
    (cc <- Async$new(
      urls = c(
        url[1],
        url[2],
        url[3],
        url[4]
      )))
    res <- cc$get()
  
    cleanResp <- as.data.frame(fromJSON(res[[1]]$parse("UTF-8"), flatten = TRUE))
    cleanResp2 <- as.data.frame(fromJSON(res[[2]]$parse("UTF-8"), flatten = TRUE))
    cleanResp3 <- as.data.frame(fromJSON(res[[3]]$parse("UTF-8"), flatten = TRUE))
    cleanResp4 <- as.data.frame(fromJSON(res[[4]]$parse("UTF-8"), flatten = TRUE))
    foodDF <- rbind(cleanResp, cleanResp2, cleanResp3, cleanResp4)
    businessWeighted <- with(foodDF, foodDF[rep(1:nrow(foodDF), businesses.rating*50),])
  })
  
  reactiveMap <- reactive({
    queryData <- reactiveQuery()
    get_map(location = c(lon = queryData$region.center.longitude[1], lat = queryData$region.center.latitude[1]), source = "google", maptype = "roadmap", zoom = input$zoom)
  })
  
  output$map <- renderPlot({
    YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
    queryData <- reactiveQuery()
    ggmap(reactiveMap(), extent = "panel") +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
      xlab('') +
      ylab('') +
      stat_density2d(data = queryData, aes(x = businesses.coordinates.longitude, y = businesses.coordinates.latitude, fill = ..level.., alpha = ..level..),
                     geom = "polygon", size = 0.01, bins = 50,show.legend = FALSE) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = FALSE)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)