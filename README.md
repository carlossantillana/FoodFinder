# FoodFinder
Instructions on how to run
* Download R: https://cran.r-project.org/mirrors.html
* Download Rstudio: https://www.rstudio.com/products/rstudio/download/
* clone project
* get google api key to use ggmap https://developers.google.com/maps/documentation/geocoding/get-api-key
* get yelp api key https://www.yelp.com/developers/documentation/v3/authentication
* create file .Renviron at root directory, `touch ~/.Renviron`
* add api keys to .Renviron like shown below (without brackets)
```
google = <insert api key>
yelp = Bearer <insert api key>
```
* Open Rstudio and in console within Rstudio run this command to download necessary libraries
`install.packages(c("keyring","plyr","dplyr","ggmap","RColorBrewer","jsonlite","httr","tidyr","reshape","shinythemes","crul"))`

* Open Rstudio and open app.R then click Run App
