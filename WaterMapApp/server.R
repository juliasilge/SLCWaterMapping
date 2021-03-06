library(choroplethrUTCensusTract)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(sp)
library(stringr)
library(lubridate)
library(reshape2)

# read the data from the file and clean up
water <- read.csv("./SALT_LAKE_CITY_-_WATER_CONSUMPTION_BY_TRACT.csv", 
                  stringsAsFactors = FALSE)
water <- water[grep("[0-9]{4}", water$YEAR),]
water <- water[!is.na(water$CONSUMPTION),]
water[,1:4] <- lapply(water[,1:4], as.factor)
water[,5:6] <- lapply(water[,5:6], as.numeric)
# make long/tidy data frame with median monthly water use for each census tract
tidytracts <- water %>% 
        group_by(TRACT, YEAR, month = month(as.integer(MONTH), label = TRUE)) %>% 
        summarise(consumption = sum(CONSUMPTION)) %>%
        group_by(TRACT, month) %>%
        summarise(consumption = median(consumption))
colnames(tidytracts) <- c("tract", "month", "consumption")
# make wide data frame
maptracts <- dcast(tidytracts, tract ~ month)
# region column is needed for mapping in choroplethr; needs county FIPS code
maptracts$region <- str_c("49035", as.character(maptracts$tract))
maptracts$value <- maptracts$Mar
monthvector <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

# make data frame for finding census tract ID from long and lat
data(ut.tract.map)
tractstest <- select(ut.tract.map, long, lat, id, TRACTCE)


shinyServer(function(input, output) {
        output$myPlot <- renderPlot({
                monthint <- which(monthvector == input$month)
                maptracts$value <- maptracts[,monthint + 1]
                choro = UtTractChoropleth$new(maptracts)
                choro$set_num_colors(1)
                choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
                choro$set_zoom_tract(tract_zoom = maptracts$region, county_zoom = NULL)
                choro$ggplot_scale = scale_fill_gradientn(name = "Water Use\n(100 cubic ft)",
                                                          colours = brewer.pal(8, "PuRd"))
                if (input$refmap) p <- choro$render_with_reference_map()
                else p <- choro$render()
                p
        })
        
        output$tractText <- renderText({
                if(is.null(input$plot_click) || is.na(input$plot_click)) {
                        return()
                }
                for (i in unique(ut.tract.map$id)) {
                        itractstest <- filter(tractstest, id == i)
                        if(point.in.polygon(input$plot_click$x, input$plot_click$y, 
                                            itractstest$long, itractstest$lat)) {
                                mytract <- itractstest$TRACTCE[[1]]
                        }                
                }
#                paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y,
#                       "\nU.S. Census Tract: 49035", mytract, "\n")
                paste0("U.S. Census Tract 49035", mytract, "\n")
        })        
})
