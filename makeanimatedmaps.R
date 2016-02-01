library(choroplethrUTCensusTract)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)

 
MakeMap <- function(df, month, monthstring) {
# Makes a choropleth map of water use data in a given month and writes it to PNG
# Args:
# df:          Wide format data frame containing the water use data with 
#              region column and 12 month columns.
# month:       Integer 1 to 12 representing the month
# monthstring: string containing the month's name, i.e. "January"
        df$value <- df[,month+1]
        if (month < 10) {myFilename <- str_c("./watermap00", month, ".png")}
        if (month >= 10) {myFilename <- str_c("./watermap0", month, ".png")}
        png(filename = myFilename, width = 600, height = 400)
        choro = UtTractChoropleth$new(df)
        choro$title = str_c("Annual Water Use in Salt Lake Census Tracts\n", monthstring)
        choro$set_num_colors(1)
        choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
        choro$set_zoom_tract(tract_zoom = df$region, county_zoom = NULL)
        choro$ggplot_scale = scale_fill_gradientn(colours = brewer.pal(8, "PuRd"))
        p <- choro$render() + theme(legend.position="none")
        print(p)
        dev.off()
}

# read the data from the file and clean up
water <- read.csv("../SALT_LAKE_CITY_-_WATER_CONSUMPTION_BY_TRACT.csv", 
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
monthvector <- c("January", "February", "March", "April", "May", "June",
                        "July", "August", "September", "October", "November", "December")

# call MakeMap function for each month; make 12 PNG choropleth maps
for(i in seq_along(monthvector)) MakeMap(maptracts, i, monthvector[i])

# command line code to use ImageMagick to make animated GIF:
# convert *.png -set delay 30 -loop 0 watermap.gif