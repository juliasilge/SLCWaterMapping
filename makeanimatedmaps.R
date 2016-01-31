library(choroplethrUTCensusTract)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)

make_map <- function(df, month, monthstring) {
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

water <- read.csv("../SALT_LAKE_CITY_-_WATER_CONSUMPTION_BY_TRACT.csv", 
                  stringsAsFactors = FALSE)
water <- water[grep("[0-9]{4}", water$YEAR),]
water <- water[!is.na(water$CONSUMPTION),]
water[,1:4] <- lapply(water[,1:4], as.factor)
water[,5:6] <- lapply(water[,5:6], as.numeric)
tidytracts <- water %>% 
        group_by(TRACT, YEAR, month = month(as.integer(MONTH), label = TRUE)) %>% 
        summarise(consumption = sum(CONSUMPTION)) %>%
        group_by(TRACT, month) %>%
        summarise(consumption = median(consumption))
colnames(tidytracts) <- c("tract", "month", "consumption")
maptracts <- dcast(tidytracts, tract ~ month)
maptracts$region <- str_c("49035", as.character(maptracts$tract))
monthvector <- c("January", "February", "March", "April", "May", "June",
                        "July", "August", "September", "October", "November", "December")

for(i in seq_along(monthvector)) make_map(maptracts, i, monthvector[i])

# command line code to use ImageMagick to make animated GIF:
# convert *.png -set delay 30 -loop 0 watermap.gif