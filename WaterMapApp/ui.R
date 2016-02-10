monthvector <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")


shinyUI(fluidPage(
        titlePanel("Water Use in Salt Lake City Census Tracts"),
        
        sidebarLayout(
                sidebarPanel("This map shows the median monthly water use in 
                             census tracts in Salt Lake City.",
                             br(),
                             br(),
                             "Choose which month to display and whether to plot 
                             the data over a reference map. Click on the map
                             get the ID of that census tract.",
                             br(),
                             br(),
                             "The data used in 
                             these plots are publicly available at ", 
                             a("Utah's Open Data Catalog.", 
                               href = "https://opendata.utah.gov/Energy-Environment/SALT-LAKE-CITY-WATER-CONSUMPTION-BY-TRACT/j4aa-ce7s"),
                             "Read more about this analysis at ",
                             a("my blog.",
                               href = "http://juliasilge.com/blog/"),
                             br(),
                             br(),
                             selectInput('month', 'Month', monthvector,
                                         selected = monthvector[3]),
                             checkboxInput('refmap', label = 'Reference map', value = FALSE)
                ),
                mainPanel(
                        plotOutput('myPlot', click = 'plot_click'),
                        br(),
                        textOutput("tractText")
                )
)
))