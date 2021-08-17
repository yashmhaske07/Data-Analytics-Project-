library(shiny)
library(shinythemes)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
SuburbCardGroup
x <- rnorm(10000,2,1)
y <- c()
for(i in 1:length(x))
{
y[i] <- (1/1+x[i])^2
}
plot(log(x),log(y))


sub_traffic$Lat
map <- leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addMarkers(lng = tail(sort(sub_traffic$Long)),
             lat = tail(sort(sub_traffic$Lat)), 
             popup = tail(sort(sub_traffic$Long)),
             options = markerOptions(draggable = TRUE, riseOnHover = TRUE))

map
suburbVsStopVsYear <- read_xlsx("suburbVsStopVsYear.xlsx")

for(i in 1:nrow(suburbVsStopVsYear))
{
  if(is.na(suburbVsStopVsYear$`Stop Name Short`[i]))
  {
    suburbVsStopVsYear$`Stop Name Short`[i] <- Value
  }
  else
  {
    Value <- suburbVsStopVsYear$`Stop Name Short`[i]
  }
}
write.csv(suburbVsStopVsYear, "suburbVsStopVsYear.csv")

nrow(suburbVsStopVsYear)


Sys.setenv("plotly_username"="gabalepankaj499c")
Sys.setenv("plotly_api_key"="WYi12NKMiWHNzdZMhwMq")
library(plotly)

d <- Suburbcoordinates[sample(nrow(Suburbcoordinates), 1000), ]

p <- plot_ly(
  Suburbcoordinates, x = ~`2015`, y = ~`2016`,
  color = ~`2015`, size = ~`2015`
)
p
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="scatter-colorAndSize")
chart_link


SubvsStopvsCardvsVehicle <- read_xlsx("SubvsStopvsCardvsVehicle.xlsx")

for(i in 1:nrow(SubvsStopvsCardvsVehicle))
{
  if(is.na(SubvsStopvsCardvsVehicle$`Suburb Name`[i]))
  {
    SubvsStopvsCardvsVehicle$`Suburb Name`[i] <- Value
  }
  else
  {
    Value <- SubvsStopvsCardvsVehicle$`Suburb Name`[i]
  }
}

write.csv(SubvsStopvsCardvsVehicle,"SubvsStopvsCardvsVehicle.csv")




StopIDAndcordinateswithPOSTcode <- read_xlsx("StopIDAndcordinateswithPOSTcode.xlsx")
for(i in 1:nrow(StopIDAndcordinateswithPOSTcode))
{
  if(is.na(StopIDAndcordinateswithPOSTcode$StopNameShort[i]))
  {
    StopIDAndcordinateswithPOSTcode$StopNameShort[i] <- Value
  }
  else
  {
    Value <- StopIDAndcordinateswithPOSTcode$StopNameShort[i]
  }
}

StopIDAndcordinateswithPOSTcode$StopNameShort
for(i in 1:nrow(SubvsStopvsCardvsVehicle))
{
  value <-SubvsStopvsCardvsVehicle$`Stop Name Short`[i]
  consider <- StopIDAndcordinateswithPOSTcode[StopIDAndcordinateswithPOSTcode$StopNameShort == value, ]
  SubvsStopvsCardvsVehicle$GPSLat[i] <- consider$GPSLat
  SubvsStopvsCardvsVehicle$GPSLong[i] <-consider$GPSLong
  }

SubvsStopvsCardvsVehicle$StopNameShort <- SubvsStopvsCardvsVehicle$`Stop Name Short`
object <- merge(StopIDAndcordinateswithPOSTcode,SubvsStopvsCardvsVehicle, by.x ="StopNameShort", by.y = "StopNameShort")
write.csv(unique(cordinateMatrix), "object1.csv")
object1_df<- unique(cordinateMatrix)

#function to remove duplicates
duplicated(object1_df$StopNameShort)

object2<- object1_df[!duplicated(object1_df$StopNameShort),]
write.csv( object2, "object2.csv")

# load example data (Fiji Earthquakes) + keep only 100 first lines
data(quakes)

modelData <- read.csv("ModelData.csv")



quakes =  head(quakes, 100)


cordinateMatrix <- read.csv("object.csv")



# Create a color palette with handmade bins.
mybins=seq(4, 6.5, by=0.5)
mypalette = colorBin( palette="YlOrBr", domain=quakes$mag, na.color="transparent", bins=mybins)

# Prepar the text for the tooltip:
mytext=paste("Depth: ", quakes$depth, "<br/>", "Stations: ", quakes$stations, "<br/>", "Magnitude: ", quakes$mag, sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(quakes) %>% 
  addTiles()  %>% 
  setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = ~mypalette(mag), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~mag, opacity=0.9, title = "Magnitude", position = "bottomright" )



VehicleIDvsCardcount <- read_xlsx("VehicleIDvsCardcount.xlsx")
plot(log(VehicleIDvsCardcount$`Count of Card ID`),log(VehicleIDvsCardcount$`Distinct count of Vehicle ID`))
summary(lm(log(VehicleIDvsCardcount$`Count of Card ID`)~log(VehicleIDvsCardcount$`Distinct count of Vehicle ID`)))
plot(lm(log(VehicleIDvsCardcount$`Count of Card ID`)~log(VehicleIDvsCardcount$`Distinct count of Vehicle ID`)))









themeSelector <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
                  c("default", shinythemes:::allThemes()),
                  selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
      .on('change', function(el) {
      var allThemes = $(this).find('option').map(function() {
      if ($(this).val() === 'default')
      return 'bootstrap';
      else
      return $(this).val();
      });
      // Find the current theme
      var curTheme = el.target.value;
      if (curTheme === 'default') {
      curTheme = 'bootstrap';
      curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
      } else {
      curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
      }
      // Find the <link> element with that has the bootstrap.css
      var $link = $('link').filter(function() {
      var theme = $(this).attr('href');
      theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
      return $.inArray(theme, allThemes) !== -1;
      });
      // Set it to the correct path
      $link.attr('href', curThemePath);
      });"
      )
    )
}

origin = c("40.431478+-80.0505401")
destination = c("43.0995629+-79.0437609")
results = gmapsdistance(origin, destination, mode = "driving",shape="long")
results$Distance

ui <- fluidPage(
  fluidRow(
    column(4, themeSelector())
  )
)

server <- function(input, output) {

}

shinyApp(ui, server)
