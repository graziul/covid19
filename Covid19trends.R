library(leaflet)
library(lubridate)
library(RColorBrewer)
library(raster)

#Get a map with county boundaries
USA <- getData("GADM", country = "usa", level = 2)

#Get data from NYT
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
df_nyt <- read.csv(url, stringsAsFactors = FALSE)
df_nyt$date <- ymd(df_nyt$date)
df_nyt <- df_nyt[!is.na(df_nyt$fips) & df_nyt$county!="Unknown",]

#Get data from last 7 days
last_7 <- tail(unique(sort(df_nyt$date)),7)
df_nyt_last7 <- subset(df_nyt, date %in% last_7)

#Function to get average doubling rate
get_avg_doub <- function(df_temp, days) {
  growthrate <- c(log(df_temp$cases[-1]/df_temp$cases[-days]))
  if (sum(growthrate==0)/length(growthrate)<0.51) {
  doub <- c(log(2)/growthrate)
  avg_doub <- mean(doub[doub!=Inf])
  return(avg_doub)
  } else {
  return(NA)
  }
}

#Function to get row of data
get_row <- function(df_temp) {
  days <- length(df_temp$cases)
  if (days>1) {
    current_cases <- df_temp$cases[length(df_temp$cases)]
    state <- unique(df_temp$state)[1]
    county <- unique(df_temp$county)[1]
    avg_doub <- get_avg_doub(df_temp, days)
    row <- c(state, county, avg_doub, current_cases)
    return(row)
  }
}

#Get all the counties in the NYT data
all_fips <- unique(df_nyt_last7$fips) 

#Get relevant county-level information
Data <- NULL
for (county in all_fips) {
  df_temp <- df_nyt_last7[df_nyt_last7$fips==county,] 
  row <- get_row(df_temp)
  Data <- rbind(Data, row)
}

#Make it into a dataframe
df <- as.data.frame(Data, stringsAsFactors=F)
names(df) <- c("state","county","avg_doub","cases")
df$avg_doub <- as.numeric(df$avg_doub)
df$cases <- as.numeric(df$cases)
rownames(df) <- NULL
df <- df[!is.na(df$avg_doub),]
#Calculate breaks to create categories for colorbrewer


#Merge with county boundary map
temp <- merge(USA, df,
              by.x = c("NAME_1","NAME_2"), by.y= c("state","county"),
              all.x=T)
temp$avg_doub <- as.numeric(temp$avg_doub)
temp$cases <- as.numeric(temp$cases)


mypal <- colorNumeric(palette = brewer.pal(5,"RdYlBu")[5:1], domain = temp$avg_doub, na.color = "lightgrey")

leaflet(options = 
          leafletOptions(minZoom = 3.75, dragging = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lat = 39.3, lng = -98, zoom = 3.75) %>%
  addPolygons(data=temp, stroke=F,
              fillOpacity = 1,
              fillColor=mypal(temp$avg_doub))

