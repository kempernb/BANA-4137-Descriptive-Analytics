library(tidyverse)
library(lubridate)
library(corrplot)

# 1 Read in and setup ----
flights <- read.csv("CVG_Flights.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
flights$FLIGHT_DATE <- as.Date(as.character(flights$FLIGHT_DATE), format = "%m/%d/%y")
airports <-  read.csv("airports.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
airlines <-  read.csv("airlines.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

# Merge all the data
first_dataframe <- left_join(flights, 
                             airlines, 
                             by=c("AIRLINE"="IATA_CODE"))
head(first_dataframe)
all_flights <- left_join(first_dataframe,
                             airports,
                             by=c("ORIGIN_AIRPORT"="IATA_CODE"))

# Get all the missing values in the dataset
na.vals <- apply(is.na(all_flights), 2, which)
# Missing values are when flights ARENT cancelled

# Summary of DF
summary(all_flights)


# Anaylzing variables -----
# Select not calcled flights for some analysis
not_canc <- all_flights %>% 
  subset(is.na(CANCELLATION_REASON))

# Cor plot of numeric variables
m <- not_canc %>% 
  select(
    DEPARTURE_DELAY,
    TAXI_OUT,
    SCHEDULED_TIME,
    ELAPSED_TIME,
    AIR_TIME,
    DISTANCE,
    TAXI_IN,
    ARRIVAL_DELAY
  ) %>% 
  drop_na() %>% 
  cor()
# Not really anything interesting
corrplot(m)




# Which airlines are the best? ----


# Flight day trends ----
# When are most flights happening/ trends
flights_by_date <- flights %>%
  select(FLIGHT_DATE) %>%
  mutate(flights = 1) %>%
  group_by(FLIGHT_DATE) %>%
  summarise(flights = sum(flights)) %>% 
  drop_na()

flights_by_date$FLIGHT_DATE <- as.Date(flights_by_date$FLIGHT_DATE)

# Weird that one day always has less flights, look into which day of week?
ggplot(flights_by_date,aes(x = FLIGHT_DATE,y = flights)) + geom_line()

# Which day of week flights are least
flights_by_date$WEEKDAY <- weekdays(flights_by_date$FLIGHT_DATE)

# Box plot for weekday
ggplot(flights_by_date,aes(x = WEEKDAY,y = flights)) + geom_boxplot()
# Saturday is the least!

# Day of the week with most canceled flights?

library(maps)
library(ggmap)
library(leaflet)
library(htmlwidgets) # replacing leaflets
library(ggplot2)
library(maps)
library(geosphere)
library(usmap)
library(ggplot2)
library(data.table)
library(tidyverse)
library(geofacet)

maps::map("world", col="grey", fill=TRUE, border=0,bg="white", lwd=0.05, 
          xlim=c(-171.738281, -56.601563), ylim=c(12.039321, 71.856229)) #include the Alaska and Hawaii
# fsub <- flights[flights$AIRLINE == "MQ",]  #flights of  AA
fsub <- flights
for (j in 1:length(fsub$AIRLINE)) {        
  air1 <- airports[airports$IATA_CODE == fsub[j,]$ORIGIN_AIRPORT,]  #iata code for j flights in airport1
  air2 <- airports[airports$IATA_CODE == fsub[j,]$DESTINATION_AIRPORT,] #iata code for j flights in airport2
  inter <- gcIntermediate(c(air1[1,]$LONGITUDE, air1[1,]$LATITUDE), c(air2[1,]$LONGITUDE, air2[1,]$LATITUDE), n=100, addStartEnd=TRUE)  #calculate the great circles
  lines(inter, col="black", lwd=0.8)  #draw the line
}

flights_leave <- flights %>% 
  select(ORIGIN_AIRPORT) %>% 
  merge(airports, by.x = "ORIGIN_AIRPORT",by.y = "IATA_CODE") %>% 
  subset(STATE != "KY") %>% 
  group_by(STATE) %>% 
  count() %>% 
  mutate(state = STATE)
  

plot_usmap(data = flights_leave, values = "n", color = "red") + 
  scale_fill_continuous(name = "CVG origin flights", label = scales::comma) + 
  theme(legend.position = "right")

flights_arive <- flights %>% 
  select(DESTINATION_AIRPORT) %>% 
  merge(airports, by.x = "DESTINATION_AIRPORT",by.y = "IATA_CODE") %>% 
  subset(STATE != "KY") %>% 
  group_by(STATE) %>% 
  count() %>% 
  mutate(state = STATE)


plot_usmap(data = flights_arive, values = "n", color = "red") + 
  scale_fill_continuous(name = "CVG destination flights", label = scales::comma) + 
  theme(legend.position = "right")
