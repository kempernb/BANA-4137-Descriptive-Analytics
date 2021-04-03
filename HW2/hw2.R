library(tidyverse)

# 1 Read in
flights <- read.csv("CVG_Flights.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
airports <-  read.csv("airports.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
airlines <-  read.csv("airlines.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

# 2 rows
nrow(flights)
nrow(airports)
nrow(airlines)

# 3 columns
ncol(flights)
ncol(airports)
ncol(airlines)

# 4 merged
first_dataframe <- left_join(flights, 
                             airlines, 
                             by=c("AIRLINE"="IATA_CODE"))
head(first_dataframe)
total_dataframe <- left_join(first_dataframe,
                             airports,
                             by=c("ORIGIN_AIRPORT"="IATA_CODE"))
head(total_dataframe)

# 5 first and last 6
head(total_dataframe,6)
tail(total_dataframe,6)

# 6 Missing values
na.vals <- apply(is.na(total_dataframe), 2, which)

# 7 Porportion of cancelled flights, 0 is not and 1 is
(total_dataframe %>%
    subset(CANCELLED == 1) %>%
    nrow()) / nrow(total_dataframe)

# 8 Missing depature times
na.vals$DEPARTURE_TIME

# occurs when flight is cancled

# 9 new variables
total_dataframe$TOTAL_TIME <-
  total_dataframe$SCHEDULED_TIME -  total_dataframe$ELAPSED_TIME
head(total_dataframe, 6)
tail(total_dataframe, 6)

# 10 only CVG and Delta
cvgDelta <- total_dataframe %>%
  subset(AIRLINE.y == "Delta Air Lines Inc." &
           ORIGIN_AIRPORT == "CVG")


head(cvgDelta, 6)
tail(cvgDelta, 6)

# 11 CVG Dela long delay
cvgDeltaLongDelay <- total_dataframe %>%
  subset(AIRLINE.y == "Delta Air Lines Inc." &
           ORIGIN_AIRPORT == "CVG" &
           DEPARTURE_DELAY > 30)

head(cvgDeltaLongDelay, 6)
tail(cvgDeltaLongDelay, 6)

# 12 group by summerize airline
total_dataframe %>% 
  group_by(AIRLINE) %>% 
  summarise(mean = mean(DEPARTURE_DELAY,na.rm = T))

# 13 group by summerize airline
total_dataframe %>% 
  group_by(ORIGIN_AIRPORT) %>% 
  summarise(mean = mean(DEPARTURE_DELAY,na.rm = T))

