library(tidyverse)
library(lubridate)
library(highcharter)
library(RColorBrewer)
library(ggTimeSeries)
library(RColorBrewer)
# library(ggraph)
# library(igraph)
# library(reshape2)
# library(ggforce)
# library(maps)
# library(ggmap)
# library(leaflet)
# library(htmlwidgets) # replacing leaflets
# library(RColorBrewer)
# library(tmap)    # for static and interactive maps
# library(leaflet) # for interactive maps
# library(sf)
# library(raster)
# library(dplyr)
# library(spData)
# library(spDataLarge)
# 
# library(gapminder)
# library(gganimate)
library(wordcloud)
library(wordcloud2)

library(tm)

# Police data, only from years 2018-20 ----
police <-
  read_csv(
    "police.csv"
  )


# Format time Columns as date
police$CREATE_DATE <- as.Date(substring(police$CREATE_TIME_INCIDENT,1,10),format="%m/%d/%Y")


# Format time Columns as date
police$CREATE_TIME_INCIDENT <- as.POSIXct(police$CREATE_TIME_INCIDENT, format="%m/%d/%Y %H:%M:%S")
police$ARRIVAL_TIME_PRIMARY_UNIT <- as.POSIXct(police$ARRIVAL_TIME_PRIMARY_UNIT, format="%m/%d/%Y %H:%M:%S")
police$CLOSED_TIME_INCIDENT <- as.POSIXct(police$ARRIVAL_TIME_PRIMARY_UNIT, format="%m/%d/%Y %H:%M:%S")
police$DISPATCH_TIME_PRIMARY_UNIT <- as.POSIXct(police$DISPATCH_TIME_PRIMARY_UNIT, format="%m/%d/%Y %H:%M:%S")



# Get individual dates ----
# police2020 <- police %>%
#   subset(year(CREATE_TIME_INCIDENT) == 2020)
#
# police2019 <- police %>%
#   subset(year(CREATE_TIME_INCIDENT) == 2019)
#
# police2018 <-  police %>%
#   subset(year(CREATE_TIME_INCIDENT) == 2018)


# Create a year variable ----
police$year <- year(police$CREATE_TIME_INCIDENT)
# Create a date variable ----
police$date <- as.Date(police$CREATE_TIME_INCIDENT)
# drop NA values
police <- police[!is.na(police$year), ]

# What does each priority color mean? -----

priority_colors <- police %>% 
  dplyr::select(PRIORITY_COLOR) %>% 
  drop_na() %>%
  unique()

# # word cloud of priority levels
# set.seed(1234) # for reproducibility
# for (i in priority_colors$PRIORITY_COLOR){
#   police_color <- police %>% 
#     subset(PRIORITY_COLOR == i)
#   
#   text <- police_color$INCIDENT_TYPE_DESC
#   docs <- Corpus(VectorSource(text))
#   docs <- docs %>%
#     tm_map(removeNumbers) %>%
#     tm_map(removePunctuation) %>%
#     tm_map(stripWhitespace)
#   docs <- tm_map(docs, content_transformer(tolower))
#   docs <- tm_map(docs, removeWords, stopwords("english"))
#   dtm <- TermDocumentMatrix(docs)
#   matrix <- as.matrix(dtm)
#   words <- sort(rowSums(matrix), decreasing = TRUE)
#   df <- data.frame(word = names(words), freq = words)
#   
#   # print(wordcloud2(data=df, size=1.6, color='random-dark'))
#   wordcloud(words = df$word, freq = df$freq, min.freq = 1,
#             max.words=200, random.order=FALSE, rot.per=0.35, 
#             colors=brewer.pal(8, "Dark2"))
# }

# set.seed(1234) # for reproducibility
# data_list <- list()
# for (i in priority_colors$PRIORITY_COLOR){
  police_color <- police %>%
    subset(PRIORITY_COLOR == i)
#   
#   text <- police_color$INCIDENT_TYPE_DESC
#   docs <- Corpus(VectorSource(text))
#   docs <- docs %>%
#     tm_map(removeNumbers) %>%
#     tm_map(removePunctuation) %>%
#     tm_map(stripWhitespace)
#   docs <- tm_map(docs, content_transformer(tolower))
#   docs <- tm_map(docs, removeWords, stopwords("english"))
#   dtm <- TermDocumentMatrix(docs)
#   matrix <- as.matrix(dtm)
#   words <- sort(rowSums(matrix), decreasing = TRUE)
#   df <- data.frame(word = names(words), freq = words)
#   dfL <- list(df)
#   
#   data_list <- append(data_list,dfL)
# }

 freqWordsPolice <- function(police_color){
    text <- police_color$INCIDENT_TYPE_DESC
    docs <- Corpus(VectorSource(text))
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    dtm <- TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix), decreasing = TRUE)
    df <- data.frame(word = names(words), freq = words)
    
    return(df)
}

 freqWords <- freqWordsPolice(police)
 
wordcloud2(data=data_list[[1]], size=1.6, color='random-dark')

# Priority color is na, contains no description

# How has 911 calls differed in 2020? and overtime? ----

# Crime in neighorhoods, nothing much
year_neighborhood <- police %>%
  subset(CPD_NEIGHBORHOOD != "N/A") %>% 
  # subset(PRIORITY_COLOR == "BLUE") %>% 
  group_by(year, CPD_NEIGHBORHOOD) %>%
  count()

# Time series plot of the general trend, red it rising and 
# Others decreasing

daily_calls <- police %>% 
  group_by(date,PRIORITY_COLOR) %>% 
  count() %>% 
  drop_na()

ggplot(daily_calls, aes(x=date, y=n,color = PRIORITY_COLOR)) +
  geom_line() + 
  xlab("Date")

wide <- daily_calls %>%
  pivot_wider(names_from = "PRIORITY_COLOR", values_from = "n")

ggplot(wide, aes(x=RED, y=BLUE)) +
  geom_point() + geom_smooth(method = "lm")

# Not much
# p<- ggplot(year_neighborhood,aes(x=year,y=CPD_NEIGHBORHOOD,fill=n))+
#   geom_tile() 
# 
# p + scale_fill_distiller(palette = "RdPu")

# Good plot
q <- ggplot(year_neighborhood,aes(x=year,y=n,group=CPD_NEIGHBORHOOD)) +  
  geom_line(aes(color=CPD_NEIGHBORHOOD,alpha=1),size=1) +
  geom_point(aes(color=CPD_NEIGHBORHOOD,alpha=1),size=2) 
  
q


day_police<- police %>%
  group_by(date) %>%
  count() 



p1 <- ggplot_calendar_heatmap(
  day_police,
  'date',
  'n'
) + scale_fill_continuous(low = 'green', high = 'red') + 
  facet_wrap(~Year, ncol = 1)

p1

neighborhood_call <- police %>% 
  group_by(SNA_NEIGHBORHOOD,INCIDENT_TYPE_ID,PRIORITY_COLOR) %>% 
  count()


day_severity_data <- police %>%
  # subset(SNA_NEIGHBORHOOD != "N/A") %>% 
  dplyr::select(date,PRIORITY_COLOR,LATITUDE_X,LONGITUDE_X) %>% 
  drop_na()

# Animation of City

# ggplot(day_severity_data, aes(LATITUDE_X, LONGITUDE_X,  colour = PRIORITY_COLOR))  +
#   geom_point() +
#   transition_time(date) +
#   ease_aes('linear')

# anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")

