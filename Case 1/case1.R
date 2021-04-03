library(tidyverse)
library(corrplot)
library(reshape2)
library(sf)
library(mapview)
library(ggmap)
library(maps)


# my custom functions
HistNumeric <- function(x){
  d <- melt(x)
  ggplot(d,aes(x = value)) +
    facet_wrap(~variable,scales = "free_x") +
    geom_histogram()
}

# Read and change
college <- read.csv("college.csv")
college$loan_default_rate <- as.numeric(college$loan_default_rate)

# 1 nrow and ncol
nrow(college)
ncol(college)

# Missing values
na.vals <- apply(is.na(college), 2, which)
# some loan default rate missing

# Summary of data
summary(college)

# histogram of numeric
HistNumeric(college)

# correlation between variables
m <- college %>% 
  select_if(is.numeric) %>% 
  select(-c(lon,lat)) %>% 
  drop_na() %>% 
  cor()

# Interesting Plot, Correlation between variables
cor.plot(m)
corrplot(m)

# Plot loan default
ggplot(college,aes(x = sat_avg,y = loan_default_rate)) + geom_point()

# Are more people defaulting on loans at private
ggplot(college,aes(x = control,y = loan_default_rate)) + geom_boxplot()
# No apprent difference

# Ceratin parts on the coutnry
ggplot(college,aes(x = region,y = loan_default_rate,color =control )) + geom_boxplot()
ggplot(college, aes(x = region, y = loan_default_rate, color = control)) + geom_boxplot()
median# Slightly more in south

# median debt vs default rate, slightly
ggplot(college,
       aes(x = median_debt, y = loan_default_rate, color = highest_degree)) + geom_point()

# Types of colleges
ggplot(college,aes(x = region,y = loan_default_rate,color = highest_degree)) + geom_boxplot()
# Asociate a little higher

ggplot(college, aes(x = admission_rate, y = faculty_salary_avg, color = control)) + geom_point()

# Which states have the hardeset colleges to get in? min degree a Graduate
college.grad <-college %>% 
  subset(highest_degree == "Graduate")
  

ggplot(college.grad,aes(x = state,y = admission_rate)) + geom_boxplot()


locs <- college %>% 
  select(lat,lon)

locations_sf <- st_as_sf(locs, coords = c("lon", "lat"), crs = 4326)
mapview(locations_sf)

maps::map('state')
points(x=college$lon, y=college$lat, col="tomato", cex=1, pch=20)  #cex: size of point，pch: shape of the point

collge.mod <- college %>% 
  select(
    -c(
      id,
      name,
      lon,
      lat,
      city,
      state
    )
  )

# Linear regression
mod <- lm(loan_default_rate ~. , data = collge.mod)
summary(mod)

library(rpart)
library(rpart.plot)


mod <- rpart(loan_default_rate ~. , data = collge.mod)

prp(mod, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(mod$frame$var == "<leaf>", 'gray', 'white'))

