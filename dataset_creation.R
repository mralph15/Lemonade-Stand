### Dataset Creation ###

#libraries
library(tidyverse)

#set seed
set.seed(1234)

#days of the month
day <- seq(1, 28, 1)
day_of_week <- rep(c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"), 4)

day <- data.frame(day)
day_of_week <- data.frame(day_of_week)

month <- cbind(day, day_of_week)

#number of cars per day
for(i in 1:nrow(month)){
  if(month$day_of_week[i] %in% c("Fr", "Sa", "Su")){
    month$cars[i] = rpois(1, 120)
  }
  else{
    month$cars[i] = rpois(1, 90)
  }
}

#row per car
month_long <- month %>% uncount(cars)
month_long <- left_join(month_long, month %>% select(day, cars), by = "day") %>%
  rename("total_cars" = "cars")

#car characteristics

