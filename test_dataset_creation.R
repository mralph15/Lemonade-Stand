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

#did car stop?
for(i in 1:nrow(month_long)){
  month_long$car_stopped[i] = as.numeric(rbernoulli(1, 0.5))
}

#color of car
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    month_long$car_color[i] = sample(c("black", "white", "red", "blue", "other"), size = 1,
                                     prob = c(0.3, 0.3, 0.1, 0.2, 0.1))
  }
  else{
    month_long$car_color[i] = NA
  }
}

#driver age
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    month_long$age[i] = sample(16:80, 1)
  }
  else{
    month_long$age[i] = NA
  }
}

#driver gender
male_prob = c()
for(i in 1:28){
  male_prob[i] = runif(1, 0, 1)
}

for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    month_long$gender[i] = sample(c("M", "F"), size = 1,
                                  prob = c(male_prob[month_long$day[i]], 1-male_prob[month_long$day[i]]))
  }
  else{
    month_long$gender[i] = NA
  }
}

#driver coupon
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    month_long$coupon[i] = as.integer(rbernoulli(1, 0.2))
  }
  else{
    month_long$coupon[i] = NA
  }
}

#driver preferences
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    month_long$prefs[i] = sample(c("hungry", "thirsty", "both"), size = 1,
                                 prob = c(0.2, 0.5, 0.3))
    month_long$lemon_bar[i] = 0
    month_long$lemonade[i] = 0
  }
  else{
    month_long$prefs[i] = NA
    month_long$lemon_bar[i] = NA
    month_long$lemonade[i] = NA
  }
}

#baseline driver purchases
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    if(month_long$prefs[i] == "hungry"){
      month_long$lemon_bar[i] = month_long$lemon_bar[i] + rpois(1, 1)
    }
    else if(month_long$prefs[i] == "thirsty"){
      month_long$lemonade[i] = month_long$lemonade[i] + rpois(1, 2)
    }
    else{
      month_long$lemon_bar[i] = month_long$lemon_bar[i] + rpois(1, 1)
      month_long$lemonade[i] = month_long$lemonade[i] + rpois(1, 2)
    }
  }
}

#specific driver preferences
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    if(month_long$gender[i] == "M"){
      month_long$lemon_bar[i] = month_long$lemon_bar[i] + as.integer(rbernoulli(1, 0.2))
    }
    if(month_long$day_of_week[i] %in% c("Mo", "Tu", "We", "Th")){
      month_long$lemonade[i] == month_long$lemonade[i] + rpois(1, 0.5)
    }
    month_long$lemonade[i] =  month_long$lemonade[i] + rpois(1, month_long$age[i]/100)
    month_long$lemon_bar[i] = month_long$lemon_bar[i] + rpois(1, month_long$age[i]/100)
  }
}

#driver spending
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1){
    month_long$spending[i] = month_long$lemonade[i] + month_long$lemon_bar[i] * 2
  }
  else{
    month_long$spending[i] = NA
  }
}

#driver coupon application
for(i in 1:nrow(month_long)){
  if(month_long$car_stopped[i] == 1 & month_long$coupon[i] == 1 & month_long$spending[i] > 10){
    month_long$spending[i] = month_long$spending[i] * 0.85
  }
}
