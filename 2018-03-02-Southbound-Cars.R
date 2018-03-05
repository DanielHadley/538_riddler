library(tidyverse)

# https://fivethirtyeight.com/features/how-long-will-you-shuffle-this-damn-deck-of-cards/

# Andrea and Barry both exercise every day on their lunch hour on a path that runs alongside a parkway. Andrea walks north on the path at a steady 3 mph, while Barry bikes south on the path at a consistent 15 mph, and each travels in their original direction the whole time — they never turn around and go back the other way. The speed limit on the parkway is the same in both directions and vehicle traffic flows smoothly in both directions exactly at the speed limit.2
# In order to pass the time while they exercise, both Andrea and Barry count the number of cars that go past them in both directions and keep daily statistics. After several months of keeping such stats, they compare notes.
# 
# Andrea says: “The ratio of the number of cars that passed me driving south on the parkway to the number of cars that passed me driving north was 35-to-19.”
# 
# Barry retorts: “I think you’re way off. The ratio for me was 1-to-1 — the number of cars that passed me going south was the same as the number that passed me going north.”
# 
# Assuming Andrea and Barry are both very good at stats, what is the speed limit on the parkway?

#### First the parameters ####
# North 0 ----------------- # South 15

# There are more southbound cars
# This is essentially how much closer SB cars are to each other
southbound_car_multiplier <- 21 / 35

# The great unknown
speed_limit <- 60

# 0 is the northernmost and 15 is southern edge
miles <- c(1:15)
# Tracking things for every minute
hour <- 60 
# Andrea starts at the southern edge and moves north
andrea <- 15
# Barry does the opposite
barry <- 0




#### Calculate things at the starting gate ####

## The pace cars
# These start at the outer edge to ensure that the ratios are not thrown off
# by the gap between starting and seeing the first car
southbound_car1 <- 15
northbound_car1 <- 0

# Run to see how many are already past Barry and Andrea
southbound_cars <- c(1:1500) %>% map(function(x) southbound_car1 - x * southbound_car_multiplier / 10)
northbound_cars <- c(1:1500) %>% map(function(x) northbound_car1 + x * .1)

# already past Barry
sb_cars_past_barry <- sum(southbound_cars > barry)
nb_cars_past_barry <- sum(northbound_cars < barry)

# already past Andrea
sb_cars_past_andrea <- sum(southbound_cars > andrea)
nb_cars_past_andrea <- sum(northbound_cars < andrea)




#### The simulation: move Andrea, Barry, and the pace cars ####
for (minute in 1:hour) {
  
  # Andrea is northbound
  andrea <- andrea - 3 / hour 
  # Comment above and uncomment below to test what happens when they stand still
  # andrea <- andrea - 0 / hour
  
  # Barry is southbound
  barry <- barry + 15 / hour
  # Comment above and uncomment below to test what happens when they stand still
  # barry <- barry + 0 / hour
  
  # Pacecars move
  southbound_car1 <- southbound_car1 + speed_limit / hour
  northbound_car1 <- northbound_car1 - speed_limit / hour
  
}




#### Line up all cars behind the pace cars and see how many passed Andrea and Barry ####
# Recalculate
southbound_cars <- c(1:1500) %>% map(function(x) southbound_car1 - x * southbound_car_multiplier / 10)
northbound_cars <- c(1:1500) %>% map(function(x) northbound_car1 + x * .1)


# Now count the cars that passed both of them during the simulation
sb_cars_that_passed_barry <- sum(southbound_cars > barry) - sb_cars_past_barry
nb_cars_that_passed_barry <- sum(northbound_cars < barry) - nb_cars_past_barry

sb_cars_that_passed_andrea <- sum(southbound_cars > andrea) - sb_cars_past_andrea
nb_cars_that_passed_andrea <- sum(northbound_cars < andrea) - nb_cars_past_andrea


# Finally the ratio
barry_ratio <- sb_cars_that_passed_barry / nb_cars_that_passed_barry
andrea_ratio <- sb_cars_that_passed_andrea / nb_cars_that_passed_andrea

# If we get the speed limit and ratios right, these should be ~ 1:1 and 35:19
barry_ratio
andrea_ratio
