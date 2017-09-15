# You and a random opponent are playing a simplified game of War. Both you and your opponent have 13 cards in your deck: 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king and ace. You can arrange these cards in any order you want. You’ll play a one-round game, where you go through all 13 of your cards just once. Both you and your opponent draw a single card off the top of your deck and compare them. If your card outranks your opponent’s, you get a point. (No points are awarded for ties.) After all 13 cards have been shown, the player with the most points wins.
# 
# I’ll match you up against every player who submits, and the player who wins the most games overall will be this week’s Express winner.
# 
# However, to enter into this tournament, you must first beat me, the house. I have a deck that is in this order: 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, A. I, being the house and all, get an additional advantage in that I also win ties. Plus I can choose to play my decks forward or backward — the order above or A, K, Q, J, 10, 9, 8, 7, 6, 5, 4, 3, 2. Your deck must be able to beat both of my decks to enter the tournament.

# I wrote a function that finds random permutations that beat the house. Running this 100k times, I estimated that there were ~ 600 million permutations that will beat the house. So instead of trying to find one that wins more often, I just went with the first random one picked by my algo:

library(tidyverse)
set.seed(4321)

# jack, queen, king and ace = 10, 11, 12, 13
deck <- c(2:14)

# hand <- sample(deck, 13, replace = FALSE)


house1 <- c(2:14)
house2 <- rev(house1)

# My friend's hand
andrew1 <- c(8, 4, 13, 12, 11, 10, 9, 2, 7, 6, 5, 14, 3)



play_game <- function(x,y){
  # This function returns 1 if x wins
  
  ties <- sum(x == y)
  
  # You actually need .5 or 1 more than this #:
  wins_needed <- (13 - ties) / 2
  
  if_else((sum(x > y) > wins_needed), 1, 0)
}



play_house <- function(x){
  # This function returns 1 if x wins
  one <- if_else((sum(x > house1) > 6), 1, 0)
  two <- if_else((sum(x > house2) > 6), 1, 0)
  
  if_else((one + two == 2), 1, 0)
}


# Beat the house
# initial x
x <- sample(deck, 13, replace = FALSE)

# Now overwrite X
while(play_house(x) == 0){
  x <- sample(deck, 13, replace = FALSE)
}



# # Beat the house & Andrew
# # initial x
# x <- sample(deck, 13, replace = FALSE)
# # Now overwrite X
# while(play_house(x) == 0 & play_game(x, andrew1) == 0){
#   x <- sample(deck, 13, replace = FALSE)
# }




# How many permutations are likely to beat the house
play_house_sim <- function(z){
  # This function returns 1 if x wins
  print(z)
  
  x <- sample(deck, 13, replace = FALSE)
  
  one <- if_else((sum(x > house1) > 6), 1, 0)
  two <- if_else((sum(x > house2) > 6), 1, 0)
  
  if_else((one + two == 2), 1, 0)
  
}

test <- 1:100000 %>% 
  map(play_house_sim) %>% 
  unlist()

beat_it <- factorial(13) * (mean(test))
