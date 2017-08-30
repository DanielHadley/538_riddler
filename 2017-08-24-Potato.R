library(tidyverse)
set.seed(123)

# https://fivethirtyeight.com/features/is-this-bathroom-occupied/

# A class of 30 children is playing a game where they all stand in a circle along with their teacher. The teacher is holding two things: a coin and a potato. The game progresses like this: The teacher tosses the coin. Whoever holds the potato passes it to the left if the coin comes up heads and to the right if the coin comes up tails. The game ends when every child except one has held the potato, and the one who hasn’t is declared the winner.

# How do a child’s chances of winning change depending on where they are in the circle? In other words, what is each child’s win probability?



# We will consider the teacher to be 0

flip_coin_move_potato <- function(kid){
  flip <- sample(c(0,1), 1)
  ifelse(flip == 1 & kid == 30, 0,
          ifelse(flip == 0 & kid == 0, 30,
                  ifelse(flip == 1, (kid = kid + 1),
                          ifelse(flip == 0, (kid = kid - 1),
                                  576))))
}


play_game <- function(x){
  kids <- c()
  kid <- 0
  
  while(length(unique(kids)) < 29) {
    kid <- flip_coin_move_potato(kid)
    if(kid > 0) {kids <- append(kids, kid)}
  }
  
  kids <- unique(kids)
  all_kids <- c(1:30)
  
  print(x)
  
  setdiff(all_kids, kids)
  
}

sims <- 100000

answer2 <- 1:sims %>%
  map(play_game) %>% 
  unlist() %>%
  data_frame() 

names(answer2) <- "place"


answer2 <- answer2 %>% 
  group_by(place) %>% 
  tally() %>% 
  mutate(percent = n / sims)
















