# Twenty ghostbusters are on their annual camping retreat. Two of them, Abe and Betty, have discovered that another pair, Candace and Dan, are in fact ghosts posing as ghostbusters. Abe and Betty hatch a plan: When all 20 campers are sitting in a circle around the campfire, Abe will fire his proton pack at Candace, and Betty will simultaneously fire her proton pack at Dan, annihilating the ghosts. However, if two proton streams cross, it means the end of all life on Earth.
# 
# If the ghostbusters are arranged randomly around the fire, what are the chances that Abe and Betty will cross streams?

library(gtools)
library(tidyverse)

n = 10

sitting <- 1:n

circle_permutations <- as.tibble(permutations(n, n, sitting)) %>% 
  filter(V1 == 1)


# 1 shoots 3, 2 shoots 4

see_if_streams_cross <- function(x){
  vec <- as.numeric(circle_permutations[x,])
  three <- match(3, vec)
  two <- match(2, vec)
  four <- match(4, vec)
  
  if_else(three == n, 0,
         if_else(three == 2, 0,
                if_else((two > three & four > three), 0,
                       if_else((two < three & four < three), 0, 1))))
}


they_cross <- 1:nrow(circle_permutations) %>% 
  map(see_if_streams_cross) %>% 
  unlist() %>% 
  sum()

answer <- they_cross / nrow(circle_permutations)
