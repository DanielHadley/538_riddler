library(tidyverse)
set.seed(123)



die <- c(1:6)


roll_dice_and_pay <- function(money, person) {
  
  if (money[person] == 0) {
    return(money)
  }
  
  die1 <- sample(die, 1)
  die2 <- sample(die, 1)
  die3 <- sample(die, 1)
  
  dice <- c(die1, die2, die3)
  
  left <- if_else(person == 1, 6, person - 1)
  right <- if_else(person == 6, 1, person + 1)
  
  dollars <- if_else(money[person] > 3, 3, money[person])
  
  for (i in 1:dollars) {
    if (dice[i] %in% c(1,2)) {
      money[person] = money[person] - 1
      money[left] = money[left] + 1 
    }
    
    if (dice[i] %in% c(3,4)) {
      money[person] = money[person] - 1
      money[right] = money[right] + 1 
    }
    
    if (dice[i] %in% c(5,6)) {
      money[person] = money[person] - 1
    }
    
  }
  
  return(money)
  
}



play_game <- function(x) {
  
  money <- c(rep(3, 6))
  
  n = 0
  
  while (length(money[money == 0]) < 5) {
    
    for (m in 1:length(money)) {
      money <- roll_dice_and_pay(money, m)
      n = n + 1
      
      if (length(money[money == 0]) == 5) {
        break
        
      }
    }
    
    
  }
  
  return(n)
  
}


times <- 1:10000 %>% 
  map(play_game) %>% 
  unlist() %>% 
  mean()






