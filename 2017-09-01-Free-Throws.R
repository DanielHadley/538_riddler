library(tidyverse)
set.seed(123)

# https://fivethirtyeight.com/features/is-your-friend-full-of-it/

# From Shaun Raviv, a tall tale on the basketball court:

# You’re hanging out with some friends, shooting the breeze and talking sports. One of them brags to the group that he once made 17 free throws in a row after years of not having touched a basketball. You think the claim sounds unlikely, but plausible. Another friend scoffs, thinking it completely impossible. Let’s give your bragging friend the benefit of the doubt and say he’s a 70-percent free-throw shooter.

# So, who’s right? What is the number of free throws that a 70-percent shooter would be expected to take before having a streak of 17 makes in a row? And what if his accuracy was a bit worse?

# There's a simple equation that can be derived from the probability of seeing the 17 pattern vs. the probability of the free throw sequence starting over. In code, that looks like ex <- (p^(-1*n) - 1) / (1-p). But I wanted to simulate so I could see how the distribution of possibilities looks. I did 1m simulations and got sequences that lasted up to 17,000+ shots. Interestingly, the most common occurrence (the mode) was 17. So while it is incredibly unlikely my friend is telling the truth, I give it outside odds of about 18%, which is the percent of simulations where the 17 sequence appeared within 300 shots, which seems reasonable to me.    

shoot_free_throws <- function(x, p){
  count <- 0
  consecutive_count <- 0
  
  while(consecutive_count < 17) {
    shot <- sample(c(1,0), 1, ,c(p, 1 - p))
    ifelse(shot == 1, (consecutive_count = consecutive_count + 1), (consecutive_count = 0))
    count <- count + 1
  }
  
  print(x)
  return(count)
}


sims <- 1e6

answer <- 1:sims %>%
  map(function(x) shoot_free_throws(x, .7)) %>% 
  unlist() 


mean(answer)
median(answer)
max(answer)
hist(answer)


myodds <- length(answer[answer < 300]) / sims

#### 
# Let's confirm with math
# https://math.stackexchange.com/questions/27989/time-until-a-consecutive-sequence-of-ones-in-a-random-bit-sequence/27991#27991
p = .7
n = 17

ex <- (p^(-1*n) - 1) / (1-p)


# And what if accuracy is a bit worse
p = .5
n = 17

ex_low <- (p^(-1*n) - 1) / (1-p) 


# The record for free throws / minute is apparently 52:
# http://www.guinnessworldrecords.com/world-records/most-basketball-free-throws-in-one-minute
# He took 75 shots in that minute
hours <- ex / 75 

# This would take 19 hours. 