library(tidyverse)
options(scipen = 999)
# https://fivethirtyeight.com/features/work-a-shift-in-the-riddler-gift-shop/
# You take half of a vitamin every morning. The vitamins are sold in a bottle of 100 (whole) tablets, so at first you have to cut the tablets in half. Every day you randomly pull one thing from the bottle — if it’s a whole tablet, you cut it in half and put the leftover half back in the bottle. If it’s a half-tablet, you take the vitamin. You just bought a fresh bottle. How many days, on average, will it be before you pull a half-tablet out of the bottle?

#Extra credit: What if the halves are less likely to come up than the full tablets? They are smaller, after all.


# This problem was fun because its answer is similar to a geometric distribution - that is, the number X of Bernoulli trials needed to get one success - but in this case, the probabilities update dynamically based on how many half-pills are being placed in the bottle. I first thought of ways to either simulate the problem or find an equation for sampling with replacement; but then it occurred to me that I could just create a vector with the probability of *not* picking a half-pill on any given day and then take the cumulative product of those. I was initially tempted to look for the day where the probability of not picking a half-pill goes below .5, but my friend Andrew pointed out that this is the median and not the mean. To get the mean, you need to sum the product of the days, probability of picking a half-pill, and the probability of *not* picking a half-pill. 



# n is a vector of the days
n <- c(1:100)

# p is the conditional probability of picking a half pill, conditioned on you already making it to day n without having selected a half pill (essentially, the portion of half-pills in the jar at the beginning of a day)
p <- n %>% 
  map(function(x) (x-1) / 100) %>% 
  unlist()

#  k is the probability you will make it to day n, without selecting a half pill (not including day n).
k <- p %>%
  map(function(x) 1 - x) %>%
  cumprod() %>% 
  append(1, after = 1) %>% 
  head(-1)

mean <- sum(n * p * k)

# The approximate mediad
median <- length(k[k > .50])


ggplot() + geom_line(aes(x=n,y= k), stat='identity') + xlab('days') + ylab('prob of making it to this day')



## Extra credit!
library(tidyverse)

# n is a vector of the days
n <- c(1:100)

# p is the conditional probability of picking a half pill, conditioned on you already making it to day n without having selected a half pill (essentially, the portion of half-pills in the jar at the beginning of a day)
p <- n %>% 
  map(function(x) (x-1) * .5 / 100) %>% 
  unlist()

#  k is the probability you will make it to day n, without selecting a half pill (not including day n).
k <- p %>%
  map(function(x) 1 - x) %>%
  cumprod() %>% 
  append(1, after = 1) %>% 
  head(-1)

mean <- sum(n * p * k)


ggplot() + geom_line(aes(x=n,y= k), stat='identity') + xlab('days') + ylab('prob of making it to this day')




#### Test

# Now to show it works with the traditional dice roll - this is the response you would expect from geometric
n <- c(1:250)

# p is the conditional probability of rolling a 6, conditioned on you already making it to roll n without having rolled a 6
p <- n %>% 
  map(function(x) 1/6) %>% 
  unlist()

#  k is the probability you will make it to roll n, without rolling a 6 (not including roll n).
k <- p %>%
  map(function(x) 1 - x) %>%
  cumprod() %>% 
  append(1, after = 0) %>% 
  head(-1)

mean <- sum(n * p * k)


ggplot() + geom_line(aes(x=n,y= k), stat='identity') + xlab('days') + ylab('prob of making it to this roll')


