library(tidyverse)

# https://fivethirtyeight.com/features/can-you-fool-the-bank-with-your-counterfeit-bills/

# You are an expert counterfeiter, and you specialize in forging one of the most ubiquitous notes in global circulation, the U.S. $100 bill. You’ve been able to fool the authorities with your carefully crafted C-notes for some time, but you’ve learned that new security features will make it impossible for you to continue to avoid detection. As a result, you decide to deposit as many fake notes as you dare before the security features are implemented and then retire from your life of crime.
# 
# You know from experience that the bank can only spot your fakes 25 percent of the time, and trying to deposit only counterfeit bills would be a ticket to jail. However, if you combine fake and real notes, there’s a chance the bank will accept your money. You have $2,500 in bona fide hundreds, plus a virtually unlimited supply of counterfeits. The bank scrutinizes cash deposits carefully: They randomly select 5 percent of the notes they receive, rounded up to the nearest whole number, for close examination. If they identify any note in a deposit as fake, they will confiscate the entire sum, leaving you only enough time to flee.
# 
# How many fake notes should you add to the $2,500 in order to maximize the expected value of your bank account? How much free money are you likely to make from your strategy?



mix_money <- function(n){
  
  real <- rep(0, 25)
  fake <- rep(1, n)
  
  money_bag <- sample(c(real, fake), 25 + n)
}


inspect_money_bag <- function(money_bag){
  
  selection <- sample(money_bag, size = round(.05 * length(money_bag)))
  
  counterfeits <- sum(selection)
  
  prob_spotting <- 1 - (.25 ^ counterfeits)
  
  spotted_counterfeits <- sample(c(1,0), 1, prob = c(prob_spotting, 1 - prob_spotting))
  
  expected_value <- case_when(
    spotted_counterfeits > 0 ~ 0, 
    TRUE ~ (length(money_bag) * 100)
    )
  
}


try_scenario <- function(n){
  
  n_sims <- 100000
  
  total <- sum(replicate(n_sims, inspect_money_bag(mix_money(n)))) / n_sims
    
  
}

0:10 %>% 
  map(try_scenario)


# 5
