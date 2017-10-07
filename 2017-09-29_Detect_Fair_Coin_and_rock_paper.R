# Riddler Classic
# Speaking of ways to randomly settle scores, how about some coin flipping? From James Nugent, a numismatic detective problem:
#   
#   On the table in front of you are two coins. They look and feel identical, but you know one of them has been doctored. The fair coin comes up heads half the time while the doctored coin comes up heads 60 percent of the time. How many flips — you must flip both coins at once, one with each hand — would you need to give yourself a 95 percent chance of correctly identifying the doctored coin?
# 
# Extra credit: What if, instead of 60 percent, the doctored coin came up heads some P percent of the time? How does that affect the speed with which you can correctly detect it?

# 
x <- c(0:143)
regular <- dbinom(x, size=143, prob=.5)
biased <- dbinom(x,size=143,prob=.6)




# I don't thin this answers the question of being 95% confident. It's more the average time of getting to that using Bayesian methods. 
#### Method two: Bayesian updating simulation ####
# H = coin1 = fair 

update <- function(prior, coin_randomizer) {
  # This function updates our prior probabilities after looking at both coins
  
  # Flip
  # The biased coin is randomly selected. 
  # They both return .4 or .6 because we plug these figures into Bayes' theorem below
  # But actual heads/tails probabilities are either 50/50 or 60/40
  flip_coin1 <- sample(c(.4,.6), 1, prob = c(coin_randomizer[1], 1 - coin_randomizer[1]))
  flip_coin2 <- sample(c(.4,.6), 1, prob = c(coin_randomizer[2], 1 - coin_randomizer[2]))
  
  # Hypothesis given the data
  # Bayes Theorum = prob(A|X) =
  # Numerator
  # P(X|A)*P(A) /
  # Denominator
  # P(X) = P(X|A)*P(A) + P(X|not A) * P(not A)
  
  # Prob coin1 is fair given flip1
  posterior_after_flip1 <- (.50 * prior) / ((.50 * prior) + flip_coin1 * (1 - prior))
  
  # Prob coin1 is fair given flip2 and the priors informed by flip1
  posterior_after_flip2 <- (flip_coin2 * posterior_after_flip1) / 
    ((flip_coin2 * posterior_after_flip1) + .50 * (1 - posterior_after_flip1))
  
  return(posterior_after_flip2)
}


simulate <- function(x) {
  
  # We randomly assign the coins 1x per simulation
  coin_randomizer <- sample(c(.5, .4), 2)
  
  # Keep track for checking later
  is_fair_coin1 <- if_else(coin_randomizer[1] == .5, 1, .1)
  
  # Start here
  n_flips <- 0
  prior <- .5
  
  while(prior >= .05 & prior <= .95){
    prior <- update(prior, coin_randomizer)
    n_flips <- n_flips + 1
  }
  
  # Just for updating progress
  print(x)
  # Return how long it took to be 95% sure
  return(n_flips)
  
}

n_sims <- 2e6

sim_results <- 1:n_sims %>% 
  map(function(x) simulate(x)) %>% 
  unlist()

summary(sim_results)
hist(sim_results)



# Let's make sure we get the right # of true positives
test_the_sim <- function(x) {
  
  # We randomly assign the coins 1x per simulation
  coin_randomizer <- sample(c(.5, .4), 2)
  
  # Keep track for checking later
  is_fair_coin1 <- if_else(coin_randomizer[1] == .5, 1, .1)
  
  # Start here
  n_flips <- 0
  prior <- .5
  
  while(prior >= .05 & prior <= .95){
    prior <- update(prior, coin_randomizer)
    n_flips <- n_flips + 1
  }
  
  # Just for updating progress
  print(x)
  return(prior * is_fair_coin1)
  
}


# Don't need as many tests
n_tests <- 20000

test_results <- 1:n_tests %>% 
  map(function(x) test_the_sim(x)) %>% 
  unlist()


true_positivies <- sum(test_results > .95)
true_negatives <- sum(test_results < .004)

(true_negatives + true_positivies) / n_tests

# !It works 96% of the time




#### Rock paper scissors double scissors ####
# Besides the usual three options that players have — rock, paper or scissors — let’s add a fourth option, double scissors, which is played by making a scissors with two fingers on each side (like a Vulcan salute). Double scissors, being larger and tougher, defeat regular scissors, and just like regular scissors, they cut paper and are smashed by rock. The three traditional options interact just as they do in the standard game.
# 
# A rock-paper-scissors-double scissors match is always played best two out of three (or, more precisely, first to win two throws, since there can be an unlimited number of ties). There is just one exception: If your opponent throws paper and you throw regular scissors, you immediately win the match regardless of the score.
# 
# What is the optimal strategy at each possible score (0-0, 1-0, 0-1, 1-1)? (You can ignore any ties.) What is the probability of winning the match given a 1-0 lead?

options <- c("r", "p", "s", "ds")

play_game <- function(){
  
  hand <- sample(options, 2)
  
  # All the ways that hand one wins
  result <- case_when(hand[1] == "r" & hand[2] == "ds" ~ 1,
            hand[1] == "r" & hand[2] == "s" ~ 1,
            hand[1] == "p" & hand[2] == "r" ~ 1,
            hand[1] == "s" & hand[2] == "p" ~ 2,
            hand[1] == "ds" & hand[2] == "p" ~ 1,
            hand[1] == "ds" & hand[2] == "s" ~ 1,
            # The ways you lose immediately
            hand[1] == "p" & hand[2] == "s" ~ -2,
            TRUE ~ 0)
  
  return(c(result, hand[1]))
}


play_series <- function(){
  
  win_count <- 0
  
  first <- play_game()
  
  win_count <- win_count + as.numeric(first[1])
  
  if(first[1] == 2 | first[1] == -2) {
    return(c(first, win_count))
    }
  
  second <- play_game()
  
  win_count <- win_count + as.numeric(second[1])
  
  if(second[1] == 2 | 
     as.numeric(first[1]) + as.numeric(second[1]) >= 2 |
     # Opponent wins
     as.numeric(first[1]) + as.numeric(second[1]) <= 0) {
    return(c(first, second, win_count))
  }
  
  third <- play_game()
  
  win_count <- win_count + as.numeric(third[1])
  
  return(c(first, second, third, win_count))
  
}


games <- 1:100000 %>% 
  map(function(x) play_series())



find_best_0_0 <- function(game){
  vec <- unlist(game)
  result <- as.numeric(vec[length(vec)])
  if(result > 1){
    return(vec[2])
  }
}

games %>% 
  map(function(game) find_best_0_0(game)) %>% 
  unlist() %>% 
  table()


find_best_1_0 <- function(game){
  vec <- unlist(game)
  result <- as.numeric(vec[length(vec)])
  after_first <- as.numeric(vec[1])
  if(after_first == 1 & result > 1){
    return(vec[4])
  }
}

games %>% 
  map(function(game) find_best_1_0(game)) %>% 
  unlist() %>% 
  table()


find_best_0_1 <- function(game){
  vec <- unlist(game)
  result <- as.numeric(vec[length(vec)])
  after_first <- as.numeric(vec[1])
  if(after_first == 0 & result > 1){
    return(vec[4])
  }
}

games %>% 
  map(function(game) find_best_0_1(game)) %>% 
  unlist() %>% 
  table()


find_best_1_1 <- function(game){
  vec <- unlist(game)
  result <- as.numeric(vec[length(vec)])
  after_first <- as.numeric(vec[1])
  if(after_first == 0 & result > 1){
    return(vec[4])
  }
}

games %>% 
  map(function(game) find_best_0_1(game)) %>% 
  unlist() %>% 
  table()
