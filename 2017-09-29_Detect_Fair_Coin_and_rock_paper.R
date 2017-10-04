# Riddler Classic
# Speaking of ways to randomly settle scores, how about some coin flipping? From James Nugent, a numismatic detective problem:
#   
#   On the table in front of you are two coins. They look and feel identical, but you know one of them has been doctored. The fair coin comes up heads half the time while the doctored coin comes up heads 60 percent of the time. How many flips — you must flip both coins at once, one with each hand — would you need to give yourself a 95 percent chance of correctly identifying the doctored coin?
# 
# Extra credit: What if, instead of 60 percent, the doctored coin came up heads some P percent of the time? How does that affect the speed with which you can correctly detect it?




#### Method one: using the beta distribution:
n_flips <- 1
delta = 1

while(delta > 0){
  # min_biased is actually the 2.5% quantile for the beta distribution for the biased coin
  min_biased <- qbeta(.025, n_flips * .6, n_flips * .4)
  max_fair <- qbeta(.975, n_flips * .5, n_flips * .5)
  
  delta = max_fair - min_biased
  
  n_flips = n_flips + 1
}

# Otherwise the delta will be less than 1
n_flips = n_flips - 1


# Plot it
x <- seq(0, 1, .01)
n_flips_needed <- 369

q <- dbeta(x, n_flips_needed * .6, n_flips_needed * .4)
z <- dbeta(x, n_flips_needed * .5, n_flips_needed * .5)

df <- data.frame(x, q, z)
# Make it into a long data frame so we can use some inherent properties of ggplot2 w/r/t aesthetics.

df <- gather(df, func, val, -x)
# now, we can use aesthetic mapping vs hard-coding values and doing multiple geom_line()s.

gg <- ggplot(df, aes(x=x, y=val, group=func))
gg <- gg + geom_line(aes(color=func)) 
# Tighten up the y axis limits a bit

gg <- gg + scale_y_continuous(expand=c(0, 0))
# We can actually get a legend now (you could also remove it and manually label the lines with geom_text())

gg <- gg + scale_color_manual(name="Beta params", 
                              values=c("#b2182b", "#4393c3"),
                              labels=c("Biased Coin", "Fair Coin"))
# Combine multiple label statements into one.

gg <- gg + labs(x="x value", y="PDF",
                title="Beta Probability Distribution Functions")
# Remove some chart junk.

gg <- gg + theme_bw()
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
gg




#### Method two: Bayesian updating simulation ####
# H = coin1 = fair 

update <- function(prior) {
  
  # Flip
  # The first coin is the fair coin. 
  # It returns a .4 or .6 because that is what we need for Bayes' theorum
  # i.e., P(X|not A)
  flip_coin1 <- sample(c(.4,.6), 1, prob = c(.5, .5))
  # The second coin is biased
  flip_coin2 <- sample(c(.4,.6), 1, prob = c(.4, .6))
  
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
  n_flips <- 0
  prior <- .5
  
  while(prior >= .05 & prior <= .95){
    prior <- update(prior)
    
    n_flips <- n_flips + 1
    
  }
  
  print(x)
  print(prior)
  return(n_flips)
  
}


test3 <- 1:1e6 %>% 
  map(function(x) simulate(x)) %>% 
  unlist()




#### Rock paper scissors double scissors ####
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
            TRUE ~ 0)
  
  return(c(result, hand[1]))
}


play_series <- function(){
  
  win_count <- 0
  
  first <- play_game()
  
  win_count <- win_count + as.numeric(first[1])
  
  if(first[1] == 2) {
    return(c(first, win_count))
    }
  
  second <- play_game()
  
  win_count <- win_count + as.numeric(second[1])
  
  if(second[1] == 2 | 
     as.numeric(first[1]) + as.numeric(second[1]) >= 2 |
     as.numeric(first[1]) + as.numeric(second[1]) == 0) {
    return(c(first, second, win_count))
  }
  
  third <- play_game()
  
  win_count <- win_count + as.numeric(third[1])
  
  return(c(first, second, third, win_count))
  
}


games <- 1:100 %>% 
  map(function(x) play_series())










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

