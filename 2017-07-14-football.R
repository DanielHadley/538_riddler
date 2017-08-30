library(tidyverse)
set.seed(4321)

# https://fivethirtyeight.com/features/can-you-eat-more-pizza-than-your-siblings/?src=obbottom=ar_5

# Congratulations! The Acme Axegrinders, which you own, are the regular season champions of the National Squishyball League (NSL). Your team will now play a championship series against the Boondocks Barbarians, which had the second-best regular season record. You feel good about Acme’s chances in the series because Acme won exactly 60 percent of the hundreds of games it played against Boondocks this season. (The NSL has an incredibly long regular season.) The NSL has two special rules for the playoffs:
#   
#   The owner of the top-seeded team (i.e., you) gets to select the length of the championship series in advance of the first game, so you could decide to play a single game, a best two out of three series, a three out of five series, etc., all the way up to a 50 out of 99 series.
# The owner of the winning team gets $1 million minus $10,000 for each of the victories required to win the series, regardless of how many games the series lasts in total. Thus, if the top-seeded team’s owner selects a single-game championship, the winning owner will collect $990,000. If he or she selects a 4 out of 7 series, the winning team’s owner will collect $960,000. The owner of the losing team gets nothing.
# Since Acme has a 60 percent chance of winning any individual game against Boondocks, Rule 1 encourages you to opt for a very long series to improve Acme’s chances of winning the series. But Rule 2 means that a long series will mean less winnings for you if Acme does take the series.
# 
# How long a series should you select in order to maximize your expected winnings? And how much money do you expect to win?


games <- seq(1, 99, 2)
winsNeeded <- map(games, function(x) (x / 2) + .5) %>% unlist()

#Sim
n_sims <- 100000

winGameOrNot <- function(){
  sample(c(1,0), size = 1, prob = c(.6,.4))
}

winSeriesOrNot <- function(games){
  sum(replicate(games, winGameOrNot()))
}

computeResults <- function(games, winsNeeded){
  results <- replicate(n_sims, winSeriesOrNot(games))
  theyWon <- sum(results >= winsNeeded) / n_sims
  print(games)
  return(theyWon)
}

probWin <- map2(games, winsNeeded, computeResults) %>% unlist()

winnings <- map(winsNeeded, function(x) 1e6 - (x * 10000)) %>% unlist()

theData <- tibble(games, winsNeeded, probWin, winnings) %>% 
  mutate(expected.value = probWin * winnings) %>% 
  arrange(desc(expected.value))