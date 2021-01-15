library(tidyverse)
setwd('~/Dropbox/Data/Projects/nba-predictor')

# predictions -------------------------------------------------------------
teams <- sapply(1:30, function(x) paste0(sample(LETTERS, 3, TRUE), collapse = ""))

tibble(teamA = teams[seq(1, 29, by =2)],
       teamB = teams[seq(2, 30, by = 2)],
       probA = runif(15),
       probB = 1 - probA,
       date = sample(c('2021-01-09', '2021-01-10', '2021-01-11'), 15, T)) %>% 
  write_csv("Frontend/Data/game_predictions.csv")


# historical ratings ------------------------------------------------------

conf <- c("Eastern", "Western")
means <- rnorm(length(teams), 1, 0.1)
games <- 60

map2_dfr(.x = teams, .y = means, .f = function(team, mean){
  
  # simulate random walk of ratings
  mymeans <- c()
  for (i in 1:games){
    mean <- rnorm(1, mean, 0.01)
    mymeans[i] <- mean
  }
  
  # create dataframe
  dat <- tibble(date = Sys.Date() - 1:games,
         team = team,
         rating = mymeans * 1500,
         conference = rep(sample(conf, 1), games))
  
  # rolling regression
  indices <- 1:nrow(dat)
  n_days <- 15
  coefs <- map_dbl(indices, function(index){
    coef(lm(rating ~ date, data = dat[index:(index+n_days),]))['date']
  }) 
  
  # combine and return
  dat %>% 
    mutate(rating_delta = coefs*n_days)
}) %>% 
  write_csv("Frontend/Data/team_ratings.csv")
