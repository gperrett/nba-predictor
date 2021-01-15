library(tidyverse)
setwd('~/Dropbox/Data/Projects/nba-predictor')

# read in results from first elo run
elos <- read_csv('Elo/Data/historical_elo.csv')

# define parameters and basic functions
c_factor <- 400
get_exp_win <- function(rating_team_A, rating_team_B, c_factor=400){
  exp_win <- 1 / (1 + 10^((rating_team_B - rating_team_A)/c_factor))
  return(exp_win)
}

# usage
get_exp_win(1500, 1500)
get_exp_win(c(1500, 1600), c(1700, 1500))

# filter out first season b/c noisy and get expected win probabilities
elos <- elos %>% 
  filter(season > 1991) %>% 
  mutate(team1_exp_win = get_exp_win(team1_elo, team2_elo))

# pivot data longer 
elo_long <- elos %>% 
  mutate(team1 = paste0(team1, ":", team1_elo),
         team2 = paste0(team2, ":", team2_elo)) %>% 
  pivot_longer(cols = c(team1, team2)) %>%
  separate(value, sep = ":", into = c("team", "elo")) %>% 
  select(date, season, team, elo) %>% 
  mutate(elo = as.numeric(elo))


# home court advantage ----------------------------------------------------
# expected win is based on the elo score *after* the game so its not perfect 
  # but should be close enough

# conditional on expected win, how often the home team wins 
elos %>% 
  filter(team1_exp_win >= 0.5) %>% 
  summarize(mean(score1 > score2))

# conditional on expected lose, how often the home team wins 
elos %>% 
  filter(team1_exp_win < 0.5) %>% 
  summarize(mean(score1 > score2))


# team fatigue ------------------------------------------------------------


