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


# fudged data from historical elos ----------------------------------------

# read in the data
elo_ratings <- read_csv("Elo/Data/adjusted_historical_elo.csv")

# filter to last three dates, calculate rough exp probabilties, and write out
elo_ratings %>% 
  select(date, teamA = team1, teamB = team2, team1_elo, team2_elo) %>% 
  filter(date >= nth(unique(date), 3, order_by = rev(sort(unique(date))))) %>% 
  mutate(probA = 1 / (1 + 10^((team2_elo - team1_elo)/400)),
         probB = 1 - probA) %>% 
  select(teamA, teamB, probA, probB, date) %>% 
  write_csv("Frontend/Data/game_predictions.csv")

# teams by conference
active_teams <- elo_ratings %>% 
  filter(season == 2021) %>% 
  select(team1, team2) %>% 
  pivot_longer(cols = everything()) %>% 
  distinct(value) %>% 
  pull() %>% 
  sort()
eastern <- c("BOS", "PHI", "MIL", "IND", "ORL", "BRK", "ATL", "CHA", "NYK", 'CLE', 'MIA', 'CHI', 'TOR', 'WAS', 'DET')
western <- setdiff(active_teams, eastern)
conferences <- tibble(team = c(eastern, western), conference = c(rep("Eastern", 15), rep("Western", 16)))

# pivot longer, filter to just this season, and add conference
historical_ratings <- elo_ratings %>%
  filter(season == 2021 | season == 2020) %>% 
  mutate(team1 = paste0(team1, ":", team1_elo),
         team2 = paste0(team2, ":", team2_elo)) %>% 
  pivot_longer(cols = c(team1, team2)) %>%
  separate(value, sep = ":", into = c("team", "elo")) %>% 
  select(date, team, rating = elo) %>% 
  mutate(rating = as.numeric(rating)) %>% 
  left_join(conferences, by = 'team')

# add rating trend, filter to last 20 games and write out
historical_ratings %>%
  arrange(desc(date), team) %>% 
  group_by(team) %>%
  mutate(index = n():1) %>% 
  group_split() %>% 
  map_dfr(., function(team_df){
    indices <- 1:nrow(team_df)
    n_games <- 10
    coefs <- map_dbl(indices, function(index){
      coef(lm(rating ~ index, data = team_df[index:(index+n_games-1),]))['index']
    })
    team_df %>% 
      mutate(rating_delta = coefs*n_games)
  }) %>% 
  select(-index) %>% 
  group_by(team) %>% 
  mutate(latest = if_else(date == max(date), 1, 0)) %>% 
  slice_head(n = 20) %>% 
  write_csv("Frontend/Data/team_ratings.csv")
