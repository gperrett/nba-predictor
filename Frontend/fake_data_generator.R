library(tidyverse)
setwd('~/Dropbox/Data/Projects/nba-predictor')

# read in the data
elo_ratings <- read_csv("Elo/Data/adjusted_historical_elo.csv")

# filter to last three dates, calculate rough exp probabilties, and write out
elo_ratings %>% 
  select(date, teamA = team1, teamB = team2, probA = team1_exp_win, probB = team2_exp_win) %>%
  filter(date >= nth(unique(date), 3, order_by = rev(sort(unique(date))))) %>% 
  select(teamA, teamB, probA, probB, date) %>% 
  arrange(date) %>% # this determines the order of the date sections on predictions page
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
