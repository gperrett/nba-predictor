library(tidyverse)
setwd('~/Dropbox/Data/Projects/nba-predictor')
source("Plots/ggplot_settings.R")

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
  mutate(team1_exp_win = get_exp_win(team1_elo, team2_elo),
         index = 1:nrow(.)) %>% 
  select(-X1)

# pivot data longer 
elo_long <- elos %>% 
  mutate(team1 = paste0(team1, ":", score1>score2, ":", team1_elo, ":", team1_exp_win, ":", TRUE),
         team2 = paste0(team2, ":", score2>score1, ":", team2_elo, ":", 1-team1_exp_win, ":", FALSE)) %>% 
  pivot_longer(cols = c(team1, team2)) %>%
  separate(value, sep = ":", into = c("team", "win", "elo", 'exp_win', "home")) %>% 
  select(index, date, season, team, win, elo, exp_win, home) %>% 
  mutate(elo = as.numeric(elo),
         exp_win = as.numeric(exp_win),
         win = as.logical(win),
         home = as.logical(home))


# home court advantage ----------------------------------------------------
# expected win is based on the elo score *after* the game so its not perfect 
  # but should be close enough

# home team expected win rate vs actual win rate
elos %>% 
  summarize(exp_win_rate = mean(team1_exp_win),
            act_win_rate = mean(score1 > score2))

# conditional on expected win, how often the home team wins 
elos %>% 
  group_by(home_exp_win = team1_exp_win >= 0.5) %>% 
  summarize(home_win_rate = mean(score1 > score2),
            away_win_rate = mean(score2 > score1))

# conditional on home game, margin of victory compared to expected
# this doesn't make sense b/c win probability != MOV
elos %>% 
  select(score1, score2, team1_exp_win) %>% 
  mutate(expected_score1 = (score1 + score2) * team1_exp_win,
         expected_score2 = (score1 + score2) * (1 - team1_exp_win),
         team1_mov = score1 - score2,
         team1_exp_mov = expected_score1 - expected_score2,
         actual_over_expected = team1_mov / team1_exp_mov) %>%
  group_by(home = team1_exp_win >= 0.5) %>% 
  summarize(actual_over_expected = mean(actual_over_expected))


# team fatigue ------------------------------------------------------------

# break the dataframe into dfs per team
team_schedule <- elo_long %>% 
  arrange(team, date) %>% 
  group_by(team) %>% 
  group_split()

# per team:date combo, count the games played in the last n days
team_schedule <- map_dfr(1:7, function(n_days){
  map_dfr(team_schedule, function(df){
    max_index <- nrow(df)
    games_played <- map_dbl(1:max_index, function(index){
      df_sliced <- df[1:index,]
      games_played <- sum(df_sliced$date >= (last(df_sliced$date)-n_days))-1
      return(games_played)
    })
    
    # add games back to df
    df %>% 
      mutate(games_played = games_played,
             n_days = n_days)
  })
})

# calculate difference in actual win rate vs expected win rate
team_schedule %>% 
  group_by(games_played, n_days) %>% 
  summarize(exp_win_rate = mean(exp_win),
            act_win_rate = mean(win),
            sample_size = n()) %>%
  mutate(exp_over_act = act_win_rate - exp_win_rate,
         n_days = paste0('n days: ', n_days)) %>% 
  filter(sample_size > 500) %>% 
  select(games_played, n_days, exp_over_act, sample_size) %>% 
  ggplot(aes(x = games_played, y = exp_over_act, fill = exp_over_act)) +
  geom_col() +
  # geom_text(aes(y = 0.05, label = sample_size)) +
  geom_hline(yintercept = 0, color = 'grey20', size = 0.2) +
  scale_x_continuous(breaks = 0:7) +
  scale_fill_gradient2(mid = 'grey60', high = '#328c4a') +
  facet_wrap(~n_days) +
  labs(title = 'Difference between actual win rate and expected win rate',
       subtitle = 'Metric considered at different window sizes (n days)',
       x = 'Games played in last n days',
       y = NULL,
       fill = NULL)
# ggsave("Plots/fatigue_results.png", height = 8, width = 10)
  
  