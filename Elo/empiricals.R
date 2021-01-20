library(tidyverse)
setwd('~/Dropbox/Data/Projects/nba-predictor')
source("Plots/ggplot_settings.R")

# in retrospect, a better method would be to model the difference in exp win and 
  # observed win as a function home court and fatigue status. The below script
  # estimates the effects individually

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

# home team expected win rate vs actual win rate: binned by prediction
binned_home_adjustment <- elos %>% 
  mutate(bin = cut(team1_exp_win, breaks = seq(0, 1, by = 0.05)),
         home_win = score1 > score2) %>%
  left_join(tibble(
    midpoint_exp = seq(0.05, 1, by = 0.05) - 0.025,
    bin = cut(midpoint_exp, seq(0, 1, by = 0.05))
  ), by = 'bin') %>% 
  group_by(midpoint_exp) %>% 
  summarize(observed_win_rate = mean(home_win)) %>% 
  mutate(diff = observed_win_rate - midpoint_exp)
binned_home_adjustment %>% 
  ggplot(aes(x = midpoint_exp, y = diff)) +
  geom_hline(yintercept = 0, color = 'grey70', linetype = 'dashed') +
  geom_smooth(method = 'loess', color = 'grey50') +
  geom_line() +
  geom_point() +
  labs(title = 'Adjustment for home court advantage',
       subtitle = 'Home court advantage has the strongest effect on teams predicted to lose badly and teams predicted slightly beat their opponents',
       x = 'Original binned win probability',
       y = 'Adjustment to win probability')
# ggsave("Plots/home_court_adjustment.png", height = 5, width = 10)

# smooth the curve and write out to df
loess(diff ~ midpoint_exp, data = binned_home_adjustment) %>% 
  predict() %>% 
  tibble(prediction = binned_home_adjustment$midpoint_exp,
         adjustment = .) %>% 
  write_csv('Elo/Data/home_court_adjustment.csv')

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

# home team expected win rate vs actual win rate: binned by prediction
elo_long_fatigue <- elo_long %>% 
  arrange(team, date) %>% 
  group_by(team) %>% 
  group_split() %>% 
  map_dfr(function(df){
    max_index <- nrow(df)
    games_played <- map_dbl(1:max_index, function(index){
      df_sliced <- df[1:index,]
      games_played <- sum(df_sliced$date >= (last(df_sliced$date)-1))-1
      return(games_played)
    })
    
    # add games back to df
    df %>% 
      mutate(fatigue = games_played > 0)
  })
binned_fatigue_adjustment <- elo_long_fatigue %>% 
  mutate(bin = cut(exp_win, breaks = seq(0, 1, by = 0.05))) %>%
  left_join(tibble(
    midpoint_exp = seq(0.05, 1, by = 0.05) - 0.025,
    bin = cut(midpoint_exp, seq(0, 1, by = 0.05))
  ), by = 'bin') %>% 
  group_by(midpoint_exp, fatigue) %>% 
  summarize(observed_win_rate = mean(win)) %>%
  mutate(diff = observed_win_rate - midpoint_exp)

# plot the absolute difference between teams w/ and w/o fatigue
# note: the implemented adjustment will be the difference between observed_win_rate
  # and midpoint_exp per teams with or without fatigue
binned_fatigue_adjustment %>%
  select(midpoint_exp, fatigue, observed_win_rate) %>%
  pivot_wider(names_from = fatigue, values_from = observed_win_rate) %>%
  mutate(diff = `TRUE` - `FALSE`) %>%
  ggplot(aes(x = midpoint_exp, y = diff)) + #, color = fatigue, group = fatigue)) +
  geom_hline(yintercept = 0, color = 'grey70', linetype = 'dashed') +
  geom_smooth(method = 'loess', color = 'grey50') +
  geom_line() +
  geom_point() +
  labs(title = 'Adjustment for fatigue',
       subtitle = 'Fatigue has the strongest effect on teams that are predicted to slightly beat their opponents',
       x = 'Original binned win probability',
       y = 'Adjustment to win probability',
       caption = 'Fatigue defined as playing at least one game in same or previous day')
# ggsave("Plots/fatigue_adjustment.png", height = 5, width = 10)

# smooth the curve and write out to df
loess(diff ~ midpoint_exp + fatigue, data = binned_fatigue_adjustment) %>% 
  predict() %>% 
  tibble(prediction = binned_fatigue_adjustment$midpoint_exp,
         fatigue = binned_fatigue_adjustment$fatigue,
         adjustment = .) %>% 
  write_csv('Elo/Data/fatigue_adjustment.csv')


# model -------------------------------------------------------------------

# create df indicating if home and if fatigue
elo_interaction <- elo_long_fatigue %>% 
  mutate(bin = cut(exp_win, breaks = seq(0, 1, by = 0.05))) %>%
  left_join(tibble(
    midpoint_exp = seq(0.05, 1, by = 0.05) - 0.025,
    bin = cut(midpoint_exp, seq(0, 1, by = 0.05))
  ), by = 'bin') %>% 
  group_by(midpoint_exp, fatigue, home) %>% 
  summarize(observed_win_rate = mean(win)) %>%
  mutate(diff = observed_win_rate - midpoint_exp)

elo_interaction %>% 
  mutate(group = paste0(home,':',fatigue)) %>% 
  ggplot(aes(x=midpoint_exp, y=observed_win_rate, group=group, color=group)) +
  geom_abline(linetype='dashed', color='grey50') +
  geom_line() +
  geom_point() +
  labs(title = 'Difference between actual win rate and expected win rate',
       caption = 'Fatigue defined as playing at least one game in same or previous day',
       x = 'Original binned win probability',
       y = 'Observed win probability',
       color = 'Home:Fatigue')

elo_interaction %>%
  mutate(group = paste0(home,':',fatigue)) %>% 
  ggplot(aes(x=midpoint_exp, y=diff, group=group, color=group)) +
  geom_hline(yintercept = 0, color = 'grey70', linetype = 'dashed') +
  geom_smooth(method = 'loess', color = 'grey50') +
  geom_line() +
  geom_point() +
  labs(title = 'Adjustment to win probability due to fatigue and home court advantage',
       caption = 'Fatigue defined as playing at least one game in same or previous day',
       x = 'Original binned win probability',
       y = 'Adjustment to win probability',
       color = 'Home:Fatigue')

# write out adjustments
loess(diff ~ midpoint_exp + fatigue*home, data=elo_interaction) %>% 
  predict() %>% 
  tibble(prior_prediction = elo_interaction$midpoint_exp,
         fatigue = elo_interaction$fatigue,
         home = elo_interaction$home,
         adjustment = .) %>% 
  write_csv('Elo/Data/adjustments.csv')
