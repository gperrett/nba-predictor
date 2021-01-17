library(tidyverse)
setwd('~/Dropbox/Data/Projects/nba-predictor')
source("Plots/ggplot_settings.R")


# accuracy ----------------------------------------------------------------

# read in results from first elo run
elos <- read_csv('Elo/Data/historical_elo.csv')

# pivot longer and add previous elo score
elos_long <- elos %>% 
  mutate(game = 1:n(),
         team1 = paste0(team1, ":", score1>score2, ":", team1_elo, ":", TRUE),
         team2 = paste0(team2, ":", score2>score1, ":", team2_elo, ":", FALSE)) %>% 
  pivot_longer(cols = c(team1, team2)) %>%
  separate(value, sep = ":", into = c("team", "win", "elo", "home")) %>% 
  select(game, date, season, team, win, elo, home) %>% 
  mutate(elo = as.numeric(elo),
         win = as.logical(win),
         home = as.logical(home)) %>% 
  group_by(team) %>% 
  mutate(elo_pre = lag(elo)) %>% 
  ungroup() 

# add projected win columns
elos_wins <- elos_long %>%
  mutate(home = if_else(home, 'home', 'away')) %>% 
  pivot_wider(names_from = home, names_sep = ".",
              values_from = c(elo, elo_pre, team, win)) %>%
  mutate(home_win = win.home,
         projected_home_win = elo_pre.home > elo_pre.away) %>% 
  select(game, date, season, home_win, projected_home_win) %>% 
  na.omit()

# accuracy 
conf_mat <- table(elos_wins$home_win, elos_wins$projected_home_win)
(conf_mat['FALSE', 'FALSE'] +  conf_mat['TRUE', 'TRUE']) / nrow(elos_wins)


# read in results from second elo run
elos_adjusted <- read_csv('Elo/Data/adjusted_historical_elo.csv')

# pivot longer and add previous elo score
elos_adj_long <- elos_adjusted %>% 
  mutate(game = 1:n(),
         team1 = paste0(team1, ":", score1>score2, ":", team1_elo, ":", TRUE),
         team2 = paste0(team2, ":", score2>score1, ":", team2_elo, ":", FALSE)) %>% 
  pivot_longer(cols = c(team1, team2)) %>%
  separate(value, sep = ":", into = c("team", "win", "elo", "home")) %>% 
  select(game, date, season, team, win, elo, home) %>% 
  mutate(elo = as.numeric(elo),
         win = as.logical(win),
         home = as.logical(home)) %>% 
  group_by(team) %>% 
  mutate(elo_pre = lag(elo)) %>% 
  ungroup() 

# add projected win columns
elos_adj_wins <- elos_adj_long %>%
  mutate(home = if_else(home, 'home', 'away')) %>% 
  pivot_wider(names_from = home, names_sep = ".",
              values_from = c(elo, elo_pre, team, win)) %>%
  mutate(home_win = win.home,
         projected_home_win = elo_pre.home > elo_pre.away) %>% 
  select(game, date, season, home_win, projected_home_win) %>% 
  na.omit()

# accuracy 
conf_mat_adj <- table(elos_adj_wins$home_win, elos_adj_wins$projected_home_win)
(conf_mat['FALSE', 'FALSE'] +  conf_mat['TRUE', 'TRUE']) / nrow(elos_adj_wins)

# correct predictions over time
elos_adj_wins %>% 
  group_by(season) %>% 
  summarize(correct = mean(home_win == projected_home_win)) %>% 
  ggplot(aes(x = season, y = correct)) +
  geom_line()


# read in 538 elos
elos_538 <- read_csv('Elo/Data/nba_elo.csv')

# accuracy by season by model
elos_538 %>% 
  mutate(home_win = score1 > score2,
         projected_home_win = elo1_pre > elo2_pre) %>% 
  group_by(season) %>% 
  summarize(correct = mean(home_win == projected_home_win)) %>%
  mutate(source = '538') %>% 
  bind_rows(elos_adj_wins %>% 
               group_by(season) %>% 
               summarize(correct = mean(home_win == projected_home_win)) %>% 
               mutate(source = 'Us - adjusted')) %>%
  bind_rows(elos_wins %>% 
              group_by(season) %>% 
              summarize(correct = mean(home_win == projected_home_win)) %>% 
              mutate(source = 'Us - unadjusted')) %>% 
  filter(season > 1991) %>% 
  ggplot(aes(x = season, y = correct, group = source, color = source)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  labs(title = 'Percent of games called correct by Elo score',
       y = '% games correct',
       x = 'Season',
       color = NULL) +
  theme(legend.position = 'bottom')
# ggsave("Plots/backtest.png", height = 6, width = 10)


# calibration -------------------------------------------------------------

# calibration plot
elos_adjusted %>% 
  filter(season > 1992) %>% 
  mutate(home_win = score1 > score2) %>% 
  select(home_win, team1_exp_win) %>% 
  mutate(bin = cut(team1_exp_win, breaks = seq(0, 1, by = 0.05))) %>%
  left_join(tibble(
    midpoint = seq(0.05, 1, by = 0.05) - 0.025,
    bin = cut(midpoint, seq(0, 1, by = 0.05))
  ), by = 'bin') %>% 
  group_by(midpoint) %>% 
  summarize(observed_win_rate = mean(home_win)) %>% 
  ggplot(aes(x = midpoint, y = observed_win_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_smooth(method = 'lm', color = 'grey70', alpha = 0.2) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Calibration: Observed win rate vs. predictions",
       x = "Midpoint of binned predictions",
       y = "Mean observed win rate")

# derive a confidence interval via cross validation
cross_val <- map_dfr(1:1000, function(i){
  elos_adjusted %>% 
    filter(season > 1992) %>% 
    slice_sample(prop = 1, replace = TRUE) %>% 
    mutate(home_win = score1 > score2) %>% 
    select(home_win, team1_exp_win) %>% 
    mutate(bin = cut(team1_exp_win, breaks = seq(0, 1, by = 0.05))) %>%
    left_join(tibble(
      midpoint = seq(0.05, 1, by = 0.05) - 0.025,
      bin = cut(midpoint, seq(0, 1, by = 0.05))
    ), by = 'bin') %>% 
    group_by(midpoint) %>% 
    summarize(observed_win_rate = mean(home_win),
              .groups = 'drop') %>% 
    mutate(run = i)
})
cross_val %>% 
  group_by(midpoint) %>% 
  summarize(mean = mean(observed_win_rate),
            high = quantile(observed_win_rate, 0.95),
            low = quantile(observed_win_rate, 0.05)) %>% 
  ggplot(aes(x = midpoint, y = mean, ymax = high, ymin = low, group = midpoint)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_point() +
  geom_linerange() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Calibration: Observed win rate vs. predictions",
       subtitle = '95% confidence interval derived from 1,000 resample bootstrap',
       x = "Midpoint of binned predictions",
       y = "Mean observed win rate")
# ggsave("Plots/backtest_calibration.png", height = 6, width = 10)
