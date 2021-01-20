library(tidyverse)
setwd('~/Dropbox/Data/Projects/nba-predictor')
source("Plots/ggplot_settings.R")

# read in results from second elo run
elos_adjusted <- read_csv('Elo/Data/adjusted_historical_elo.csv')


# calibration -------------------------------------------------------------

# derive a confidence interval via bootstrap
bootstrap <- map_dfr(1:1000, function(i){
  elos_adjusted %>%
    filter(season >= 2000) %>%
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

# calibration plot
bootstrap %>%
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
