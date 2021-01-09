library(tidyverse)
setwd('/home/joemarlo/Dropbox/Data/Projects/nba-predictor')
theme_set(theme_minimal())

# read in the data
elo_ratings <- read_csv("Elo/Data/historical_elo.csv")

# pivot data longer
elo_long <- elo_ratings %>% 
  mutate(team1 = paste0(team1, ":", team1_elo),
         team2 = paste0(team2, ":", team2_elo)) %>% 
  pivot_longer(cols = c(team1, team2)) %>%
  separate(value, sep = ":", into = c("team", "elo")) %>% 
  select(date, team, elo) %>% 
  mutate(elo = as.numeric(elo))

# plot elo scores over time
elo_long %>% 
  ggplot(aes(x = date, y = elo, group = team, color = team)) +
  geom_line(alpha = 0.3) +
  labs(title = "Team elo ratings",
       x = NULL,
       y = NULL,
       color = NULL)
# ggsave("Plots/historical_elo_ratings.png", height = 5, width = 8)

# elo ratings by year (not season)
elo_long %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year > 2010) %>% 
  group_by(year, team) %>% 
  filter(date == max(date)) %>%
  group_by(year) %>% 
  mutate(rank = rank(-elo)) %>% 
  ggplot(aes(x = 1, y = rank)) +
  geom_text(aes(label = team)) +
  scale_x_continuous(labels = NULL) +
  facet_wrap(~year, nrow = 1) +
  labs(title = "Teams ranked by elo rating by year (not season)",
       x = NULL,
       y = NULL)
# ggsave("Plots/historical_elo_rankings.png", height = 8, width = 10)
