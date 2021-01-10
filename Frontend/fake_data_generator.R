library(tidyverse)
teams <- sapply(1:30, function(x) paste0(sample(LETTERS, 3, TRUE), collapse = ""))


tibble(teamA = teams[seq(1, 29, by =2)],
       teamB = teams[seq(2, 30, by = 2)],
       probA = runif(15),
       probB = 1 - probA,
       date = sample(c('2021-01-09', '2021-01-10'), 15, T)) %>% 
  write_csv("game_predictions.csv")
