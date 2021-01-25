# -- Pull game logs for all NBA players for a specific season or range of seasons --:

library(tidyverse)
# https://github.com/bobbyingram/bballR
# package to scrape basketball-reference.com in R
library(bballR) 

# Extract all playerids & set years of interest:
players <- scrape_all_players()[,2]
years <- as.numeric(c(1990:2021))

stats <- list()
player_df <- list()

# Get game logs for every player and every season in 'years':
for (j in 1:nrow(players)) {
  stats <- list()
  for (i in 1:length(years)) {
    try(stats[[i]] <- scrape_game_logs(player_id = players[j,][[1]], year = years[i]))
    df <- plyr::ldply(stats, data.frame)
  }
  player_df[[j]] <- df
}

# Write to csv:
df_final <- plyr::rbind.fill(player_df)
write_csv(df_final, "player_logs.csv")