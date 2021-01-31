setwd('~/Dropbox/nba-predictor/Players/')
library(tidyverse)
library(lubridate)

game_logs <- read_csv('game_logs.csv') %>% dplyr::select(-X1)
player_df <- read_csv('player_df.csv')  %>% dplyr::select(-X1)

dat <- left_join(game_logs, player_df, by = c("Player_Key" = "player_link"))

dat <-dat %>% 
  # recode home and away
  mutate(location = 
           if_else(is.na(dat$`Unnamed: 5`), 'home', 'away')) %>% 
  dplyr::select(-`Unnamed: 5`) 

dat <- dat %>% filter(`3P%` != '3P%'| is.na(`3P%`))

# only include seasons with real plus minus
dat <- dat %>% filter(is.na(`+/-`) == F)


rm(game_logs)
rm(player_df)




ages <- lapply(str_split(dat$Age, "-"), as.numeric)

process_age <- function(vec){
out <- (as.numeric(vec[1])*365) + vec[2]
out <- as.data.frame(out)
return(out)
}


process_age(ages[[1]])
ages <- map(ages, process_age)
ages <- bind_rows(ages)

# add trandformed age var 
dat$Age <- ages$out
rm(ages)


# process height ----------------------------------------------------------

heights <- lapply(str_split(dat$Ht, "-"), as.numeric)

process_height <- function(vec){
  out <- (as.numeric(vec[1])*12) + vec[2]
  out <- as.data.frame(out)
  return(out)
}

heights <- map(heights, process_height)
heights <- bind_rows(heights)

dat$Ht <- heights$out
rm(heights)


# Process minutes played  -------------------------------------------------
minutes <- lapply(str_split(dat$MP, ":"), as.numeric)

process_mp<- function(vec){
  out <- ((vec[1]*60) + vec[2])/60
  out <- as.data.frame(out)
  return(out)
}


minutes <- map(minutes, process_mp)
dat$MP <- minutes$out
rm(minutes)


# Create Season var -------------------------------------------------------

dat$Date <- ymd(as.Date(dat$Date))

season_2020 <- dat %>% filter(year(Date) == 2020)
season_2020_resid <- dat %>% filter(year(Date) == 2019)
season_2020_resid <- season_2020_resid %>% filter(month(Date) > 8)
season_2020 <- rbind(season_2020_resid, season_2020)
season_2020$season <- '2020'
rm(season_2020_resid)

non_2020 <- dat %>% anti_join(season_2020)

non_2020 <- non_2020 %>% 
  arrange(Date) %>% 
  mutate(season = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

dat <- rbind.data.frame(non_2020, season_2020)
rm(season_2020)
rm(non_2020)

# Select variables for model  ---------------------------------------------

dat <- dat %>% 
  dplyr::select(2:23, 26:31, 34:36, 39:40)

dat <- dat %>% 
  mutate(win = 
           if_else(str_detect(dat$`Unnamed: 7`, 'W'), 1, 0)) %>% 
  select(-`Unnamed: 7`)
    

# Change type  ------------------------------------------------------------

cats <- dat %>% 
  dplyr::select(Player, Date, Opp, Tm, location, Pos, season)

nums <- dat %>% 
  dplyr::select(-c(Player, Date, Opp, Tm, location, Pos, season))

nums <- map_df(nums, as.numeric)
nums <- nums %>% mutate_all(~replace(., is.na(.), 0))


dat <- cbind.data.frame(cats, nums)

write_csv(dat, 'cleaned_player_logs.csv')
