import pandas as pd
import numpy as np
from datetime import date
import os
import matplotlib.pyplot as plt
import seaborn as sns

# set directory
os.chdir('/home/joemarlo/Dropbox/Data/Projects/nba-predictor')

### read in and clean data
# read in the historical game data
game_data = pd.read_csv("Elo/Data/nba_elo.csv")

# retain only important columns and filter to historical data only
game_data = game_data[['date', 'season', 'team1', 'team2', 'score1', 'score2']]
game_data['date'] = pd.to_datetime(game_data['date'], format='%Y-%m-%d')
is_historical = game_data[['date']] < pd.to_datetime(date.today())
game_data = game_data[np.array(is_historical)]

### define parameters and base functions
# parameters
rookie_elo = 1300
k_factor = 24 #starting value for K factor; 538 starts theirs at 24
c_factor = 400 #set to 400 in chess system. means that an opponent that is 200 pts greater has a 75% chance of winning
u_elo = 1500 #mean score to normalize to
sd_elo = 200 #standard deviation that the Elo scores are normalized to after each race

#curve for k_factor; varies with game not season
plt.plot(1 / (np.linspace(1, 200)**(1/10)) * k_factor)
#plt.show()

def get_exp_win(rating_team_A, rating_team_B, c_factor=c_factor):
    exp_win = 1 / (1 + 10 ** ((rating_team_B - rating_team_A)/c_factor))
    return exp_win

def calc_score(tenure, actual_win, expected_win, previous_rating, k_factor=k_factor):
    # actual_win = 1 if win, 0 if loss
    personal_k_factor = k_factor * (1 / (tenure ** (1/10)))
    new_rating = previous_rating + personal_k_factor * (actual_win - expected_win)
    return new_rating


# usage
#exp_win = get_exp_win(1500, 2000)
#calc_score(10, 1, exp_win, 1500)
