import pandas as pd
import numpy as np
from datetime import date
import os
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import scale

# set directory
os.chdir('/home/joemarlo/Dropbox/Data/Projects/nba-predictor')

### read in and clean data
# read in the historical game data
game_data = pd.read_csv("Elo/Data/nba_elo.csv")

# retain only important columns and filter to the last 30 years
game_data = game_data[['date', 'season', 'team1', 'team2', 'score1', 'score2']]
game_data['date'] = pd.to_datetime(game_data['date'], format='%Y-%m-%d')
is_historical = (game_data[['date']] < pd.to_datetime('20200108', format='%Y%m%d')) & (game_data[['date']] > pd.to_datetime('19901001', format='%Y%m%d'))
game_data = game_data.loc[np.array(is_historical)].reset_index()

### define parameters and base functions
# parameters
rookie_elo = 1300
k_factor = 24 #starting value for K factor; 538 starts theirs at 24
c_factor = 400 #set to 400 in chess system. means that an opponent that is 200 pts greater has a 75% chance of winning
u_elo = 1500 #mean rating to normalize to
sd_elo = 200 #standard deviation that the Elo rating are normalized to after each race

#curve for k_factor; varies with game not season
plt.plot(1 / (np.linspace(1, 200)**(1/10)) * k_factor)
#plt.show()

def get_exp_win(rating_team_A, rating_team_B, c_factor=c_factor):
    exp_win = 1 / (1 + 10 ** ((rating_team_B - rating_team_A)/c_factor))
    return exp_win

def calc_rating(tenure, actual_win, expected_win, previous_rating, k_factor=k_factor):
    # actual_win = 1 if win, 0 if loss
    personal_k_factor = k_factor * (1 / (tenure ** (1/10)))
    new_rating = previous_rating + personal_k_factor * (actual_win - expected_win)
    return new_rating

def normalize_elo(elo_ratings, u_elo=u_elo, sd_elo=sd_elo):
    elo_ratings = elo_ratings * u_elo / np.mean(elo_ratings)
    scale_factor = sd_elo / np.std(elo_ratings)
    new_ratings = [u_elo + (scale_factor * rating * np.std(elo_ratings)) for rating in scale(elo_ratings)]
    elo_ratings['rating'] = np.array(new_ratings)
    return elo_ratings

# usage
#exp_win = get_exp_win(1500, 2000)
#calc_rating(10, 1, exp_win, 1500)

### initialize rating dataframe
# get unique dates and games
dates = game_data.date.unique()
teams1 = game_data.team1.unique()
teams2 = game_data.team2.unique()
teams = np.concatenate((teams1, teams2))
teams = pd.Series(teams).unique()

# df of latest elo ratings
current_elo_ratings = pd.DataFrame({'rating': rookie_elo}, index=teams)

# intialize lists to append elo results to
elo_dates = []
elo_rating_team1 = []
elo_rating_team2 = []

# iterate over the game data frame and calculate the new elo ratings
# takes ~10min
for index, row in game_data.iterrows():

    # print status
    if index % 100 == 0: print("On game: ", index, " of ", len(game_data))

    # extract team names, date, and winner
    team1 = row['team1']
    team2 = row['team2']
    game_date = row['date']
    winner1 = row['score1'] > row['score2']

    # retrive tenure for these teams
    tenure_team1 = 10 # tenures[[game_date][team1]]
    tenure_team2 = 10

    # retrieve latest elo rating
    previous_rating_team1 = current_elo_ratings.loc[[team1]].values
    previous_rating_team2 = current_elo_ratings.loc[[team2]].values

    ## get new ratings
    # team1
    exp_team1 = get_exp_win(previous_rating_team1, previous_rating_team2)
    rating_team1 = calc_rating(tenure_team1, winner1, exp_team1, previous_rating_team1)

    ## team2
    exp_team2 = get_exp_win(previous_rating_team2, previous_rating_team1)
    rating_team2 = calc_rating(tenure_team2, not winner1, exp_team2, previous_rating_team2)

    ## save results
    # save the ratings to the current elo df
    current_elo_ratings.loc[[team1]] = rating_team1
    current_elo_ratings.loc[[team2]] = rating_team2

    # normalize elo ratings
    # TODO: remove historical teams? normalize at end of each day instead?
    current_elo_ratings = normalize_elo(current_elo_ratings)

    # save the ratings to the history
    elo_dates.append(game_date)
    elo_rating_team1.append(float(current_elo_ratings.loc[team1].values))
    elo_rating_team2.append(float(current_elo_ratings.loc[team2].values))

# write out results
final_elo_ratings = game_data #.iloc[np.linspace(0 , 6582, 6582)]
final_elo_ratings['team1_elo'] = elo_rating_team1
final_elo_ratings['team2_elo'] = elo_rating_team2
final_elo_ratings.to_csv('Elo/Data/historical_elo.csv')
