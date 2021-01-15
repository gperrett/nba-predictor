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
is_historical = (game_data[['date']] < pd.to_datetime('20210108', format='%Y%m%d')) & (game_data[['date']] > pd.to_datetime('19900601', format='%Y%m%d'))
game_data = game_data.loc[np.array(is_historical)].reset_index()

### define parameters and base functions
# parameters
rookie_elo = 1300
k_factor = 40 # grid search determined this was optimal value based on MSE of exp win - actual win
c_factor = 400 # set to 400 in chess system. means that an opponent that is 200 pts greater has a 75% chance of winning
u_elo = 1500 # mean rating to normalize to
sd_elo = 200 # standard deviation of ratings to normalize to

#curve for k_factor; varies with game not season
#plt.plot(1 / (np.linspace(1, 200)**(1/10)) * k_factor)
#plt.show()

# need to incorporate:
    # margin of victory

# home court advantage
# empiricals.R script -> 9.5 percentage points increase in win rate over expected win rate

# fatigue
# empiricals.R script -> 5 percentage points decrease in win rate over expected win rate for playing a game yesterday

# basic functions
def get_exp_win(rating_team_A, rating_team_B, home_court, fatigue, c_factor=c_factor, home_court_advantage=0.1, fatigue_penalty=0.05):

    # calculate exp win using Elo formula
    exp_win = 1 / (1 + 10 ** ((rating_team_B - rating_team_A)/c_factor))

    # reduce/increase exp win probably based on home court status
    exp_win = exp_win + (home_court * home_court_advantage) - ((not home_court) * home_court_advantage)

    # adjust for fatigue
    exp_win = exp_win - (fatigue * fatigue_penalty)

    # ensure number is between 0 and 1
    exp_win = np.min([1, np.max([0, exp_win])])

    return exp_win

def calc_rating(actual_win, expected_win, previous_rating, tenure=10, k_factor=k_factor):
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
#exp_win = get_exp_win(1500, 2000, True, True)
#calc_rating(10, 1, exp_win, 1500)

def played_yesterday(team, game_date, game_data=game_data):
    yesterday_date = game_date - pd.to_timedelta(1, unit='d')
    boolean_index = np.logical_and(np.logical_or(game_data.team1 == team, game_data.team2 == team), game_data.date == yesterday_date)
    played_yesterday = (sum(boolean_index) > 0) is True

    return played_yesterday

# get unique datesteams
teams1 = game_data.team1.unique()
teams2 = game_data.team2.unique()
teams = np.concatenate((teams1, teams2))
teams = pd.Series(teams).unique()

# initialize df of rating (this will hold the latest elo ratings)
current_elo_ratings = pd.DataFrame({'rating': rookie_elo}, index=teams)

# intialize lists to append elo results to
elo_dates = []
elo_rating_team1 = []
elo_rating_team2 = []
#k_tuning = []

# iterate over the game data frame and calculate the new elo ratings
# takes ~20min
for index, row in game_data.iterrows():

    # print status
    if index % 100 == 0: print("On game: ", index, " of ", len(game_data))

    # extract team names, date, and winner
    team1 = row['team1']
    team2 = row['team2']
    game_date = row['date']
    winner1 = row['score1'] > row['score2']

    # did these teams play yesterday?
    fatigue_team1 = played_yesterday(team1, game_date)
    fatigue_team2 = played_yesterday(team2, game_date)

    # retrieve latest elo rating
    previous_rating_team1 = float(current_elo_ratings.loc[[team1]].values)
    previous_rating_team2 = float(current_elo_ratings.loc[[team2]].values)

    ## get new ratings
    # team1
    exp_team1 = get_exp_win(previous_rating_team1, previous_rating_team2, home_court=True, fatigue=fatigue_team1)
    rating_team1 = calc_rating(winner1, exp_team1, previous_rating_team1)

    ## team2
    exp_team2 = get_exp_win(previous_rating_team2, previous_rating_team1, home_court=False, fatigue=fatigue_team2)
    rating_team2 = calc_rating(not winner1, exp_team2, previous_rating_team2)

    ## save results
    # save the ratings to the current elo df
    current_elo_ratings.loc[[team1]] = rating_team1
    current_elo_ratings.loc[[team2]] = rating_team2

    # normalize elo ratings
    # TODO: are there historical teams that should be removed?
    current_elo_ratings = normalize_elo(current_elo_ratings)

    # save the ratings to the history
    elo_dates.append(game_date)
    elo_rating_team1.append(float(current_elo_ratings.loc[team1].values))
    elo_rating_team2.append(float(current_elo_ratings.loc[team2].values))

    # save the squared error to later assess k factor
    #k_tuning.append(float((exp_team1 - winner1)**2) + float((exp_team2 - (not winner1))**2))


# kfactor results of 2010-2020
#np.mean(k_tuning)
# 10 = 0.4847999117222912
# 20 = 0.4633329762656422
# 30 = 0.45797306263811216
# 35 = 0.45693282735040835
# 40 = 0.4565609863859344
# 45 = 0.4566970121366243
# 50 = 0.45721915090217674
# 75 = 0.4630154165519191
# 100 = 0.4711452135111493

# write out results
final_elo_ratings = game_data #.iloc[np.linspace(0 , 6582, 6582)]
final_elo_ratings['team1_elo'] = elo_rating_team1
final_elo_ratings['team2_elo'] = elo_rating_team2
final_elo_ratings.to_csv('Elo/Data/adjusted_historical_elo.csv')
