import os
from pyprojroot import here

#os.chdir(here('Dropbox/nba-predictor/Players'))

from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd
import players
import utils
from datetime import datetime
from joblib import Parallel, delayed
import multiprocessing
import math

# load data and convert names to a list
names = pd.read_csv("br_names.txt", header=None)
names = list(names[0])

# specify start and end date
startdate = '1989-07-01'
enddate = '2021-01-11'

test = names[0:12]

def get_player_stat(name):
    some_player = players.get_game_logs(name, start_date = startdate, end_date = enddate, playoffs = False)
    if len(some_player) > 0:
        some_player['name'] = name
        return(some_player)

from multiprocessing.dummy import Pool

pool = Pool(12)
if __name__ == '__main__':
    results = pool.map(get_player_stat, test, 1)

results = pd.concat(results, axis = 0)
results.to_csv('../Tests/test1.csv')
