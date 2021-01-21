import pandas as pd
import ssl
import string

ssl._create_default_https_context = ssl._create_unverified_context


def get_player_list(from_year):
    from_yr = from_year
    players_to_pull = []
    letters = list(map(chr, range(ord('a'), ord('z')+1)))
    for letter in letters:
        url = 'https://www.basketball-reference.com/players/{}/'.format(letter)
        df = pd.read_html(url)[0]
        df = df[df['To'] > from_yr]
        players_to_pull.append(df)
    players_to_pull = pd.concat(players_to_pull, axis = 0)
    players_to_pull = players_to_pull.reset_index(drop = True)
    return(players_to_pull)


player_list_df = get_player_list(1991)


import requests
from bs4 import BeautifulSoup

req = requests.get('https://www.basketball-reference.com/players/o/')
soup = BeautifulSoup(req.content, 'html.parser')
links = soup.find_all("a")
hrefs = []
for link in links:
    hrefs.append(link.get('href'))

player_page = [x for x in hrefs if 'players/o' in x]
