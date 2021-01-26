import time
start_time = time.time()


import pandas as pd
import ssl
import string
import requests
from bs4 import BeautifulSoup
import numpy as np
import os

ssl._create_default_https_context = ssl._create_unverified_context



from_year = 1991
# process duplicates used in later function
def rem_dup(l):
    seen = set()
    result = []
    for item in l:
        if item not in seen:
            seen.add(item)
            result.append(item)
    return(result)

def get_player_list(from_year):
    from_yr = from_year
    players_to_pull = []
    letters = list(map(chr, range(ord('a'), ord('z')+1)))
    for letter in letters:
        try:
            url = 'https://www.basketball-reference.com/players/{}/'.format(letter)
            link = 'https://www.basketball-reference.com/players/{}/'.format(letter)
            # get player links
            req = requests.get(link)
            soup = BeautifulSoup(req.content, 'html.parser')
            links = soup.find_all("a")
            hrefs = []
            for link in links:
                hrefs.append(link.get('href'))

            player_page = [x for x in hrefs if 'players/' in x]
            # trim list
            player_page = player_page[2:]
            player_page = player_page[:-27]
            # reove any duplicates
            player_page = rem_dup(player_page)

            # scrape and merge
            df = pd.read_html(url)[0]
            df['player_link'] = player_page
            df['player_link'] = 'https://www.basketball-reference.com' + df['player_link']
            df = df[df['To'] > from_year]
            players_to_pull.append(df)
        except:
            print(letter)

    players_to_pull = pd.concat(players_to_pull, axis = 0)
    players_to_pull = players_to_pull.reset_index(drop = True)
    return(players_to_pull)


player_list_df = get_player_list(from_year)




# next step

all_links = list(player_list_df['player_link'])

def get_logs(link):
    logs = pd.read_html(link)[0]
    logs['Season'] = logs['Season'].str.slice(0,2) + logs['Season'].str.slice(5)
    logs = logs[logs['Season'].str.len()==4.0]
    logs['Season'] = np.where(logs['Season'] == '1900', '2000', logs['Season'])
    # filter out years that do not fit specified range
    logs = logs[logs['Season'].astype(int) > from_year]
    game_logs = []
    seasons = list(logs['Season'])
    for season in seasons:
        try:
            log_url = link[:-5] + '/gamelog/' + season +'/'
            log_df = pd.read_html(log_url)[7]
            game_logs.append(log_df)
        except:
            print(link + 'did not play' + season)

    game_logs = pd.concat(game_logs, axis = 0)
    game_logs['Player_Key'] = link
    print(link + 'complete')
    return(game_logs)


if __name__ == "__main__":
    import multiprocessing
    pool = multiprocessing.Pool(os.cpu_count())
    results = pool.map(get_logs, all_links)
    results = pd.concat(results, axis = 0)
    results.to_csv('data_file.csv')
    print("--- %s seconds ---" % (time.time() - start_time))
