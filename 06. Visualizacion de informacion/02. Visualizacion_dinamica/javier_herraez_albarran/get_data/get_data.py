import sys
from time import sleep

import pandas as pd
import requests
from selenium import webdriver
from selenium.webdriver.common.by import By

SEASON = 2020

STATS_TO_SAVE = [
    'id',
    'game_id',
    'player_id',
    'player',
    'player_position',
    'team_name',
    'team_city',
    'points',
    'assists',
    'rebounds',
    'steals',
    'blocks',
    'personal_foul',
    'turnovers',
    'field_goals_made',
    'field_goals_attempted',
    'date'
]

URL = 'https://balldontlie.io/api/v1/stats'


def get_nba_stats_api(page: int, previous_error: bool):
    # print(page, previous_error)
    if (previous_error):
        sleep(1)
    params = {
        'seasons[]': SEASON,
        'page': page,
        'per_page': 100,
        'post_season': False
    }
    try:
        response = requests.get(URL, params)
    except:
        print("Error en la petición")
        sys.exit()
    # Too Many Requests
    if (response.status_code == 429):
        sleep(10)
        return get_nba_stats_api(page, True)
    if (response.status_code != 200):
        return None, True
    return response.json(), previous_error


def select_stats(stat):
    # hay observaciones sin jugador
    if stat['player']:
        return {
            'id': stat['id'],
            'game_id': stat['game']['id'],
            'player_id': stat['player']['id'],
            'player': stat['player']['first_name'] + ' ' + stat['player']['last_name'],
            'player_position': stat['player']['position'],
            'team_name': stat['team']['name'],
            'team_city': stat['team']['city'],
            'points': stat['pts'],
            'assists': stat['ast'],
            'rebounds': stat['reb'],
            'steals': stat['stl'],
            'blocks': stat['blk'],
            'personal_foul': stat['pf'],
            'turnovers': stat['turnover'],
            'field_goals_made': stat['fgm'],
            'field_goals_attempted': stat['fga'],
            'date': stat['game']['date']
        }


def get_nba_stats():
    df_nba = pd.DataFrame(columns=STATS_TO_SAVE)
    page = 1
    previous_error = False
    while page is not None:
        response, previous_error = get_nba_stats_api(page, previous_error)
        if response == None:
            break
        for stat in response['data']:
            current_stat = select_stats(stat)
            df_nba = df_nba.append(current_stat, ignore_index=True)
        page = response['meta']['next_page']
    return df_nba


def get_nba_salaries():
    browser = webdriver.Chrome('.\\get_data\\driver\\chromedriver.exe')
    browser.set_window_position(0, 0)
    browser.set_window_size(1920, 1080)
    browser.get('https://hoopshype.com/salaries/' +
                str(SEASON) + '-' + str(SEASON + 1) + '/')

    players_button = browser.find_element(
        By.CSS_SELECTOR, 'ul.tab-controls li:not(.ui-tabs-active) a')
    players_button.click()

    players = []
    salaries = []
    for player in browser.find_elements(By.CSS_SELECTOR, 'table.hh-salaries-ranking-table tbody tr'):
        players.append(player.find_element(By.CSS_SELECTOR, 'td.name').text)
        salaries.append(player.find_element(By.CSS_SELECTOR,
                                            'td.hh-salaries-sorted').get_attribute('data-value'))
    browser.close()
    return pd.DataFrame({'player': players, 'salary': salaries})


def clean_names(names: pd.DataFrame):
    return names.replace(r'[^\w|\s]', r'', regex=True)


df_nba_stats = get_nba_stats()
df_nba_stats = df_nba_stats.drop_duplicates()
df_nba_stats['player'] = clean_names(df_nba_stats['player'])

df_nba_salaries = get_nba_salaries()
df_nba_salaries['player'] = clean_names(df_nba_salaries['player'])

df_nba = df_nba_stats.merge(df_nba_salaries, how='left')

df_nba.to_csv("data/nba" + str(SEASON) + ".csv", index=False)
df_nba.to_csv("visualitation/nba" + str(SEASON) + ".csv", index=False)
