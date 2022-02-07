import re
from datetime import datetime

import numpy as np
import pandas as pd
import requests
from bs4 import BeautifulSoup


def process_timestamp(element_str):
    timestamp_format = '%d/%m/%Y %H:%M'
    if (pd.isnull(element_str)):
        element = np.nan
    elif (re.search('^[0-9]{1,2}/[0-9]{1,2}$', element_str) != None):
        element_str = element_str + '/' + datetime.now().strftime('%Y') + ' 23:59'
        element = datetime.strptime(element_str, timestamp_format)
    elif (re.search('^[0-9]{1,2}:[0-9]{1,2}$', element_str) != None):
        element_str = datetime.now().strftime('%d/%m/%Y') + ' ' + element_str
        element = datetime.strptime(element_str, timestamp_format)
    else:
        element = datetime.now()

    return element


def scrape_eurostoxx_data(verbose):
    url_expansion = 'http://www.expansion.com/mercados/cotizaciones/indices/eurostoxx_I.5E.html'
    eurostoxx_table_columns = ['stock', 'last', 'var_perc', 'var', 'ytd', 'max', 'min', 'volume',
                               'capitalization', 'timestamp']

    expansion_request = requests.get(url_expansion)

    if expansion_request.status_code != 200:
        return pd.DataFrame([], columns=eurostoxx_table_columns)

    expansion_web = BeautifulSoup(expansion_request.text, 'html.parser')

    if (verbose):
        print(expansion_web.prettify())

    table = expansion_web.find('table', {'id': 'listado_valores'})

    if (verbose):
        print(table.prettify())

    eurostoxx_table = pd.read_html(str(table), 
                                   header = 0, 
                                   encoding = 'utf-8', 
                                   decimal = ',',
                                   thousands = '.')[0]

    if (verbose):
        print(eurostoxx_table.head())

    # Last column is empty so remove it
    eurostoxx_table = eurostoxx_table.iloc[:, :-1]

    # Set new column names
    eurostoxx_table.columns = eurostoxx_table_columns

    if (verbose):
        print(eurostoxx_table.dtypes)

    eurostoxx_table[['var_perc', 'ytd']] = eurostoxx_table[['var_perc', 'ytd']].div(100)

    eurostoxx_table['timestamp'] = eurostoxx_table['timestamp'].apply(process_timestamp)

    if (verbose):
        print(eurostoxx_table.dtypes)

    return(eurostoxx_table)


eurostoxx_data = scrape_eurostoxx_data(True)

eurostoxx_data.to_csv('.\\beautifulsoup_eurostoxx\\eurostoxx.csv', index = False)
eurostoxx_data.to_json('.\\beautifulsoup_eurostoxx\\eurostoxx.json', orient='records', indent = 4, force_ascii = False)
