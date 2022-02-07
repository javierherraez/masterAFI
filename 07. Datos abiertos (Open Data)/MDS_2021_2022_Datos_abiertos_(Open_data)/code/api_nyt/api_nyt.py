import requests
import json

def get_nyt_most_popular_url(service, time_period):
    return 'https://api.nytimes.com/svc/mostpopular/v2/' + service + '/' + time_period + '.json'

def download_nyt_most_popular(service, time_period, api_key):
    nyt_most_popular_url = get_nyt_most_popular_url(service, time_period)
    payload = {'api-key': api_key}
    response = requests.get(nyt_most_popular_url, payload)
    
    if (response.status_code != 200):
        return None
    
    return response.json()


api_key_nyt_most_popular = 'rR90UIBj5BRmrgg7llsnuxEDZcvc4OTA'
time_period = '30'

service = 'viewed'
nyt_most_viewed = download_nyt_most_popular(service, time_period, api_key_nyt_most_popular)

service = 'shared'
nyt_most_shared = download_nyt_most_popular(service, time_period, api_key_nyt_most_popular)

service = 'emailed'
nyt_most_mailed = download_nyt_most_popular(service, time_period, api_key_nyt_most_popular)

nyt_most_popular = nyt_most_viewed['results'] + nyt_most_shared['results'] + nyt_most_mailed['results']

with open('.\\code\\api_nyt\\news.json', 'w', encoding = 'utf-8') as file:
    json.dump(nyt_most_popular, file, ensure_ascii = False, indent = 4)