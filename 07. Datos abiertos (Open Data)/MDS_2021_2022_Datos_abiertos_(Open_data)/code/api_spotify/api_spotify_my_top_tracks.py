import spotipy
from spotipy.oauth2 import SpotifyOAuth
import json

def download_user_top_tracks(limit, time_range, client_id, 
                        client_secret, redirect_uri):

    spc = SpotifyOAuth(client_id = client_id,
                       client_secret = client_secret,
                       redirect_uri = redirect_uri,
                       scope = 'user-top-read')

    sp = spotipy.Spotify(oauth_manager = spc)
    
    results = sp.current_user_top_tracks(limit = limit, time_range = time_range)
    for idx, item in enumerate(results['items']):
        artist = item['artists'][0]
        album = item['album']
        print(idx, artist['name'], ' – ', album['name'], ' - ', item['name'])
        
    return results['items']


client_id = '8ced8a41ea494b6a9aeec2ace0710ecf'
client_secret = '72b994c4bae143759a4d1401cfa4468f'


redirect_uri = 'http://localhost:8080'

user_top_tracks = download_user_top_tracks(limit = 100, 
                                           time_range = 'long_term', 
                                           client_id = client_id, 
                                           client_secret = client_secret, 
                                           redirect_uri = redirect_uri)

with open('.\\api_spotify\\spotify2.json', 'w', encoding = 'utf-8') as file:
    json.dump(user_top_tracks, file, ensure_ascii = False, indent = 4)
