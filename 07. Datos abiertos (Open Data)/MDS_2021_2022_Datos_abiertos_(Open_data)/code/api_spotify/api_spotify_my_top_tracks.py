import spotipy
from spotipy.oauth2 import SpotifyOAuth
import json

def download_user_top_tracks(limit, time_range, client_id, 
                        client_secret, redirect_uri):

    spc = SpotifyOAuth(client_id = client_id,
                       client_secret = client_secret,
                       redirect_uri = redirect_uri,
                       scope = 'user-top-read')

    sp = spotipy.Spotify(auth_manager = spc)
    
    results = sp.current_user_top_tracks(limit = limit, time_range = time_range)
    for idx, item in enumerate(results['items']):
        artist = item['artists'][0]
        album = item['album']
        print(idx, artist['name'], ' – ', album['name'], ' - ', item['name'])
        
    return results['items']


client_id = '942e93e82e774ef8a322eab3fb3c4243'
client_secret = '858f864ac8674d5f960c9b57e318bb6d'
redirect_uri = 'http://localhost:8080'

user_top_tracks = download_user_top_tracks(limit = 50, 
                                           time_range = 'long_term', 
                                           client_id = client_id, 
                                           client_secret = client_secret, 
                                           redirect_uri = redirect_uri)

with open('.\\code\\api_spotify\\spotify2.json', 'w', encoding = 'utf-8') as file:
    json.dump(user_top_tracks, file, ensure_ascii = False, indent = 4)
