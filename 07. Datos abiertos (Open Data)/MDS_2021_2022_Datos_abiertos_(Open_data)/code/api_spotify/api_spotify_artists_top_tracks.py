import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import json

def download_artists_top_tracks(artist_names, limit, client_id, client_secret):

    spc = SpotifyClientCredentials(client_id = client_id, 
                                   client_secret = client_secret)

    sp = spotipy.Spotify(auth_manager = spc)
    
    top_tracks = []

    for artist_name in artist_names:
        results = sp.search(q = artist_name, type = 'artist', limit = limit, market = 'es')
        
        if (len(results['artists']['items']) > 0):
            response = sp.artist_top_tracks(results['artists']['items'][0]['uri'])
    
            for idx, item in enumerate(response['tracks']):
                top_tracks.append(item)
                artist = item['artists'][0]
                album = item['album']
            
                print(idx, artist['name'], ' – ', album['name'], ' - ', item['name'])
                
    return top_tracks


client_id = '942e93e82e774ef8a322eab3fb3c4243'
client_secret = '858f864ac8674d5f960c9b57e318bb6d'

artist_names = ['the beatles', 'led zeppelin', 'the rolling stones']

artists_top_tracks = download_artists_top_tracks(artist_names = artist_names,
                                                 limit = 50, 
                                                 client_id = client_id, 
                                                 client_secret = client_secret)

with open('.\\code\\api_spotify\\spotify.json', 'w', encoding = 'utf-8') as file:
    json.dump(artists_top_tracks, file, ensure_ascii = False, indent = 4)