import json
from musixmatch import Musixmatch
import re

def process_lyric(lyric):
    return re.sub(
        '\\.\\.\\.\n\n\\*\\*\\*\\*\\*\\*\\* This Lyrics is NOT for Commercial use \\*\\*\\*\\*\\*\\*\\*\n.*',
        '',
        lyric)

def check_musixmatch_response(response):
    check = response['message']['header']['status_code'] == 200
    check = check and response['message']['body']['lyrics']['lyrics_body'] != ''
    return check

def download_lyrics(top_tracks, secret):
    musixmatch = Musixmatch(secret)
    
    songs = []
    texts = []
    
    for top_track in top_tracks:
        artist = str(top_track['artists'][0]['name'])
        track = str(top_track['name'])
        
        song = musixmatch.matcher_lyrics_get(track, artist)
        
        if (check_musixmatch_response(song)):
            text = {}
            text['text'] = song['message']['body']['lyrics']['lyrics_body']
            text['text'] = process_lyric(text['text'])
            
            song['message']['body']['lyrics']['lyrics_body'] = text['text']
            
            songs.append(song)
            texts.append(text)
            
    return songs, texts


secret = ''

with open('.\\api_spotify\\spotify.json', encoding = 'utf-8') as file:
    top_tracks = json.load(file)

songs, texts = download_lyrics(top_tracks = top_tracks, 
                               secret = secret)

with open('.\\api_musixmatch\\musixmatch.json', 'w', encoding = 'utf-8') as file:
    json.dump(songs, file, ensure_ascii = False, indent = 4)
    
with open('.\\api_musixmatch\\text.json', 'w', encoding = 'utf-8') as file:
    json.dump(texts, file, ensure_ascii = False, indent = 4)