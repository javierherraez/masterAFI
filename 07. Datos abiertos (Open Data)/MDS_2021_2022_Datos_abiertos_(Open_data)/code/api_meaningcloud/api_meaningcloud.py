import sys
import meaningcloud
import json
import matplotlib.pyplot as plt

def download_lyrics(data, license_key):
    sentiments = {}
    responses = []

    try:
        for item in data:
            text = item['text']
            sentiment_response = meaningcloud.SentimentResponse(
                meaningcloud.SentimentRequest(license_key, lang='en', txt=text, txtf='plain').sendReq())

            if sentiment_response.isSuccessful():
                score = sentiment_response.getGlobalScoreTag()
                print('\t' + text + ': ' + str(score) + '\n')
                
                if (score in sentiments):
                    sentiments[score] = sentiments[score] + 1
                else:
                    sentiments[score] = 1
            else:
                if sentiment_response.getResponse() is None:
                    print('\nOh no! The request sent did not return a Json\n')
                else:
                    print('\nOh no! There was the following error: ' + sentiment_response.getStatusMsg() + '\n')
                    
            responses.append(sentiment_response.getResponse())
        return responses, sentiments
    except ValueError:
        e = sys.exc_info()[0]
        print('\nException: ' + str(e))
    

license_key = ''

with open('.\\api_musixmatch\\text.json', encoding = 'utf-8') as file:
    data = json.load(file)
    
responses, sentiments = download_lyrics(data = data, 
                                        license_key = license_key)

with open('.\\api_meaningcloud\\meaningcloud.json', 'w', encoding = 'utf-8') as file:
    json.dump(responses, file, ensure_ascii = False, indent = 4)

sent_colors = { 
    'P+': 'forestgreen', 
    'P': 'darkseagreen', 
    'NONE': 'gray', 
    'NEU': 'lightgray', 
    'N': 'tomato', 
    'N+': 'red' 
}   
labels = []
sents = []
colors = []
for label in sorted(sent_colors):
    if (label in sentiments):
        labels.append(label + ' (' + str(sentiments[label]) + ')')
        colors.append(sent_colors[label])
        sents.append(sentiments[label])
explode = [0] * len(labels)

plt.bar(labels, sents, color = colors)
plt.show()
        

