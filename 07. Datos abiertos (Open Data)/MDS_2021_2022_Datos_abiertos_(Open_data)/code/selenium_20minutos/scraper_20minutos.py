import time

import numpy as np
import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select, WebDriverWait

def process_string(element):
    return ' '.join(element.strip().replace('\n', ' ').split())

browser = webdriver.Chrome('.\\code\\selenium_20minutos\\driver\\chromedriver.exe')
browser.set_window_position(0, 0)
browser.set_window_size(1920, 1080)
browser.get('https://www.20minutos.es/')

cookies_button = browser.find_element_by_css_selector('button#didomi-notice-agree-button')
cookies_button.click()

search_button = browser.find_element_by_css_selector('div#ui-header-land li.search')
search_button.click()

WebDriverWait(browser, 20).until(EC.element_to_be_clickable((By.CSS_SELECTOR, 'div#ui-header-land a.show-advance'))).click()

time.sleep(1)

search_text = browser.find_element_by_css_selector('div.header-primary-wrapper input[placeholder=\'¿Qué estás buscando?\']')
search_text.click()
search_text.clear()
search_text.send_keys('Volcan la palma')

sort_by = Select(browser.find_element_by_css_selector('select#busqueda-fecha'))
sort_by.select_by_visible_text('Fecha')

article_buttons = browser.find_elements_by_css_selector('label.input-check span.text-check')

for article_button in article_buttons:
    if (article_button.text == 'Artículo'):
        article_button.click()
        
submit = browser.find_element_by_css_selector('.submit-action')
submit.click()

urls = []
titles = []
authors = []
abstracts = []

num_pages_to_download = 5

for x in range(1, num_pages_to_download + 1):

    try:
        next_page = browser.find_element_by_css_selector('div.pagination ul li.active + li a').get_attribute('href')
    except NoSuchElementException: 
        next_page = None

    articles = browser.find_element_by_css_selector('section.content')
    print(articles)
    print("------")

    articles = articles.get_attribute('outerHTML')
    print(articles)


    articles = BeautifulSoup(articles, 'html.parser')
    
    for article in articles.find_all('article', {'class' : 'media'}):
        urls.append(article.select('div.media-content header h1 a')[0].attrs['href'])
        titles.append(process_string(article.select('div.media-content header h1 a')[0].text))
        authors.append(process_string(article.select('div.media-content div.author')[0].text))
        abstract = article.find('div', {'class' : 'media-intro'})
        if (abstract):
            abstracts.append(process_string(abstract.text))
        else:
            abstracts.append(np.nan)

    if (next_page and x < num_pages_to_download):
        browser.get(next_page)
    else:
        break
    
news_data = pd.DataFrame({'url': urls, 'title': titles, 'author': authors, 'abstract': abstracts })

news_data.to_csv('.\\code\\selenium_20minutos\\news.csv', index = False)
news_data.to_json('.\\code\\selenium_20minutos\\news.json', orient = 'records', indent = 4, force_ascii = False)

print("Fin")

browser.close()

print("Fin2")
