import scrapy
import re

class SpiderSpider(scrapy.Spider):
    name = 'imdb'
    allowed_domains = ['www.imdb.com']
    start_urls = ['https://www.imdb.com/title/tt0944947/episodes?season=1']
    
    def parse(self, response):
        season = re.findall(r'\d+', response.css('h3#episode_top::text').get())[0]
        for episode in response.css('div.list_item'):
            yield {
                'season': season,
                'episode': episode.css('div.info [itemprop=\'episodeNumber\']::attr(content)').get(),
                'title': episode.css('div.info strong a::text').get(),
                'rating': episode.css('div.info div.ipl-rating-widget div.ipl-rating-star.small span.ipl-rating-star__rating::text').get()
            }
        next_page = response.css('a#load_next_episodes::attr(href)').get()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse)
