from scrapy.linkextractors import LinkExtractor
from scrapy.spiders import CrawlSpider, Rule
from scraper_20minutos.items import Scraper20MinutosItem

class Scraper20MinutosSpider(CrawlSpider):
    name = '20minutos'
    allowed_domains = ['www.20minutos.es']
    start_urls = ['https://www.20minutos.es/actualidad/']
    
    custom_settings = {
        'CONCURRENT_REQUESTS': 1,
        'CONCURRENT_ITEMS' :1,
        'CLOSESPIDER_ITEMCOUNT': 100
    }

    rules = (
        Rule(
            LinkExtractor(
                allow = (), 
                restrict_css = ('li.pagination-item a')
            )
        ),
        Rule(
            LinkExtractor(
                allow = (), 
                restrict_css = ('article.media div.media-content a')
            ), 
            callback = 'parse_item',
            follow = False
        ),
    )

    def parse_item(self, response):
        item = Scraper20MinutosItem()
         
        item['url'] = response.css('link[rel=''canonical'']::attr(href)').get()
        item['title'] = response.css('div.title h1.article-title::text').get()
        item['author'] = response.css('span.article-author strong::text').get()
        item['date'] = response.css('span.article-date a::text').get()
        item['content'] = '\n'.join(response.css('div.article-text *::text').getall())
        
        return item