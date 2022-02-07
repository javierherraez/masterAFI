# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy


class ScraperImdbItem(scrapy.Item):
    # define the fields for your item here like:
    season = scrapy.Field()
    episode = scrapy.Field()
    title = scrapy.Field()
    rating = scrapy.Field()
    pass
