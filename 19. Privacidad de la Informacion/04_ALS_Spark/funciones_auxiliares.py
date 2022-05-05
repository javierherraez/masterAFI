import sys
import itertools
from math import sqrt
from operator import add
from os.path import join, isfile, dirname
from pyspark.sql import Row

def parseRating(line):
    """
    Parses a rating record in MovieLens format userId::movieId::rating::timestamp .
    """
    fields = line.strip().split("::")
    return Row(userId=int(fields[0]), movieId=int(fields[1]), rating=float(fields[2]))

def parseMovie(line):
    """
    Parses a movie record in MovieLens format movieId::movieTitle .
    """
    fields = line.strip().split("::")
    return Row(movieId=int(fields[0]), movieTitle=fields[1])

def loadRatings(ratingsFile):
    """
    Load ratings from file.
    """
    if not isfile(ratingsFile):
        print("File %s does not exist." % ratingsFile)
    f = open(ratingsFile, 'r')
    ratings = filter(lambda r: r['rating'] > 0, [parseRating(line) for line in f])
    f.close()
    if not ratings:
        print("No ratings provided.")
    else:
        return ratings