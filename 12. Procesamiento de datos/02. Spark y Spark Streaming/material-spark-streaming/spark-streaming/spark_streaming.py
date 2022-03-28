# Import findspark
import findspark

findspark.init()

# Import required packages
import sys
from pyspark import SparkContext, SparkConf
from pyspark.streaming import StreamingContext

if __name__ == "__main__":

    # Check if the parameters are correct
    if len(sys.argv) != 3:
        print("Usage: spark_streaming.py <hostname> <port>", file=sys.stderr)
        sys.exit(-1)

    # Create the SparkContext and StreamingContext
    conf = SparkConf()
    conf = conf.setAppName('mds-session')
    conf = conf.setMaster('local[*]')
    sc = SparkContext.getOrCreate(conf = conf)
    ssc = StreamingContext(sc, 10)

    # Quiet logs
    sc.setLogLevel("ERROR")

    # Retrieve lines from stream
    lines = ssc.socketTextStream(sys.argv[1], int(sys.argv[2]))

    # Retrieve the words in the stream and split by space
    words = lines.flatMap(lambda line: line.split(" "))

    # Transform the words into tuples (ready for aggregation)
    word_tuples = words.map(lambda word: (word, 1))

    # Aggregate by word to retrieve the final count in the current DStream
    counts = word_tuples.reduceByKey(lambda a, b: a+b)

    # Show the counts
    counts.pprint()
    
    # Start the streaming context
    ssc.start()

    # Await for program termination
    ssc.awaitTermination()
