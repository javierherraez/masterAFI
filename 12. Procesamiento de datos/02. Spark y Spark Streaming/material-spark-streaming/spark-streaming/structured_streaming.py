# Import findspark
import findspark

findspark.init()

# Import required packages
import sys
from pyspark import SparkConf
from pyspark.sql import SparkSession
from pyspark.sql.functions import explode
from pyspark.sql.functions import split

if __name__ == "__main__":

    # Check if the parameters are correct
    if len(sys.argv) != 3:
        print("Usage: structured_streaming.py <hostname> <port>", file=sys.stderr)
        sys.exit(-1)

    # Create the Spark Session
    conf = SparkConf()
    conf = conf.setAppName('mds-session')
    conf = conf.setMaster('local[*]')
    spark = SparkSession.builder.config(conf=conf).getOrCreate()

    # Quiet logs
    spark.sparkContext.setLogLevel("ERROR")

    # Create DataFrame representing the stream of input lines from connection to localhost:9999
    lines = spark.readStream.format("socket").option("host", sys.argv[1]).option("port", int(sys.argv[2])).load()

    # Convert the words received into a DataFrame column
    words = lines.select(
       explode(
           split(lines.value, " ")
       ).alias("word")
    )

    # Generate running word count
    wordCounts = words.groupBy("word").count()

    # Start running the query that prints the running counts to the console
    query = wordCounts.writeStream.outputMode("complete").format("console").start()

    # Await for program termination
    query.awaitTermination()
