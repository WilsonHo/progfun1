import org.apache.spark.SparkConf
import org.apache.spark.storage.StorageLevel
import org.apache.spark.streaming.{Seconds, StreamingContext}
import org.apache.log4j.Logger
import org.apache.log4j.Level

/**
  * Created by baohg on 13/09/2016.
  */

object NetworkWordCount {
  Logger.getLogger("org").setLevel(Level.OFF)
  Logger.getLogger("akka").setLevel(Level.OFF)

  def main(args: Array[String]) {
    val appName = "NetworkWordCount"
    else new SparkConf().setAppName(appName)
    val (host, port, sparkConf) =
      if (args.length < 2) ("localhost", 9999, new SparkConf().setAppName(appName).setMaster("local[2]"))
      else (args(0), args(1).toInt, new SparkConf().setAppName(appName))

    //      System.exit(1)
    // Create the context with a 1 second batch size
    val ssc = new StreamingContext(sparkConf, Seconds(10))

    // Create a socket stream on target ip:port and count the
    // words in input stream of \n delimited text (eg. generated by 'nc')
    // Note that no duplication in storage level only for running locally.
    // Replication necessary in distributed scenario for fault tolerance.
    val lines = ssc.socketTextStream(host, port, StorageLevel.MEMORY_AND_DISK_SER)
    val words = lines.flatMap(_.split(" "))
    val wordCounts = words.map(x => (x, 1)).reduceByKey(_ + _)
    wordCounts.print()
    ssc.start()
    ssc.awaitTermination()
  }
}