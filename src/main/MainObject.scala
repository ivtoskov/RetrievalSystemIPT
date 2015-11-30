package main

import scala.collection.mutable.{Map => MutMap}

/**
 * The main class that starts the application.
 *
 * @author Stefan Irimescu
 */
object MainObject {
  def main(args: Array[String]) {
    test()
    return
    var linkArg : String = "/home/kennelcrash/MySpace/Studium/ETH/Semester1/InformationRetrieval/Project2/zips"
    if(args.length > 0)
      linkArg = args.apply(0)
    val manager = new AlertSystemManager(linkArg)
    manager.run()
  }

  def test(): Unit = {
    val df = MutMap[String, Int]()
    df("w1") = 1
    df("w2") = 2
    df("w3") = 3
    df("w4") = 4
    val words = Seq("w1", "w2", "w3").toList
    val query1 = Seq("w1", "w2", "w3").toList
    val query2 = Seq("w1", "w5").toList
    val query3 = Seq("w6", "w7").toList
    val query4 = Seq("w3", "w4").toList
    TfIdf.initCollectionStats(df)
    println(query1.flatMap(q => TfIdf.tfidf(words, q) ).sum)
    println(query2.flatMap(q => TfIdf.tfidf(words, q) ).sum)
    println(query3.flatMap(q => TfIdf.tfidf(words, q) ).sum)
    println(query4.flatMap(q => TfIdf.tfidf(words, q) ).sum)
  }
}