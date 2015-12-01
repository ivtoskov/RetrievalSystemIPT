package main

import scala.collection.mutable.{Map => MutMap}
import scala.math.log10

object TfIdf {
  var termFreq : Map[String, Int] = null
  var idf = Map[String, Double]()
  var numberOfDocuments: Long = 0L

  def initCollectionStats(df: MutMap[String, Int], numOfDocuments: Long): Unit = {
    numberOfDocuments = numOfDocuments
    idf = idf(df.toMap, numberOfDocuments)
  }
  
  def score(words: Map[String, Double], query: List[String]) : Double = {
    query.flatMap(q => tfidf(words, q) ).sum
  }
  
  def tfidf(words : Map[String, Double], q: String) : Option[Double] = {
    val containedInDocument = words.get(q)
    if(containedInDocument.isEmpty) {
      return None
    }
    val containedInCollection = idf.get(q)

    Option(containedInDocument.get * containedInCollection.get)
  }

  def logtf(doc : List[String]): Map[String, Double] = logtf(tf(doc))
  
  def tf(doc : List[String]) : Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)
  
  def logtf(tf: Map[String,Int]) : Map[String, Double] = {
    val sum = tf.values.sum.toDouble
    tf.mapValues( v => log2( 1.0 + v.toDouble / sum ) )
  }
  
  def atf(tf : Map[String, Int]) = {
    val max = tf.values.max.toDouble
    tf.mapValues( f => 0.5 * f / max + 0.5 )
  }
  
  def log2 (x: Double) : Double = log10(x) / log10(2.0)
  def idf(df: Map[String, Int], n: Long) : Map[String, Double] = df.mapValues(log2(n) - log2(_))
}