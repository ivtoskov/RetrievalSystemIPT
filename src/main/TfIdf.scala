package main

import ch.ethz.dal.tinyir.processing.TipsterParse
import scala.collection.mutable.{Map => MutMap}
import scala.math.log10

object TfIdf {
  var termFreq : Map[String, Int] = null
  var cf = MutMap[String, Int]()
  var df = MutMap[String, Int]()

  def initCollectionStats(cfe: MutMap[String, Int], dfe: MutMap[String, Int]): Unit = {
    cf = cfe
    df = dfe
  }
  
  def score(document: TipsterParse, query: List[String]) : Double = {
    val words = document.content.split(" ").toList
    val score = query.flatMap(q =>  tfidf(words,q) ).sum
    score
  }
  
  def tfidf(words : List[String], q: String) : Option[Double] = {
    Option(logtf(words).get(q).get *idf(df.toMap, df.size).get(q).get)
  }

  def logtf(doc : List[String]): Map[String, Double] = logtf(tf(doc))
  
  def tf(doc : List[String]) : Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)
  
  def logtf(tf: Map[String,Int]) : Map[String, Double] = {
    val sum = tf.values.sum.toDouble
    tf.mapValues( v => log2( (v.toDouble+1.0) / sum ) )
  }
  
  def atf(tf : Map[String, Int]) = {
    val max = tf.values.max.toDouble
    tf.mapValues( f => 0.5 * f / max + 0.5 )
  }
  
  def log2 (x: Double) : Double = log10(x) / log10(2.0)
  def idf(df: Map[String, Int], n: Int) : Map[String, Double] = df.mapValues(log2(n) - log2(_))
}