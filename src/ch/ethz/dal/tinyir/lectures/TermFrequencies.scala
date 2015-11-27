package ch.ethz.dal.tinyir.lectures

import Math.log10
object TermFrequencies {

  def tf(doc : List[String]) : Map[String, Int] = 
    doc.groupBy(identity).mapValues(l => l.length)
  
  def logtf(doc : List[String]) : Map[String, Double] = 
    logtf(tf(doc))
    
  def atf(doc : List[String]) : Map[String, Double] = { 
    val atf = tf(doc)
    atf.mapValues(f => 0.5 * f /atf.values.max.toDouble + 0.5)
  }
    
  def logtf(tf: Map[String,Int]) : Map[String, Double] = 
    tf.mapValues(v => log2(v.toDouble/tf.values.sum)+1.0)
  
    def idf(df: Map[String,Int], n: Int) : Map[String, Double] = 
    df.mapValues(log10(n) - log10(_))

  def log2 (x : Double) = log10(x)/log10(2.0)
    
  def main(args: Array[String]) = {
    val query : Set[String] = Set("green", "blue", "powder") 
    val doc : List[String] = List("green", "blue", "red", "green")
  
    
    
    val score = query.flatMap(logtf(doc) get).sum 
    println(score)
  }
}