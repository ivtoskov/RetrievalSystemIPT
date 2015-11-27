package ch.ethz.dal.tinyir.shingling

import scala.collection.Map
import scala.collection.mutable.HashMap

object Shingler {

  // map from n-grams to frequencies
  def shingles(doc: String, n: Int) : Map[String,Int] = {    
    if ( n<1 || doc.length<n) Map()
    else count(ngrams(doc,n))
  }

  // n-gram fingerprints with frequencies
  def fingelsFreq(doc: String, n: Int, mod: Int) : Map[Int,Int] = {    
    if ( n<1 || doc.length<n) Map()
    else count(nhashes(doc,n,mod))
  }

  // n-gram fingerprints w/o frequencies
  def fingels(doc: String, n: Int, mod: Int) : List[Int] = {    
    if ( n<1 || doc.length<n) List()
    else uniques(nhashes(doc,n,mod))
  }
  

  // n-grams in a string
  private def ngrams(doc: String, n: Int) =  doc.sliding(n)
  // hashed n-grams 
  private def nhashes(doc: String, n: Int, mod: Int) = {
    ngrams(doc,n).map(s => Math.abs(s.hashCode()) % mod) 
  }

  // helper functions
  private def count[T](it: Iterator[T]) : Map[T,Int] = {
    it.toList.groupBy(identity).mapValues(p => p.length)
  } 
  private def uniques[T](it: Iterator[T]) : List[T] = {
    it.toList.distinct
  } 

  // k-spectrum of a string for k>=2
 def spectrum(doc: String, k: Int) = {
   require(k>=2)
   val result = new HashMap[String,Int]()
   for(n <- 2 until k+1) result ++= shingles(doc,n)
   Map() ++ result 
 }
 

  def main(args: Array[String]) {
    val doc = "a rose is a rose is a rose"
    val shingles = Shingler.shingles(doc, 5)
    val fingles =  Shingler.fingels(doc, 5, 256)
    println(shingles.mkString("[","], [","]"))
    println(fingles.mkString("[",", ","]"))
    
  }
}
