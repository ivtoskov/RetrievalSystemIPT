package main


import ch.ethz.dal.tinyir.io.TipsterStream
import org.w3c.dom.Document
import javax.xml.parsers.DocumentBuilderFactory
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import ch.ethz.dal.tinyir.io.DocStream
import com.github.aztek.porterstemmer
import scala.collection.mutable.{Map => MutMap, MutableList}
import java.io.File
import scala.io.Source
import scala.math.log

class TfIdf(iter : TipsterCorpusIterator, queryWords : MutableList[String] ) {
  
  var termFreq : Map[String, Int] = null
  
  def run() {
    termFreq = tf(null)
    
  }
  
  def logtf(doc : List[String]): Map[String, Double] = logtf(tf(doc))
  
  def tf(doc : List[String]) : Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)
  
  def logtf(tf: Map[String,Int]) : Map[String, Double] =
  {  
    val sum = tf.values.sum.toDouble
    tf.mapValues( v => log( (v.toDouble+1.0) / sum ) )
  }
  
  def atf(tf : Map[String, Int]) = {
    val max = tf.values.max.toDouble
    tf.mapValues( f => 0.5 * f / max + 0.5 )
  }
  
  
  
  
}