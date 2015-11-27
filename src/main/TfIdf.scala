package main


import ch.ethz.dal.tinyir.io.TipsterStream
import org.w3c.dom.Document
import javax.xml.parsers.DocumentBuilderFactory
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import ch.ethz.dal.tinyir.processing.TipsterParse
import ch.ethz.dal.tinyir.io.DocStream
import com.github.aztek.porterstemmer
import scala.collection.mutable.{Map => MutMap, MutableList}
import java.io.File
import scala.io.Source
import scala.math.log

object TfIdf {
  
  var termFreq : Map[String, Int] = null
  
  def run(iter : TipsterCorpusIterator, queryWords : MutableList[String]) {
    var dummyQuerry = MutableList[String]()
    dummyQuerry += "airbus"
    dummyQuerry += "subsidies"
    while(iter.hasNext){
	    	val doc = iter.next
	    	println(doc.name + " " + score(doc, dummyQuerry.toList))
	    }
    
  }
  
  def score(document: TipsterParse, query: List[String]) : Double = 
  {
    val words = document.content.split(" ").toList;
    val score = query.flatMap(q => logtf(words).get(q)).sum
    score
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