package ch.ethz.dal.tinyir.lectures


import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.util.StopWatch
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import collection.mutable.{Map => MutMap}


object TextCategorization {

  def main (args: Array[String]) = {
    val reuters  = new ReutersRCVStream("/Users/thofmann/Data/Reuters_RCV_1and2/zips")
 
    val tag = "M13"
    val tokens1 = reuters.stream.take(1000).filter(_.codes(tag)).flatMap(d => d.tokens)
    val tokens0 = reuters.stream.take(1000).filter(!_.codes(tag)).flatMap(d => d.tokens)
    val Pwc1 = tokens1.groupBy(identity).mapValues(l => l.length+1)
    val Pwc0 = tokens1.groupBy(identity).mapValues(l => l.length+1)
    val sum1 = Pwc1.values.sum
    val sum0 = Pwc0.values.sum
    
    
    println(" Some tokens for " + tag + " = " + 
        Pwc1.filter{ case (k,v) => (v>=0.001*sum1)}.take(10).mkString(":")) 
        
    println(" Number of docs for " + tag + " = " + 
        reuters.stream.take(1000).filter(_.codes(tag)).length)    
  }
}