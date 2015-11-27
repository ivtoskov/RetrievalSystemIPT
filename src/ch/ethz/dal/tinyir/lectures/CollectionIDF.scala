
package ch.ethz.dal.tinyir.lectures

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.util.StopWatch
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import collection.mutable.{Map => MutMap}

object CollectionIDF {
  
  def bigPlusSmall(big: MutMap[String, Int], small: MutMap[String, Int]) =
    big ++= small.map{ case (t,v) => t -> (v + big.getOrElse(t,0)) }  
    
    
  def main (args: Array[String]) = {
    val tipster = new TipsterStream("/Users/thofmann/Data/Tipster/zips")
    val df = MutMap[String,Int]()
    val cf = MutMap[String,Int]()
    val sw = new StopWatch; sw.start 
    val runtime = Runtime.getRuntime
    import runtime.{ totalMemory, freeMemory, maxMemory }
    
    for (doc<- tipster.stream) {
      val tokens = doc.tokens 
      df ++= tokens.distinct.map(t => t-> (1+df.getOrElse(t,0)))
      cf ++= tokens.map(t => t-> (1+cf.getOrElse(t,0)))
  	}
    sw.stop
    println("Run time = " + sw.stopped)  
    df.filter{ case(k,v) => (v>=100)}.take(10).foreach(println)
    println("Size of df = " +df.size + ", counts in df " + df.values.sum)
    println("Size of cf = " +cf.size + ", counts in cf " + cf.values.sum)

    println(">=10000:\n" + df.filter{case (k,v) => v>=10000}.take(10).mkString(" "))
    println(">=1000:\n" + df.filter{case (k,v) => v>=1000}.take(10).mkString(" "))
    println(">=100:\n" + df.filter{case (k,v) => v>=100}.take(10).mkString(" "))
    println(">=10:\n" + df.filter{case (k,v) => v>=10}.take(10).mkString(" "))
    println(">=1:\n" + df.filter{case (k,v) => v>=1}.take(10).mkString(" "))

    println(">coll:\n" + df.filter{case (k,v) => v<=2000 && v>=1000 && cf.get(k).get<=500}.take(10).mkString(" "))
    println("<coll:\n" + df.filter{case (k,v) => v<=2000 && v>=1000 && cf.get(k).get>=4000}.take(10).mkString(" "))

  }
}
