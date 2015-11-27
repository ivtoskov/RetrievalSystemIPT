package ch.ethz.dal.tinyir.alerts

import ch.ethz.dal.tinyir.util.StopWatch
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.Tokenizer

class AlertsReutersRCV(q: String, n: Int) extends Alerts(q,n) 

object AlertsReutersRCV {
  
  def main(args: Array[String]) {
  
    val query = "tourism africa"
    val num = 10
    val alerts = new AlertsReutersRCV(query,num)    
    val reuters = new ReutersRCVStream("/Users/thofmann/Data/ReutersRCV/zips")
    

    val stream = reuters.stream.take(1000) 
    val cat = "M13" // category code MONEY_MARKETS
    val tks = stream.filter(_.codes(cat)).flatMap(_.tokens)

    val distinct = stream.flatMap(_.tokens).distinct.length   
    println(distinct)
    val sum = tks.length.toDouble
    println(sum)
    
    val Pwc = tks.groupBy(identity).mapValues(l=>l.length)
    println(Pwc.toString)
    
    val sw = new StopWatch; sw.start
    
    for (doc <- reuters.stream.take(100))
      alerts.process(doc.date+": "+doc.title, Tokenizer.tokenize(doc.content))
    sw.stop
    println("Stopped time = " + sw.stopped)
    alerts.results.foreach(println)
    
  }
  
}