package ch.ethz.dal.tinyir.lectures

import collection.mutable.{Map => MutMap}
import ch.ethz.dal.tinyir.math.ProbVector

class TopicModel (vocabulary: Set[String], ntopics : Int) {

  // for each word (string) an array of topic probabilities p(w|t)
  // current and new values
  var Pwt    = MutMap[String,ProbVector]()

  // for initializing word topic probabilities on the fly 
  vocabulary.foreach(term => (Pwt += term -> ProbVector.random(ntopics).normalize))
      
  // compute topic distribution for a document  
  def topics(doc: Map[String,Int], num: Int = 20) : ProbVector = {
    var Ptd = ProbVector.random(ntopics)
    for (i <- 0 until num ) Ptd = iteration(Ptd, doc)
    Ptd
  }
    
  // one iteration of the generalized Csizar algorithm 
  private def iteration(Ptd : ProbVector, doc: Map[String,Int]) : ProbVector = {
    val newPtd = ProbVector(new Array[Double](ntopics)) 
    for ((w,f) <- doc) newPtd += (Pwt(w) * Ptd).normalize(f)
    newPtd.normalize
  }  
  
  // compute updates for word distributions from single document
  def update (doc: Map[String,Int], num: Int) : MutMap[String,ProbVector] = {
    val Ptd = topics(doc, num) // compute topics 
    val result = MutMap[String,ProbVector]()
    for ((w,f) <- doc) result += w -> (Pwt(w) * Ptd).normalize(f)
    result
  }
  
  // complete learning step 
  def learn (tfstream : Stream[Map[String,Int]]) = {
	val newPwt = MutMap[String,ProbVector]() 
    for ( doc <- tfstream ) {
      val result = update(doc,20)
      val increment = result.map{ 
        case (k,v) => k -> ( if (newPwt.contains(k)) v + newPwt(k) else v) 
      }
      increment.foreach{ case (k,a) => newPwt(k) = a }
    }
    //Pwt.clear; newPwt.foreach{ case (k,v) => Pwt += k->v }
	Pwt = newPwt
    val sums = Pwt.values.reduce((v1,v2) => v1 + v2)
    Pwt.foreach{ case (s,a) => Pwt.update(s,a/sums) } 
  }
}
  

object TopicModel {
 
  def main (args : Array[String]) : Unit = {
    
    val vocabulary = Set("a1","a2","a3","b1","b2","b3")
    val ntopics    = 2   
    val model      = new TopicModel(vocabulary,ntopics)
    val doc1       = Map("a1" -> 1, "a2" ->1, "a3" ->1)
    val doc2       = Map("b1" -> 1, "b2" ->1, "b3" ->1)
    val doc3       = Map("a1" -> 1, "b2" ->1, "a3" ->1, "b1" ->1)
    val stream     = Stream(doc1, doc2, doc3)
        
    for (i<- 0 until 50) model.learn(stream)
    
    model.Pwt.foreach{ case (w,a) => println(w + ": " + a.mkString(" ")) } 
    println("Topics for doc1 = " + model.topics(doc1).mkString(" ")) 
    println("Topics for doc2 = " + model.topics(doc2).mkString(" ")) 
    println("Topics for doc3 = " + model.topics(doc3).mkString(" ")) 
    
  }  
}
 