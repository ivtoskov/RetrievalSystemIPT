package ch.ethz.dal.tinyir.lectures
import collection.SortedMap
import util.Random

class RandomSurfer(val A: Map[Int,Array[Int]], val prob: Double){  
  
  def this (links: List[(Int,Int)], prob: Double) = 
    this(links.groupBy(_._1).mapValues(_.map(_._2).toArray), prob)
  
    val n = Math.max(A.keys.size, A.values.flatten.max)
  
  val jumpProb = Array.range(0,n).map(i => if (outDegree(i)==0) 1.0/n else (1-prob)/n)

  def iterate(num: Int) : Array[Double] =
    iterate(Array.fill[Double](n)(1.0/n),num)

  def iterate(s: Array[Double], num: Int) : Array[Double] =
    if (num>0) iterate(transition(s),num-1) else s
  
  def transition(s: Array[Double]) : Array[Double] = {
    
    // total probability from jumps 
    val pFromJumps : Double = jumpProb.zip(s).map{ case(a,b) => a*b }.reduce(_+_)
    
    // initialize with uniform jump prob
    val sNew = Array.fill[Double](n)(pFromJumps)  
    
    // iterate over source nodes 
    for ( (i,links) <- A) {
      // compute probability of transition over links
      val trProb = prob/links.length * s(i)
      // accumulate for each target node 
      links.foreach(node => sNew(node) += trProb)
    }
    sNew
  }
 
  
  def surf(state: Int) : Stream[Int] = {
    val next = nextState(state)     
    next #:: surf(next)
  }
  private def nextState(state: Int) = {
	  if ( Random.nextDouble >= prob ) randomNode 
	  else randomLink(A.getOrElse(state,Array.empty))
  }
  private def randomNode : Int = Random.nextInt(n)
  private def randomLink(links: Array[Int]) : Int = {
    val deg = links.length
    if (deg>0) links(Random.nextInt(deg))
    else randomNode
  }    
  private def outDegree(state: Int) = A.getOrElse(state,Array.empty).length
  
  
  def histogram(num: Int) = 
    surf(Random.nextInt(n)).take(num).groupBy(identity).mapValues(_.length)
    
}

object PageRank {
  
  val links = List((0,1),(1,1),(1,2),(2,0),(2,2),(2,3),(3,3),(3,4),(4,6),(5,5),(5,6),(6,3),(6,4),(6,6)) 
  val rs = new RandomSurfer(links,0.9)        
  
  def main(args: Array[String]) = {
    val steps = 1000000
    println(SortedMap(rs.histogram(steps).mapValues(v => v.toDouble/steps).toArray:_*))

    
    def truncate(s: String) = {
      val l = s.length
      s.substring(0,Math.min(5,l))
    }
    var s = rs.iterate(1)
    for (i <- 1 to 100) {
      println("Iter " + i + " = "+s.foldLeft("")((a,b) => a +" "+ truncate(b.toString)))
      s = rs.iterate(s,1)
    }
  }
	  
}