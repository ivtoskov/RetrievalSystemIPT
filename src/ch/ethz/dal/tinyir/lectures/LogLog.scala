package ch.ethz.dal.tinyir.lectures
import math._

class LogLog {

  var z : Int = 0
  def reset = z=0
  
  def +=(e: Long) = z=max(z,zeros(e.hashCode))
  def card = pow(2,z+0.5)
  
  private def zeros(i: Int) : Int = if (i>0 && i%2==0) 1+zeros(i/2) else 0 
  
}

object LogLog {

  def main(args: Array[String]) = {
    val m  = 1000000
    val s  = collection.mutable.Set[Long]()
    val ll = new LogLog
    for (i <- 0 until m) {
      val e = util.Random.nextLong
      s  += e 
      ll += e 
    }    
    println(s.size)
    println(ll.z + " : " + Math.pow(2,ll.z+0.5))
  }
}