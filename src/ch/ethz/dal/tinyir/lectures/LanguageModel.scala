package ch.ethz.dal.tinyir.lectures
import collection.{SortedMap}

class Histogram (val map: SortedMap[Int,Int]) extends AnyVal {
  
  def counts(min : Int, max : Int) : Int = 
    map.rangeImpl(Some(min),Some(max)).values.sum
    
  def probability(min : Int, max : Int) : Double = 
    counts(min,max).toDouble/map.values.sum.toDouble
    
  def cumCounts(value: Int) : Int = 
    counts(0,value)
  
  def cumProbs(value: Int) : Double = 
    probability(0,value)
}

object Histogram {
  def main(args: Array[String]) {
    val map = SortedMap(1->3, 2->2, 3->5, 4->1, 6->1, 8->4)
    val hist = new Histogram(map)
    println(hist.map.mkString(", "))
    val rng = hist.counts(2,4)
    println(rng)
  }
  
}