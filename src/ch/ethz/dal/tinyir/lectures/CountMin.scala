package ch.ethz.dal.tinyir.lectures

import scala.util.Random
import math._

class CountMinRange(val depth: Int, val width: Int, val m: Int) {
  val logm = (log(m)/log(2)).floor.toInt-1
  val c = Array.ofDim[Long](logm,depth, width)
}

class CountMin(val depth: Int, val width: Int) {

  val c = Array.ofDim[Long](depth, width)
  
  def add(e: String, num: Int = 1) =
    for (d <- 0 until depth) c(d)(hash(e,d)) += num
 
  def addMin(e: String, num: Int) : Unit = for (i <- 0 until num) addMin(e)
  def addMin(e: String) = {
    val cm = countMin(e)      
    for (d <- 0 until depth) 
      if (c(d)(hash(e,d))==cm) c(d)(hash(e,d)) += 1
  }

  def counts(e: String) = {
      for (d <- 0 until depth) yield c(d)(hash(e,d))
    }.toArray

  def countMin(e: String) = counts(e).min 

  def bias(count: Long) : Double =
    (c(0).sum-count).toDouble / (width-1).toDouble 

  def countMean(e: String) : Double= {
    val corrected = counts(e).map( count  => count - bias(count) ).map(e => if (e>0) e else 0.0)      
    median(corrected.toArray)(chooseRandomPivot)
  }
    
    //c.zipWithIndex.map{ case (arr,d) => arr(hash(e,d)) }.min
 
  // hash function related 
  private val p = (BigInt(2).pow(31)-1).toLong
  private val random1 = Array.ofDim[Long](depth).map(_ => Random.nextLong % p)
  private val random2 = Array.ofDim[Long](depth).map(_ => Random.nextLong % p)
  private def hash(e: String, i: Int) = 
    (Math.abs((random1(i) * e.hashCode + random2(i)) % p) % width).toInt
    

  // median
  def median(arr: Array[Double])(implicit choosePivot: Array[Double] => Double) : Double = {
    def medianK(arr: Array[Double], k: Int)(implicit choosePivot: Array[Double] => Double): Double = {
      val a = choosePivot(arr)
      val (s, b) = arr partition (a >)
      if (s.size == k) a
      // The following test is used to avoid infinite repetition
      else if (s.isEmpty) {
        val (s, b) = arr partition (a ==)
        if (s.size > k) a
        else medianK(b, k - s.size)
      } else if (s.size < k) medianK(b, k - s.size)
      else medianK(s, k)
    }
    medianK(arr, (arr.size - 1) / 2)
  }

  def chooseRandomPivot(arr: Array[Double]): Double = arr(scala.util.Random.nextInt(arr.size))

}

object CountMin {

  def main(args: Array[String]) = {
    val CM = new CountMin(3,200)
    val elements = new Range(1,10000,1)
    val counts   = elements.map(a => (10000* Math.pow(a,-0.7)).toInt)

    for ( (e,c) <- elements.zip(counts) ) CM.addMin(e.toString,c)
    for ( (e,c) <- elements.zip(counts).take(100) ) 
      println(c + " ~= " + CM.countMin(e.toString) + " " + CM.countMean(e.toString).toInt)
   
    CM.c.foreach(a => println(a.sum.toString +": " +a.mkString(" ")))
  }

}  
