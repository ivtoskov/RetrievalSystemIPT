package ch.ethz.dal.tinyir.lectures 

/*
import collection.immutable.Set
import collection.mutable.BitSet
import collection.mutable.HashSet
import scala.util.Random

import java.lang.Integer

class BloomFilter[T](numBits : Int, numHash : Int) {  
  require(numBits > 0)
  require(numHash > 0)

  val bitSet = new BitSet(numBits)

  def add[T](S: Set[T]) : Unit = S.foreach(e => add(e))
  def add[T](el: T) : Unit = bitSet ++= bits(el)
  def contains(el: T) : Boolean = (bits(el) &~ bitSet).isEmpty
  def numSetBits = bitSet.size
  def fpProb = math.pow(numSetBits.toFloat/numBits.toFloat, numHash)
  def fpProbExpected (num : Int) = math.pow(1.0-math.exp(-numHash * num / numBits.toFloat), numHash)

  
  // --> BLACK BOX  
  //

  // simple heuristics to create multiple hash functions
  private def bits[T](el: T) : BitSet = { 
    val res = new BitSet(numBits)
    var seed = el
    for (i <- 0 until numHash) {
     // seed = bbXorRandom(seed)
     // res += clip(seed)  
    }
    res
  }    
  
  // clip index to valid range
  //private def clip[T](i : T) = math.abs(i.hash % numBits)
  
  // simple xor based pseudo-randomization
  private def bbXorRandom(seed : Int) : Int = { 
    var y = seed
    y ^= y << 13
    y ^= y >> 17
    y ^ y << 5
  }
  
  // --> DEBUGGING AND TESTING  
  //
  def printInfo = {
    println("class BloomFilter")
    println("  number of bits     = " + numBits)
    println("  number of hashes   = " + numHash)
    println("  number of bits set = " + bitSet.size)
  }
}


object BloomFilter {
  

   def main(args: Array[String]) {
  	  println("Test for " + this.getClass().toString())  	  

  	  val n : Int = 10000
  	  val S = new HashSet[Int].empty;
      for (i <-0 until n) S.add(Random.nextInt)
      
  	  val n2 : Int = 10000
  	  val S2 = new HashSet[Int].empty;
      for (i <-0 until n2) S2.add(Random.nextInt)

      val numBits = 8*n
      val filter = new BloomFilter(numBits,4)    	
      
      println("Initializing filter")      
      filter.add(S.toSet)
      println("... number of elements: " + S.size)
      println("... number of bits:     " + numBits)  	  
      println("... number of set bits: " + filter.numSetBits)  	  

      var count = 0
      for (s <- S)
//        if (filter.contains(s)) count = count + 1
      //println("Number of elements that pass filter: " + count + " of " + n)
      
      var count2 = 0
      for (s2 <- S2) {
        if (filter.contains(s2) & !S.contains(s2)) count2 = count2 + 1
       }
       println("Percentage of incorrect elements that pass: " 
          + count2.toFloat/n2.toFloat*100.0 + " %")
       println("False pos probability (q correct)   = " + filter.fpProb*100 + " %")
       println("False pos probability (q estimated) = " + filter.fpProbExpected(S.size)*100 + " %")
  	   	  
  }
}

*/