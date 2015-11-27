package ch.ethz.dal.tinyir.shingling

import scala.util.Random

// class to compute minhashes over lists of strings
//   num: random permutations are used
//   card: range of hash function
//
class MinHash (num: Int, card: Int) {

  // create random permutations on construction
  val perms: List[Array[Int]] =
    List.fill(num)(List.range(0,card)).map(lst 	=> Random.shuffle(lst).toArray)    
  
   // compute min hashes for a list of strings or hashes
  def minhash (l: List[String]) : Array[Int] = mins(hash(l))
  def mins (l: List[Int]) : Array[Int] = remap(l).map(a => a.min)

  // hash all string elements in list 
  private def hash(lst: List[String]) : List[Int] = {
	lst.map(str => Math.abs(str.hashCode()) % card)
  }

  // multiple permutations of hash value list 
  private def remap(lst: List[Int]) : Array[List[Int]] = { 
    perms.map(perm => lst.map(perm(_))).toArray
  }  
}
