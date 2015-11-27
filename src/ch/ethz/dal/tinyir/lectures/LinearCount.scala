package ch.ethz.dal.tinyir.lectures

class LinearCounter (val num: Int) {
  val mask = Array.ofDim[Boolean](num)
  def +=(e: String) = mask(hash(e))=true
  def weight = mask.filter(a => a).size
  def cardinality =  num * Math.log(num.toDouble/(num-weight))
  private def hash(e: String) = Math.abs(e.hashCode) % num
  override def toString = mask.map(a => if (a) "1" else "0").reduce((a,b) => a + " " +b)
}


object LinearCount {

  def main(args: Array[String]) {
    val m = 10000000
    val s = collection.mutable.Set[Long]()
    val lc = new LinearCounter(m/2)
    
    for (i <- 0 until m) {
      s  += util.Random.nextLong
      lc += util.Random.nextLong.toString
    }
    
    println("True cardinality = " + s.size)
    println("Bits set = " + lc.weight)
    println("Cardinality estimate = " + lc.cardinality)
  }
}