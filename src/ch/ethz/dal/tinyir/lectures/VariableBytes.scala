package ch.ethz.dal.tinyir.lectures
import scala.util.Random
import scala.math.abs
import scala.math.log

case class UByte (val x: Byte) extends AnyVal{
  def toInt =  if (x>=0) x.toInt else (256+x).toInt
  override def toString = this.toInt.toBinaryString.reverse.padTo(8,'0').reverse
}

object VariableBytes {

  def encodeVB(n: Long) : Array[Byte] = {
    def split(n: Long) : List[Byte] =
      if (n>=1) (n % 128).toByte :: split(n>>7) else Nil 
    ((-128 + n % 128).toByte :: split(n>>7)).reverse.toArray
  }
       
  def decodeVB(vb: Array[Byte]) : Long = 
    vb.foldLeft(0)((a,b) => (a<<7) + b) + 128
   
  def intToUnary(n: Integer):String = "1"*(n)+"0"
   
 def encodeGammaN(n: Int): String =
    encodeGamma(n.toBinaryString)

  
  def encodeGamma(bits: String): String = {
    val offset = bits.substring(1)
	intToUnary(offset.length)+offset
  }
  
  def decodeGamma(bits: String) : String = {
	  val len = bits.indexOf('0')
	  "1" + bits.substring(len+1,2*len+1)
  }
  def decodeGammaN(bits: String) : Long = 
    Integer.parseInt(decodeGamma(bits),2).toLong
  
  
  def main (args: Array[String]) = {
    
    val v = 1
    val ge = encodeGammaN(v)
    println(ge)
    println(ge.length() + " : " + log(v)/log(2))
    println(decodeGammaN(ge))
    
    /*
    val numbers = Seq.fill(100)(abs(Random.nextInt).toLong)
    var errors = 0
    for (m <- numbers) {    	
    	println(m)
    	val em = encodeVB(m)
    	println(em.mkString(" "))	
    	val bits = em.map(b => UByte(b).toString)
    	println(bits.mkString(" "))
    	val dm = decodeVB(em) 
    	println(dm+"\n")
    	if (m != dm) errors +=1
    }	
    println("Number of errors = " + errors)
    * 
    */
  }
}