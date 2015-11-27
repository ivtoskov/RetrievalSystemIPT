
package ch.ethz.dal.tinyir.lectures

object Tokenization { 
  def lowercase (tokens: List[String]) = tokens.map(_.toLowerCase)

  def main(args: Array[String]) = {
    val text = "USA: Sharp rise seen in CD bootlegs, what now?"
    println(text)	
      
    val tokens = text.split("[ .,;:?!\t\n\r\f]+").toList
    println(lowercase(tokens))
  }
}
  