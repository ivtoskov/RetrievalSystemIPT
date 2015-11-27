package ch.ethz.dal.tinyir.io

import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.XMLDocument

class ReutersRCVStream(path: String, ext: String = ".xml") 
extends ParsedXMLStream(new ZipDirStream(path, ".xml")){
  def stream : Stream[XMLDocument] = unparsed.stream.map(is => new ReutersRCVParse(is))
  def length = unparsed.length 
}

object ReutersRCVStream  {

  def main(args: Array[String]) {
    val reuters = new ReutersRCVStream ("/Users/thofmann/Data/ReutersRCV/zips")  
    println("Number of files in zips = " + reuters.length)
    
    var length : Long = 0 
    var tokens : Long = 0
    for (doc <- reuters.stream) { 
      length += doc.content.length          
      tokens += Tokenizer.tokenize(doc.content).length
    }
    println("Total number of characters = " + length)
    println("Total number of tokens     = " + tokens)
  }
}