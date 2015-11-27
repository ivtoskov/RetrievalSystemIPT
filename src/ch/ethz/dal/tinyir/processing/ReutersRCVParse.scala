

package ch.ethz.dal.tinyir.processing

import util.Try
import util.Success
import javax.xml.parsers._
import org.w3c.dom.{Document => XMLDoc}
import java.io.InputStream
import ch.ethz.dal.tinyir.io.DocStream


class ReutersRCVParse(is: InputStream) extends XMLDocument(is) { 
  override def name = getAttrFromFirstNode("itemid", "newsitem").getOrElse("") 
  override def date = getAttrFromFirstNode("date","newsitem").getOrElse("")   
  override def codes = getAttrFromAllNodes("code","code").toSet 
}

object ReutersRCVParse {
  def main(args: Array[String]) {
    val dirname = "/Users/thofmann/Data/ReutersRCV/zips/19960821"
    val fname = dirname + "/4949newsML.xml"
    val parse = new ReutersRCVParse(DocStream.getStream(fname))
    val title = parse.title
    println(title)
    println(Tokenizer.tokenize(title))
    println("DocID = " + parse.ID)
    println("Date  = " + parse.date)
    println("Codes = "+ parse.codes.mkString(" "))
    
  }
}