
import ch.ethz.dal.tinyir.io.TipsterStream
import org.w3c.dom.Document
import javax.xml.parsers.DocumentBuilderFactory
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import ch.ethz.dal.tinyir.io.DocStream
import com.github.aztek.porterstemmer
import scala.collection.mutable.{Map => MutMap, MutableList}
import java.io.File
import scala.io.Source
import main.TfIdf


class AlertSystemManager (val resourcePath: String) {
  
  var queryWords : MutableList[String] = null
  
  def run() {
     println("Initializing, reading zip files")
     val iter = new TipsterCorpusIterator(resourcePath)
     println("Tipster Corpus iterator  initialized")
	   LoadQueryWords()
	   TfIdf.run(iter, queryWords)
  }
 
  def LoadQueryWords()
  {
    queryWords = MutableList[String]()
    
    /* CODE for DOM parsing, not working since topics file is not a valid XML file
    val dbFactory = DocumentBuilderFactory.newInstance
    val dBuilder = dbFactory.newDocumentBuilder
    val doc = dBuilder.parse("Resources/topics")
    val topics = doc.getElementsByTagName("title")
    for (i <- 0 until topics.getLength)
    {
      queryWords += topics.item(i).getTextContent
    }
    
    queryWords.foreach {s => println(s) }
    */
    
    for (line <- Source.fromFile("Resources/topics").getLines)
      if(line.startsWith("<title>"))
          line.replace("<title>", "").replace("Topic", "")
          .replace(":","").replace(";","").replace("&","").replace("-","").trim()
          .split(" ").foreach { word => if(!word.isEmpty()) queryWords += word.toLowerCase() }
    
    
    //queryWords.foreach {s => println(s) }
  }
  
  
}