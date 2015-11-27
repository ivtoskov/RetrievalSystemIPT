package ch.ethz.dal.tinyir.processing

import java.io.File
import java.io.FileNotFoundException
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.collection.mutable.Queue
import scala.util.Success
import scala.util.Try


/** Simple iterator over a set of XMLs zipped into several ZIP files
 * 
 */
class TipsterCorpusIterator(path : String) extends Iterator[TipsterParse] {  
  // Queue of Zip files located at path
  private val zips = {
    val dirFile = new File(path)
  	if(dirFile == null)
  		throw new FileNotFoundException("No suche directory: " + dirFile)
    
  	if(dirFile.isDirectory())
    	Queue[File](dirFile.listFiles().filter(_.getName().endsWith(".zip")) : _*)
    else
    	Queue[File](dirFile)
  }
  
  // The next zip file to be read
  private var currentZip : ZipFile = null
  // The next xml file to be read
  private var xmlIterator : Iterator[ZipEntry] = loadNextZip()
  
  def hasNext : Boolean = {
  	// We still have xmls left?
    if(xmlIterator.hasNext)
      return true
      
    // As long as there are zips, let's look for xmls...
    while(!zips.isEmpty){
      xmlIterator = loadNextZip()
      if(xmlIterator.hasNext)
      	return true
    }
    
    false
  }

  def next() : TipsterParse = {
  	if(!hasNext) 
  		throw new NoSuchElementException()
  		
  	// Open file, parse it and close it
  	val entry = xmlIterator.next
  	val is = currentZip.getInputStream(entry)
  	val xml = new TipsterParse(is)
  	is.close()
  	xml
  }
    
  /** Load next zip file and keep iterator to its content
   * 
   */
  private def loadNextZip() : Iterator[ZipEntry] = {
  	if(currentZip != null)
  		currentZip.close()
  	if(zips.isEmpty)
  		Iterator.empty
  	else{
  		Try(new ZipFile(zips.dequeue))  match {
  			case Success(zip) => { currentZip = zip; zip.entries.toIterator }
  			case _ => Iterator.empty
  		}
  	}
  }
 
}

/** Example code counting number of tokens in corpus
 * 
 */
object TipsterCorpusIterator {
	def main(args : Array[String]) = {
		println(System.currentTimeMillis())
	    val path = "/home/schmiflo/Data/IR2014/Tipster/zips"
	  	val iter = new TipsterCorpusIterator(path)
	    
	  	var count = 0;
		while(iter.hasNext){
	    	val doc = iter.next

	    	count += doc.tokens.size
	    }
	  					println(System.currentTimeMillis())

	  	println(count + " tokens in corpus")

	}
}
