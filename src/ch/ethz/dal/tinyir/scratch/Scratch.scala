package ch.ethz.dal.tinyir.scratch

import javax.xml.parsers._
import org.w3c.dom._
import scala.collection.mutable.HashMap
import scala.Range

/*
object Scratch {
	def main(args: Array[String]) {
 *    		val original : String = new String ("A" + "\u00ea" + "\u00f1" + "\u00fc" + "C")
		val utf8Bytes = original.getBytes("UTF8")
		val roundtrip = new String(utf8Bytes, "UTF8");
		println(roundtrip);

		val fname = "/Users/thofmann/Code/tinyir/reuters_RCV1_sample/19960820/2293newsML.xml"

		  //val doc    = new ParseReutersRCV1(fname)
		//println(doc.title)
		//println(doc.body)
		//println(doc.title)		
	
	
	val example = new String("example 2014 and Text in-line *quoted* here")
	val reader = new StringReader(example)
	val st = new StreamTokenizer(reader)
	st.resetSyntax
	st.wordChars(0x23, 0xFF)
	st.whitespaceChars(0x00, 0x20)
	st.quoteChar('*')
	st.lowerCaseMode(true)

	println(example)
	while(st.nextToken() != StreamTokenizer.TT_EOF) {
		print(st.sval + " | ");}	
		
	val step2List = Map( 
      "ational"->"ate", "tional"->"tion", "enci"->"ence",
	  "anci"->"ance", "izer"->"ize", "bli"->"ble", "alli"->"al",
	  "entli"->"ent", "eli"->"e", "ousli"->"ous"->"ization"->"ize",
	  "ation"->"ate", "ator"->"ate", "alism"->"al", "iveness"->"ive",
	  "fulness"->"ful", "ousness"->"ous", "aliti"->"al",
	  "iviti"->"ive", "biliti"->"ble", "logi"->"log")
val step3List = Map (
     "icate"->"ic", "ative"->"", "alize"->"al"->"iciti"->"ic",
     "ical"->"ic", "ful"->"", "ness"->"")
  val padding2  = "  " 
  val doc       = "i do not want this to happen again, not again, not again"
  val doclen    = doc.length()  
  val counters  = new HashMap[String,Integer]
     
  val trigrams = Range(0,doclen-2).map(i => doc.substring(i,i+3)).toList
println(trigrams.mkString(" : "))
}
}
*/