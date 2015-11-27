package ch.ethz.dal.tinyir.lectures

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.Document
import ch.ethz.dal.tinyir.indexing.SimpleIndex
import ch.ethz.dal.tinyir.util.StopWatch

object ForwardIndex {

  def fwIndex (docs: Stream[Document]) : Map[Int,List[String]] = 
    fwStream(docs).toMap
  def fwStream (docs: Stream[Document]) : Stream[(Int,List[String])] = 
    docs.map(d=> (d.ID, d.tokens))
    
  def postingsFw (fwIndex: Stream[(Int,List[String])]) : Stream[(String,Int)] = 
    fwIndex.flatMap{ case(id,lst) => lst.map(term => (term,id)) }
  def postings (docs: Stream[Document]) : Stream[(String,Int)] = 
    docs.flatMap( d => d.tokens.map(tk => (tk,d.ID)))

  case class TfTuple(term: String, doc: Int, count: Int) 
  def tfTuples (docs: Stream[Document]) : Stream[TfTuple] = 
    docs.flatMap( d => d.tokens.groupBy(identity)
        .map{ case (tk,lst) => TfTuple(tk, d.ID, lst.length)})
    
def main (args: Array[String]) = {
  
  val n = 1000
  val reuters  = new ReutersRCVStream("/Users/thofmann/Data/ReutersRCV/zips")
  val stream = reuters.stream.take(n)  
  
  val sw = new StopWatch
  sw.start
  val idx = new SimpleIndex(stream) 
  sw.stop
  println("Index with " + idx.index.keys.size + " posting lists")
  println("... number of postings  = " + idx.index.values.map(_.length).sum)
  println("... number of documents = " + n)
  println("... " + sw.stopped)
  }
}

