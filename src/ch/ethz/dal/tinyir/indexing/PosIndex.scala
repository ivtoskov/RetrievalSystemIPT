package ch.ethz.dal.tinyir.indexing

import ch.ethz.dal.tinyir.processing.{Document,StringDocument}

import scala.math._

case class ProxResult(val id: Int, val lpos: Int, val rpos: Int) extends Result[ProxResult] {
  def matches(that: ProxResult) : Int = {    
	if (this.id != that.id) this.id - that.id
    else if ((max(rpos,that.rpos) - min(lpos,that.lpos)) <= ProxWindow.size) 0 // match
	else this.lpos-that.lpos  // advance in list with the minimal lpos
  }
  def matched(that: ProxResult) = 
    ProxResult(id, min(this.lpos,that.lpos), max(this.rpos,that.rpos))
}

object ProxWindow {
  var size = 1
  def setSize(w: Int) {assert(w>=1); size = w}
}

class PosIndex (docs: Stream[Document]) extends InvertedIndex[ProxResult] {

  case class PosPosting(val id: Int, val pos: Int) extends Ordered[PosPosting] {
    def this(t: PosTuple) = this(t.id, t.pos) 
    def compare(that: PosPosting) = Ordering[Tuple2[Int, Int]].compare((this.id, this.pos), (that.id, that.pos) ) 
  }
  type PostList = List[PosPosting]
  val index : Map[String, PostList] = {
    val groupedPostings = postings(docs).groupBy(_.term)
    groupedPostings.mapValues(_.map(p => PosPosting(p.id,p.pos)).sorted)		     
  }
  
  case class PosTuple(term: String, id: Int, pos: Int) 
  def postings (s: Stream[Document]): List[PosTuple] =
    s.flatMap( d => d.tokens.zipWithIndex.map{ case (tk,pos) => PosTuple(tk,d.ID,pos) } ).toList

  override def results (word: String) : List[ProxResult] = 
    index.getOrElse(word,null).map(p => ProxResult(p.id, p.pos, p.pos))
  override def results (terms: Seq[String]) : List[ProxResult] = results(terms,1)
  def results (terms: Seq[String], win: Int) : List[ProxResult] = {
    val resultLists = terms.map(term => results(term))
    val shortToLongLists = resultLists.sortWith( _.length < _.length)   
    shortToLongLists.reduceLeft( (l1,l2) => InvertedIndex.sIntersect(l1,l2) )
  } 
}

object PosIndex { 
  def main(args : Array[String]) = {
    val d1 = new StringDocument(1,"mr sherlock holmes who was usually very late")
    val d0 = new StringDocument(0,"i can tell a moriaty when i see one said holmes")  
    val stream : Stream[StringDocument] = List(d1,d0).toStream
    val idx = new PosIndex(stream)
    idx.index.foreach{ case (d,lst) => println(d + ": " + lst.mkString(" "))}     
    val q1 = List("mr")
    println(q1.mkString(" ") + " = " + idx.results(q1).mkString(" "))
    val q2 = List("holmes")
    println(q2.mkString(" ") + " = " + idx.results(q2).mkString(" "))
    val q = List("mr","holmes")
    ProxWindow.size = 2
    println(q.mkString(" ") + " = " + idx.results(q).mkString(" "))
  }
}
