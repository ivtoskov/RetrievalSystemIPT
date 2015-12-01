package main
import scala.collection.mutable.{Map => MutMap}
/**
 * The main class that starts the application.
 *
 * @author Stefan Irimescu
 */
object MainObject {
  def main(args: Array[String]) {
    var linkArg : String = "/home/kennelcrash/MySpace/Studium/ETH/Semester1/InformationRetrieval/Project2/zips"
    if(args.length > 0)
      linkArg = args.apply(0)
    val manager = new AlertSystemManager(linkArg)
    manager.run()
  }

  def test(): Unit = {
    val docTf = TfIdf.tf(List("w1", "w2", "w3"))
    val collectionFrequencies = MutMap[String, Int]()
    collectionFrequencies("w1") = 1
    collectionFrequencies("w2") = 2
    collectionFrequencies("w3") = 3
    collectionFrequencies("w4") = 4
    collectionFrequencies("w5") = 5
    LanguageModel.initCollectionStats(collectionFrequencies, 15)
    val query1 = List("w1", "w2", "w3")
    val query2 = List("w1", "w2", "w6")
    val query3 = List("w7", "w5", "w6")
    val query4 = List("w1", "w8", "w6")
    val query5 = List("w3", "w8", "w6")

    println(LanguageModel.score(docTf, query1, docTf.size))
    println(LanguageModel.score(docTf, query2, docTf.size))
    println(LanguageModel.score(docTf, query3, docTf.size))
    println(LanguageModel.score(docTf, query4, docTf.size))
    println(LanguageModel.score(docTf, query5, docTf.size))
  }
}