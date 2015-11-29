package main

import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import scala.collection.mutable.{Map => MutMap, MutableList => MutList}
import scala.io.Source

/**
 * Class that manages the whole flow.
 *
 * @param resourcePath Path to a folder with the zip
 *                     files containing the documents.
 *
 * @author Stefan Irimescu
 */
class AlertSystemManager (val resourcePath: String) {
  var cf = MutMap[String, Int]() // Collection frequencies
  var df = MutMap[String, Int]() // Document frequencies
  var perQueryRelDocIds: Map[String, List[String]] = null //Map containing the Queries and the relevant documents(dataset)

  /**
   * The main method where the whole workflow is executed.
   */
  def run() {
    val queries = loadQueries()                               // Load the queries from the topics file
    computeCollectionInformation()                            // Compute cf and df
    TfIdf.initCollectionStats(cf, df)                         // Initialize cf and df for the tf-idf computation
    LanguageModel.initCollectionStats(cf)                     // Initialize cf and df for the LM computation

    // Iterator for parsing all the documents and evaluating the score for all the queries
    val queryScoreIterator = new TipsterCorpusIterator(resourcePath)
    // A map that stores for each query its corresponding hundred best matching documents based on tf-idf score
    val tfIdfScores: Map[Query, CustomMaxHeap] = queries.map(q => q -> new CustomMaxHeap(100)).toMap
    // A map that stores for each query its corresponding hundred best matching documents based on LM score
    val lmScores: Map[Query, CustomMaxHeap] = queries.map(q => q -> new CustomMaxHeap(100)).toMap

    while(queryScoreIterator.hasNext) {
      val doc = queryScoreIterator.next()
      val docLength = doc.tokens.size.toDouble
      val docTf: Map[String, Int] = TfIdf.tf(doc.tokens)
      for(query <- queries) {
        // Compute the tf-idf score for the given doc and query
        val tfIdfScore = TfIdf.score(doc, query.tokens)
        tfIdfScores.get(query).get.add(tfIdfScore, doc.name)

        // Compute the LM score for the given doc and query
        val lmScore = LanguageModel.score(docTf, query.tokens, docLength)
        lmScores.get(query).get.add(lmScore, doc.name)
      }
    }

    // Compute the final result
    computeResult(tfIdfScores, lmScores)
  }

  /**
   * Method that loads the queries from the topics file.
   *
   * @return List containing all the queries in the topics file.
   */
  def loadQueries() : List[Query] = {
    var queries = MutList[Query]()
    queries += new Query(51, Seq("airbus", "subsidies").toList) // TODO Example query. This line has to be deleted.

    // TODO Needs to be implemented. Load all the topics from the topics file.
    /*queryWords = mutable.MutableList[String]()

    for (line <- Source.fromFile("Resources/topics").getLines()) {
      if (line.startsWith("<title>")) {
        line.replace("<title>", "").replace("Topic", "")
          .replace(":", "").replace(";", "").replace("&", "").replace("-", "").trim()
          .split(" ").foreach { word => if (!word.isEmpty) queryWords += word.toLowerCase }
      }
    }*/

    queries.toList
  }

  /**
   * This method loads the collection and document frequencies
   * in the variables cf and df respectively. If those frequencies
   * have been previously computed and cached in files, the information
   * gets loaded from these files which removes the necessity of a whole parse.
   *
   */
  def computeCollectionInformation(): Unit = {
    val cached = false
    if(cached) {
      println("Loading cf and df...")
      // TODO Load cf and df from file
      println("Loaded cf and df successfully.")
    } else {
      // Iterator for the first parse where the collection and document frequencies are computed
      val iterator = new TipsterCorpusIterator(resourcePath)

      println("Computing cf and df...")
      while(iterator.hasNext) {
        val doc = iterator.next()
        cf ++= doc.tokens.groupBy(identity).mapValues(l => l.length + cf.getOrElse(l.head, 0))
        df ++= doc.tokens.distinct.map(t => t -> (1 + df.getOrElse(t,0)))
      }
      println("Computed cf and df successfully.")
      cacheCollectionInformation()
    }
  }

  /**
   * This method caches the collection and document frequencies
   * into files that can be used for future runs.
   */
  def cacheCollectionInformation(): Unit = {
    // TODO
  }

  /**
   * This method computes the statistics for the tf-idf and LM scores
   * based on the information contained in the qrels.
   *
   * @param tfIdfScores The best matching documents for a given query based on the tf-idf scores.
   * @param lmScores The best matching documents for a given query based on the LM scores.
   */
  def computeResult(tfIdfScores: Map[Query, CustomMaxHeap], lmScores: Map[Query, CustomMaxHeap]): Unit = {

    /*
    val file = "/Users/prabhu/IdeaProjects/SearchEngine/Resources/qrels"
    val lines = Source.fromFile(file).getLines.toList.map(x => x.split(" ").toList)
    perQueryRelDocIds = lines.filter(x => x(3)=="1").map(x => (x(0),x(2))).groupBy(_._1).mapValues(_.map(x => x._2))

    val ev = new Evaluate
    var map:Double = 0.0

    perQueryRelDocIds.foreach(x => {
      ev.eval(tfIdfScores.get(x._1),x._2)
      println("Precision:"+ev.precision+" Recall:"+ev.recall+" F1:"+ev.f1)
      map = map + ev.AvgPrecision
    })

    map = map/perQueryRelDocIds.size.toDouble
    */
  }
}