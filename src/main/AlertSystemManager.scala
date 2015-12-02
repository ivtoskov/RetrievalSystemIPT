package main

import ch.ethz.dal.tinyir.processing.{Tokenizer, TipsterCorpusIterator}
import scala.collection.mutable.{Map => MutMap, MutableList => MutList}
import java.nio.file.{Paths, Files}
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
  var numberOfDocuments: Long = 0L // Corresponds to the total number of documents in the corpus
  var numberOfTokens: Long = 0L // Correspons to the total number of tokens in the corpus

  /**
   * The main method where the whole workflow is executed.
   */
  def run() {
    val queries: List[Query] = loadQueries()                  // Load the queries from the topics file
    val queryTerms: Set[String] = queries.flatMap(_.tokens).toSet // Set containing unique terms from all of the queries
    computeCollectionInformation(queryTerms)                            // Compute cf and df
    TfIdf.initCollectionStats(df, numberOfDocuments)                             // Initialize cf and df for the tf-idf computation
    LanguageModel.initCollectionStats(cf, numberOfTokens)                     // Initialize cf and df for the LM computation

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

      val logDocTf: Map[String, Double] = TfIdf.atf(docTf)
      for(query <- queries) {

        // Compute the tf-idf score for the given doc and query
        val tfIdfScore = TfIdf.score(logDocTf, query.tokens)
        tfIdfScores.get(query).get.add(tfIdfScore, doc.name.replaceAll("-", ""))

        // Compute the LM score for the given doc and query
        val lmScore = LanguageModel.score(docTf, query.tokens, docLength)
        lmScores.get(query).get.add(lmScore, doc.name.replaceAll("-", ""))
      }
    }

    println("Computing Final Results...")
    // Compute the final result
    computeResult(tfIdfScores, lmScores)
  }

  /**
   * Method that loads the queries from the topics file.
   *
   * @return List containing all the queries in the topics file.
   */
  def loadQueries() : List[Query] = {
    val queries = MutList[Query]()

    var num: String = ""

    for (line <- Source.fromFile("Resources/topics").getLines()) {
      if (line.startsWith("<num>")) {
        val helper = line.split("Number:\\s*")
        num = helper(1).replace(" ", "").substring(1)
      }

      if (line.startsWith("<title>")) {
        val helper = line.split("Topic:\\s*")
        queries += new Query(num, Tokenizer.tokenize(helper(1)))
      }
    }
    println("Queries: ")
    queries.toList.foreach(q => println(q.num + " " + q.tokens))

    queries.toList
  }

  /**
   * This method loads the collection and document frequencies
   * in the variables cf and df respectively. If those frequencies
   * have been previously computed and cached in files, the information
   * gets loaded from these files which removes the necessity of a whole parse.
   *
   */
  def computeCollectionInformation(queryTerms: Set[String]): Unit = {
    val cached = Files.exists(Paths.get("Resources/cachedCf.csv")) &&
      Files.exists(Paths.get("Resources/cachedDf.csv")) && Files.exists(Paths.get("Resources/globalStats.csv"))
    if(cached) {
      println("Loading cf and df...")
      loadCollectionInformation()
      println("Loaded cf and df successfully.")
    } else {
      // Iterator for the first parse where the collection and document frequencies are computed
      val iterator = new TipsterCorpusIterator(resourcePath)

      println("Computing cf and df...")
      while(iterator.hasNext) {
        val doc = iterator.next()
        val relevantTokens = doc.tokens.filter(queryTerms.contains)
        cf ++= relevantTokens.groupBy(identity).mapValues(l => l.length + cf.getOrElse(l.head, 0))
        df ++= relevantTokens.distinct.map(t => t -> (1 + df.getOrElse(t, 0)))
        numberOfDocuments += 1
        numberOfTokens += doc.tokens.size
      }
      println("Computed cf and df successfully.")
      cacheCollectionInformation()
    }
    println("Total number of documents: " + numberOfDocuments)
    println("Total number of tokens: " + numberOfTokens)
  }

  /**
   * This method caches the collection and document frequencies
   * into files that can be used for future runs.
   */
  def cacheCollectionInformation(): Unit = {
    val statsWriter = new java.io.PrintWriter(new java.io.File("Resources/globalStats.csv"))
    // Cache the document frequencies
    printToFile(new java.io.File("Resources/cachedDf.csv")) { p =>
      df.keys.foreach(word => p.println(word + "," + df(word)))
    }
    statsWriter.println("Number of documents: " + numberOfDocuments)

    // Cache the collection frequencies
    printToFile(new java.io.File("Resources/cachedCf.csv")) { p =>
      cf.keys.foreach(word => p.println(word + "," + cf(word)))
    }
    statsWriter.println("Number of tokens: " + numberOfTokens)
    statsWriter.close()
  }

  /**
   * This method loads the cached document and collection frequencies.
   */
  def loadCollectionInformation(): Unit = {
    for(line <- io.Source.fromFile("Resources/cachedDf.csv").getLines()) {
      val keyValuePair = line.split(",")
      df(keyValuePair(0)) = keyValuePair(1).toInt
    }

    for(line <- io.Source.fromFile("Resources/cachedCf.csv").getLines()) {
      val keyValuePair = line.split(",")
      cf(keyValuePair(0)) = keyValuePair(1).toInt
    }

    for(line <- io.Source.fromFile("Resources/globalStats.csv").getLines()) {
      if(line.startsWith("Number of documents:")) {
        val keyValuePair = line.split(": ")
        numberOfDocuments = keyValuePair(1).toLong
      } else if(line.startsWith("Number of tokens:")) {
        val keyValuePair = line.split(": ")
        numberOfTokens = keyValuePair(1).toLong
      }
    }
  }

  /**
   * This method computes the statistics for the tf-idf and LM scores
   * based on the information contained in the qrels.
   *
   * @param tfIdfScores The best matching documents for a given query based on the tf-idf scores.
   * @param lmScores The best matching documents for a given query based on the LM scores.
   */
  def computeResult(tfIdfScores: Map[Query, CustomMaxHeap], lmScores: Map[Query, CustomMaxHeap]): Unit = {
    val fileWriter = new java.io.PrintWriter(new java.io.File("performanceStats.txt"))

    val file = "Resources/qrels"
    val lines = Source.fromFile(file).getLines().toList.map(x => x.split(" ").toList)
    perQueryRelDocIds = lines.filter(x => x(3) == "1").map(x => (x.head, x(2).replaceAll("-", ""))).groupBy(_._1).mapValues(_.map(x => x._2))

    var map: Double = 0.0

    fileWriter.println("tf-idf scores: ")
    tfIdfScores.foreach( x => {
      val ev = new Evaluate
      ev.eval(x._2.returnDocuments, perQueryRelDocIds(x._1.num))
      fileWriter.println("Statistics for query " + x._1.num + ":")
      fileWriter.println("Precision: " + ev.precision + ", Recall: " + ev.recall +
        ", F1:" + ev.f1 + ", Average precision: " + ev.AvgPrecision)
      fileWriter.println()
      map = map + ev.AvgPrecision
    } )
    map = map / perQueryRelDocIds.size.toDouble
    val tfIdfMAP = map

    map = 0.0
    fileWriter.println("-----------------------------------------------------")
    fileWriter.println("\n\n\nLM scores: ")
    lmScores.foreach( x => {
      val ev = new Evaluate
      ev.eval(x._2.returnDocuments, perQueryRelDocIds(x._1.num))
      fileWriter.println("Statistics for query " + x._1.num + ":")
      fileWriter.println("Precision: " + ev.precision + ", Recall: " + ev.recall +
        ", F1:" + ev.f1 + ", Average precision: " + ev.AvgPrecision)
      fileWriter.println()
      map = map + ev.AvgPrecision
    } )
    map = map / perQueryRelDocIds.size.toDouble

    fileWriter.println("MAP of the tf-idf scoring: " + tfIdfMAP)
    fileWriter.println("MAP of the language model scoring: " + map)

    fileWriter.close()
  }

  /**
   * A helper method that executes a given operation
   * and writes the output to a specified file.
   *
   * @param f The output file where the result is written.
   * @param op The operation that should be executed.
   */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}