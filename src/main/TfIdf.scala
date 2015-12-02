package main

import scala.collection.mutable.{ OpenHashMap => MutMap}
import scala.math.log10

/**
 * A class used to compute tf-idf based score for a given
 * document-query pair.
 *
 * @author Stefan Irimescu
 */
object TfIdf {
  var idf = Map[String, Double]()   // Inverted document frequencies
  var numberOfDocuments: Long = 0L  // Total number of documents in the collection

  /**
   * Initializes the inverted document frequencies for the whole collection.
   * This method is executed only once.
   *
   * @param df A map which gives the total number of documents a token appears in.
   * @param numOfDocuments Total number of documents in the collection.
   */
  def initCollectionStats(df: MutMap[String, Int], numOfDocuments: Long): Unit = {
    numberOfDocuments = numOfDocuments
    idf = idf(df.toMap, numberOfDocuments)
  }

  /**
   * A method that computes a score for a given query and document pair.
   *
   * @param words Precomputed logtf/atf map that maps a token to
   *              its logtf/atf in a particular document.
   * @param query Query, consisting of list of tokens, for which the
   *              score against the document is to be computed.
   * @return A score based on the tf-idf result.
   */
  def score(words: Map[String, Double], query: List[String]) : Double = {
    query.flatMap(q => tfidf(words, q) ).sum
  }

  /**
   * A function that computes the tf-idf score of a given word.
   * 
   * @param tf A map containing logtf/atf scores for different tokens.
   * @param word A word for which the tf-idf score has to be computed.
   * @return Option containing the tf-idf score of a word, if it is
   *         contained in the document, None otherwise.
   */
  def tfidf(tf : Map[String, Double], word: String) : Option[Double] = {
    val containedInDocument = tf.get(word)
    if(containedInDocument.isEmpty) {
      return None
    }
    val containedInCollection = idf.get(word)

    Option(containedInDocument.get * containedInCollection.get)
  }

  /**
   * A function that computes the logarithm term frequencies
   * for a given list containing tokens.
   *
   * @param doc List containing tokens.
   * @return Map containing logarithm term frequencies.
   */
  def logtf(doc : List[String]): Map[String, Double] = logtf(tf(doc))

  /**
   * A function that computes the term frequencies for
   * given list of tokens.
   *
   * @param doc List containing tokens.
   * @return Map with the term frequencies for each unique token in doc.
   */
  def tf(doc : List[String]) : Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)

  /**
   * A function that computes the logarithm term frequencies
   * for a given map containing raw term frequencies.
   *
   * @param tf Map containing raw term frequencies.
   * @return Map containing logarithm term frequencies.
   */
  def logtf(tf: Map[String,Int]) : Map[String, Double] = {
    val sum = tf.values.sum.toDouble
    tf.mapValues( v => log2( (1.0 + v.toDouble) / sum ) )
  }

  /**
   * A function that computes the augmented term frequencies
   * for a given map containing raw term frequencies.
   *
   * @param tf Map containing raw term frequencies.
   * @return Map containing augmented term frequencies.
   */
  def atf(tf : Map[String, Int]) = {
    val max = tf.values.max.toDouble
    tf.mapValues( f => 0.5 * f / max + 0.5 )
  }

  /**
   * A function that computes the second logarithm of a number.
   *
   * @param x The number for which log2(x) is to be computed.
   * @return The log2 of x.
   */
  def log2 (x: Double) : Double = log10(x) / log10(2.0)

  /**
   * Computes the inverted document frequencies for a given map of
   * raw term frequencies and the total number of documents.
   *
   * @param df Map containing raw term frequencies in the whole collection.
   * @param n Total Number of documents in the collection.
   * @return The idf values for the tokens that are used as keys in df.
   */
  def idf(df: Map[String, Int], n: Long) : Map[String, Double] = df.mapValues(log2(n) - log2(_))
}