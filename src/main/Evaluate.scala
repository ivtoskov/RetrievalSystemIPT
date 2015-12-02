package main

/**
 * Class that calculates the performance measurements per query.
 * This includes precision, recall, f1 and MAP.
 *
 * @author Prabhakaran Santhanam
 */
class Evaluate {
  var precision = 0.0
  var recall = 0.0
  var AvgPrecision = 0.0
  var f1 = 0.0

  /**
   * A method that evaluates the precision, recall, AP and F1 for
   * list of retrieved documents with given list of relevant documents.
   *
   * @param retreiv List containing the names of the documents retrieved by the system.
   * @param relev List containing the names of the documents that are specified in the qrels.
   */
  def eval (retreiv: List[String], relev: List[String]) = {
    var i = 1.0

    val rel = relev.map(x => x.replaceAll("-",""))
    retreiv.foreach( x => {
      if(rel.contains(x)){
        precision = (precision * (i-1) + 1.0) / i
        recall += 1.0
        AvgPrecision += precision
      } else {
        precision = (precision * (i-1)) / i
      }
      i += 1.0
    })

    recall /= relev.length.toDouble
    f1 = 2 * precision * recall / (precision + recall)
    AvgPrecision /= scala.math.min(retreiv.length.toDouble, relev.length.toDouble)
  }
}

