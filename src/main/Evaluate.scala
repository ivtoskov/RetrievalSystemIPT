/**
 * Created by prabhu on 25/11/15.
 */
package main

class Evaluate {

  var precision: Double = 0.0
  var recall: Double = 0.0
  var AvgPrecision = 0.0
  var f1 = 0.0

  def eval (retreiv: List[String], relev: List[String]) = {

    var i = 1


    retreiv.foreach( x => {
      if(relev.contains(x)){
        precision = (precision * (i-1) + 1.0)/i.toDouble
        recall = recall+ 1.0/relev.length.toDouble
        AvgPrecision = AvgPrecision + precision
      }
      else {
        precision = (precision * (i-1))/i
      }
      println(precision+" "+recall)
      i = i+1
    })

    f1 = 2 * precision * recall / (precision + recall)
    AvgPrecision = AvgPrecision/relev.length.toDouble
    println(AvgPrecision)
  }


}

