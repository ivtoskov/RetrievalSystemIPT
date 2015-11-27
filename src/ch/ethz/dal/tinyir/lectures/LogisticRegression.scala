package ch.ethz.dal.tinyir.lectures

case class SMap (val m: Map[String,Double]) extends AnyVal {
  def *(other: SMap) : Double =     
    m.map{ case (k,v) => v * other.m.getOrElse(k,0.0) }.sum
  def *(scalar: Double) : SMap =     
    SMap(m.mapValues(v => v * scalar))
}

class LogisticRegression {

  def logistic(x: SMap, y: SMap) : Double =
    1.0 / (1.0 + Math.exp(x*y))
  
  def update(th: SMap, x: SMap, c: Boolean) = {
    val z = if (c) (1-logistic(th,x)) else (-logistic(th,x))
    x * z
  }
  
}