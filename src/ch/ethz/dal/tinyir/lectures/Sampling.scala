package ch.ethz.dal.tinyir.lectures
import util.Random
import Array._

object Sampling {
  
  /*
  class ReservoirSampler[T] (size: Int) {
  
    def sampler(s: Stream[T]) : Set[T] = {
     val is = s.zipWithIndex
     val sample = new Array[T](size)

for ((x,i) <- is) {
       if (i<m) sample(i) = x
       else double prob = (double) m / (double) t
     boolean select = rand.nextDouble() < prob
     if (select) sample[rand.nextInt(m)] = x;
     }
     Set[T]()
   } 
 }
  
  
  Random rand = new Random() // random generator
Object[] sample = new Object[m] // sample set
int t = 0 // iteration counter
for (x : stream) {
   if (t<m) sample[t]=x
   else {
     double prob = (double) m / (double) t
     boolean select = rand.nextDouble() < prob
     if (select) sample[rand.nextInt(m)] = x;
}
t++; }
  */
  
    def main (args: Array[String]) = {
  
      def bootstrap[T: Manifest](data: Array[T]) =
        sampleWithR(data, data.length) 
      
      def sampleWithR[T: Manifest](data: Array[T], num: Int) =
        new Array[T](num).map(_ => data(Random.nextInt(data.length)))      
      
      def sampleWithoutR[T: Manifest](data: Array[T], num: Int) = {
        val sample = scala.collection.mutable.Set[T]()              
        while (sample.size < num) {
          sample += data(Random.nextInt(data.length))
        }
        sample.toArray
      }
      
      def bernoulli[T: Manifest](stream: Stream[T], prob: Double) :
        Array[T] = stream.filter(_ => Random.nextDouble < prob).toArray

      def poisson[T: Manifest](stream: Stream[T], prob: T => Double) :
        Array[T] = stream.filter(x => Random.nextDouble < prob(x)).toArray
        
      def strataRandom[T](m: Int)(x: T) = Random.nextInt(m) 

      class Person(val age: Int)
      def ageStrata(someone: Person) = someone.age match {
        case x if x < 16 => 1
        case x if x < 18 => 2
        case _ => 0
      }
 
      def longSampling[T: Manifest](s: Stream[T], key: T => Int, mods: Int, totalMods: Int) =
        s.filter(e => key(e).hashCode % totalMods < mods).toArray

        def constP[T](p: Double)(e: T) : Double = p        
      def bernoulli2[T: Manifest](s: Stream[T], p: Double) =
        poisson(s, constP(p))

      def sampler[T: Manifest](stream: Stream[T], num: Int) : Array[T] = {
        val is = stream.zipWithIndex
        val sample = new Array[T](num)

        // create initial set from first num elements
        for ((e,i) <- is.take(num))
          sample(i) = e
               
        // sampling via randomly replacing elements 
        for ((e,i) <- is.drop(num)) 
          //if ((i+1)* Random.nextDouble < num)
          if (1+Random.nextInt(i) <= num)
            sample(Random.nextInt(num)) = e 
        sample
      }
    
      def sampleAverage(data: Array[Double]) : Double =  
        data.sum / data.length.toDouble  

      def bootstrapVariance(data: Array[Double], re: Int) = {
        val average = sampleAverage(data)
        var s2 = 0.0
        for (i <- 0 to re) {
          val deviation = average - sampleAverage(bootstrap(data))
          s2 += deviation * deviation
        }
        s2 / (re.toDouble-1)
      }
        
      
      val myList = List(4, 5, 10, 11, 11, 11, 12, 45, 89)
      println("Data sample = " + myList.toArray.sorted.mkString(" "))
      println("Bootstrap #1= " + bootstrap(myList.toArray).sorted.mkString(" "))
      println("Bootstrap #2= " + bootstrap(myList.toArray).sorted.mkString(" "))
      println("Bootstrap #3= " + bootstrap(myList.toArray).sorted.mkString(" "))
      
      println(sampler(myList.toStream,3).mkString(" "))
      println(bernoulli2(myList.toStream,0.5).mkString(" "))
      println(sampleWithR(myList.toArray,3).mkString(" "))
      println(sampleWithoutR(myList.toArray,3).mkString(" "))
      
      println(ageStrata(new Person(17)))
    }
    
    
}