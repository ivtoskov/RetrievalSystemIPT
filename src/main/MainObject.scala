package main

import scala.collection.mutable.{Map => MutMap}

/**
 * The main class that starts the application.
 *
 * @author Stefan Irimescu
 */
object MainObject {
  def main(args: Array[String]) {
    var linkArg : String = "Resources/zips"
    if(args.length > 0)
      linkArg = args.apply(0)
    val manager = new AlertSystemManager(linkArg)
    manager.run()
  }
}