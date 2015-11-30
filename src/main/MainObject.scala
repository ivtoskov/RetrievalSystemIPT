package main
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
}