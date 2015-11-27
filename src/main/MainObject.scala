

object MainObject {
  def main(args: Array[String]) {
    var linkArg : String = "D:\\Scoala\\ETH\\Information_Retrieval\\download\\IR2015\\tipster\\zips\\zips"
    if(args.length > 0)
      linkArg = args.apply(0)
    val manager = new AlertSystemManager(linkArg)
    manager.run()
  }
}