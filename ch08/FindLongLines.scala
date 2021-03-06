import scala.io.Source

object LongLines {
  def processFile(fileName: String, width: Int){
    val source = Source.fromFile(fileName)
    for(line <- source.getLines())
      processLine(fileName, width, line)
  }
  private def processLine(fileName: String, width: Int, line: String){
    if(line.length > width)
      println(fileName + ": " + line.trim)
  }
}

object FindLongLines {
  def main(args: Array[String]){
    val width = args(0).toInt
    for(arg <- args.drop(1))
      LongLines.processFile(arg, width)
  }
}
