/**
 * Created by ravikupin on 25/9/16.
 */
object Spreadsheet {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines()
    lines.zipWithIndex.foreach{
      case (line, index) => println(s"line: $index value: $line")
     }
  }
}
