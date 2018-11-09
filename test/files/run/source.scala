import scala.sys.Source
import Source.line

object Test extends App {
  println(Source.file)
  println(Source.absFile)
  println(line)
  println(Source.column)
}
