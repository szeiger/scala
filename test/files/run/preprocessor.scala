object Test extends App {
  @if(feature == "foo")
  val values = sys.cfg("feature")

  @if(feature == "unset")
  val values = ???

  println(values.sorted.mkString(","))
}
