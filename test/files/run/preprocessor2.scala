object Test extends App {
  #if feature == "foo"
  val values = sys.cfg("feature")
  #endif

  #if(feature == "unset")
  val values = ???
  #endif

  println(values.sorted.mkString(","))
}
