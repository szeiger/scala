object Test extends App {
  #if feature == "foo"
  val values = sys.cfg("feature")
  #endif

  #if(feature == "unset")
  val values = ???
  #endif

  println(values.sorted.mkString(","))
  println(Foo())
}

case object Foo {
  #if feature == "foo"
    def apply(firstFields: String*) = 1
  #else
    def apply(fieldNames: String*) = 2
  #endif
}
