import scala.concurrent.{Await, Future, duration}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.async.{async, await}

object Test extends App {
  def fetchURL(url: String): Future[String] = Future {println("fetching"); s"fetched $url"}

  val sumLengths: Future[Int] = async {
    val body1 = fetchURL("http://scala-lang.org")
    val body2 = fetchURL("http://docs.scala-lang.org")
    await(body1).length + await(body2).length
  }

  Await.result(async {println(await(sumLengths)) }, duration.Duration.Inf)
}
