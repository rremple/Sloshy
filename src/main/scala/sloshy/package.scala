import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.util.Timeout
import scala.concurrent._
import java.util.concurrent.Executors

package object sloshy {
  implicit val timeout: Timeout = Timeout(300.seconds)
  implicit val system = ActorSystem("System")
  
  def log(x: => String) = println(Thread.currentThread.getName + ": " + x)

  def time[U](f: => U): (Double, U) = {
    def current = System.currentTimeMillis
    val start = current
    def elapsed = (current - start).toDouble / 1000
    try {
      val result = f
      (elapsed, result)
    } catch {
      case e: Throwable =>
        throw new Exception(s"Failure after $elapsed seconds", e)
    }
  }

}