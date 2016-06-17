package multithreading1

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.parsing.json.JSON

object WeeksWeatherFor extends App {
  println("Starting app.")
  val days = 1 to 5
  val future = for {
    jsonStr <- loadURL("https://www.metaweather.com/api/location/search/?query=san")
    Some(woeid) <- getWoeid(jsonStr)
    daysJson <- getDaysJSON(woeid, days)
  } {
    daysJson.foreach(println)
  }
  println("About to sleep")
  Thread.sleep(10000)
  println("Done")

  def loadURL(url: String): Future[String] = Future { io.Source.fromURL(url).mkString }

  def getWoeid(jsonStr: String): Future[Option[Int]] = Future {
    val json = JSON.parseFull(jsonStr)
    json.get match {
      case lst: List[Map[String, Any]] =>
        lst.find(m => m("title") == "San Antonio") match {
          case Some(map) =>
            map("woeid") match {
              case d: Double => Some(d.toInt)
              case _ => None
            }
          case None =>
            None
        }
      case _ =>
        None
    }
  }

  def getDaysJSON(woeid: Int, days: Seq[Int]): Future[Seq[String]] = {
    val dayFutures = days.map(day => loadURL(s"https://www.metaweather.com/api/location/$woeid/2016/5/$day/"))
    Future.sequence(dayFutures)
  }
}