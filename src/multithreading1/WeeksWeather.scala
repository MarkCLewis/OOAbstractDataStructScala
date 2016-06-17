package multithreading1

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.parsing.json.JSON

object WeeksWeather extends App {
  println("Starting app.")
  val f1 = Future {
    io.Source.fromURL(
        "https://www.metaweather.com/api/location/search/?query=san").mkString
  }
  val f2 = f1.map(jsonStr => {
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
  })
  val days = 1 to 5
  val f3 = f2.flatMap(optInt => optInt match {
    case Some(woeid) =>
      val dayFutures = days.map(day => Future {
        io.Source.fromURL(
            s"https://www.metaweather.com/api/location/$woeid/2016/5/$day/").mkString
      })
      Future.sequence(dayFutures)
    case None => Future { Nil }
  })
  f3.foreach(lst => lst.foreach(println))
  println("About to sleep")
  Thread.sleep(10000)
  println("Done")
}