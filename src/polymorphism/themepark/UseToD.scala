package polymorphism.themepark

object UseToD extends App {
  val riders1 = new ToDValues[Int]
  val riders2 = new ToDValues[Int]
  val worker1 = new ToDValues[String]
  val worker2 = new ToDValues[String]

  riders1(12) = 5
  riders1("8:24AM") = 10
  riders1(14) = 7
  riders2("2:13PM") = 8

  worker1(12) = "Kyle"

  val totalRiders = riders1.combine(riders2, (o1, o2) => (o1, o2) match {
    case (None, None) => None
    case (Some(a), None) => Some(a)
    case (None, Some(b)) => Some(b)
    case (Some(a), Some(b)) => Some(a + b)
  })

  println(riders1)
  println(totalRiders)
}