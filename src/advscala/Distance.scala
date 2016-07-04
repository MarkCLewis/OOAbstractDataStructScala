package advscala

class Mile(val dist: Double) extends AnyVal {
  override def toString = dist+" mile"+(if (dist != 1.0) "s" else "")
  def +(m: Mile) = new Mile(dist + m.dist)
  def toKilometers = new Kilometer(dist * 1.60934)
}

class Kilometer(val dist: Double) extends AnyVal {
  override def toString = dist+" km"
  def +(km: Kilometer) = new Kilometer(dist + km.dist)
  def toMiles = new Mile(dist * 0.621371)
}

object Distance extends App {
  val marathon = new Mile(26.2)
  val fivek = new Kilometer(5)

  println(marathon + fivek.toMiles)
}