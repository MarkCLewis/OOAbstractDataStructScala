package advscala

class Vect3D(val x: Double, val y: Double, val z: Double) {
  def +(v: Vect3D) = Vect3D(x + v.x, y + v.y, z + v.z)
  def -(v: Vect3D) = Vect3D(x - v.x, y - v.y, z - v.z)
  def *(c: Double) = Vect3D(c * x, c * y, c * z)
  def /(c: Double) = Vect3D(c / x, c / y, c / z)
  def dot(v: Vect3D) = x * v.x + y * v.y + z * v.z
  def cross(v: Vect3D) = Vect3D(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
}

object Vect3D {
  def apply(x: Double, y: Double, z: Double) = {
    new Vect3D(x, y, z)
  }

  def unapply(str: String): Option[(Double, Double, Double)] = {
    val s = str.trim
    if (!s.startsWith("(") || !s.endsWith(")")) None
    else {
      val parts = s.substring(1, s.length - 1).split(",")
      if (parts.length != 3) None
      else try {
        Some(parts(0).trim.toDouble, parts(1).trim.toDouble, parts(2).trim.toDouble)
      } catch {
        case e: NumberFormatException => None
      }
    }
  }

}

object TestingApp extends App {
  //  implicit def doubleToScaling(c: Double): VectScaler = new VectScaler(c)

  implicit class VectScaler(c: Double) {
    def *(v: Vect3D): Vect3D = Vect3D(c * v.x, c * v.y, c * v.z)
  }

  val v = Vect3D(1, 2, 3)
  v * 3
  3 * v

  "(1,4.5,83)" match {
    case Vect3D(x, y, z) => println(x+" "+y+" "+z)
  }

  import Stream._
  def fibFrom(a: BigInt, b: BigInt): Stream[BigInt] = a #:: fibFrom(b, a + b)

  fibFrom(1, 1).take(100).toList

  def structRead[A <: { def hasNextInt(): Boolean; def nextInt(): Int }](source: A): List[Int] = {
    var buf = List[Int]()
    while (source.hasNextInt) buf ::= source.nextInt
    buf.reverse
  }

}
  
