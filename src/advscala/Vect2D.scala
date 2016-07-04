package advscala

trait Vect2D {
  val x: Double
  val y: Double
  val mag = math.sqrt(x * x + y * y)
}

object Vect2DTester extends App {
  val v = new Vect2D {
    val x = 3.0
    val y = 4.0
  }

  println(v.mag)

  val v2 = new {
    val x = 3.0
    val y = 4.0
  } with Vect2D
  
  println(v2.mag)
  
  class Vect2DClass(_x: Double, _y: Double) extends {
    val x = _x
    val y = _y
  } with Vect2D {
    // Other stuff we want in the class
  }
  
  val v3 = new Vect2DClass(3,4)
  
  println(v3.mag)
}