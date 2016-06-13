package polymorphism

object StreetLightColor extends Enumeration {
  val Red, Green, Yellow = Value
}

class StreetLight(private var _color: StreetLightColor.Value) {
  def color = _color
  
  import StreetLightColor._
  
  def cycle: Unit = _color match {
    case Red => _color = Green
    case Green => _color = Yellow
    case Yellow => _color = Red
  }
}