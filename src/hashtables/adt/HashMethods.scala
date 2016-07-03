package hashtables.adt

object HashingMethods extends Enumeration {
  val Division, Multiplication = Value

  private[adt] def buildHashFunction[K](method: Value): (K, Int) => Int = method match {
    case Division => (key, size) => key.hashCode().abs % size
    case Multiplication => (key, size) => {
      val x = key.hashCode() * A
      (size * (x - x.toInt).abs).toInt
    }
  }

  val A = (math.sqrt(5) - 1) / 2
}

object ProbingMethods extends Enumeration {
  val Linear, Quadratic = Value

  private[adt] def buildProbingFunction(method: Value): (Int, Int, Int) => Int = method match {
    case Linear => (i, j, size) => (i + j) % size
    case Quadratic => (i, j, size) => (i + 2 * j + 3 * j * j) % size
  }
}