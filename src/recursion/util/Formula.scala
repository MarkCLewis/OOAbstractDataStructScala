package recursion.util

object Formula {
  val ops = "+-*/".toSet

  def eval(form: String): Double = evalParse(form.filter(_ != ' '))

  private def evalParse(f: String): Double = {
    var opLoc = -1
    var parensCount = 0
    var i = f.length - 1
    while (i > 0) {
      if (f(i) == '(') parensCount += 1
      else if (f(i) == ')') parensCount -= 1
      else if (parensCount == 0 && (f(i) == '+' || f(i) == '-' && !ops(f(i - 1)))) {
        opLoc = i
        i = -1
      } else if (parensCount == 0 && opLoc == -1 && (f(i) == '*' || f(i) == '/')) {
        opLoc = i
      }
      i -= 1
    }
    if (opLoc < 0) {
      if (f(0) == '(') {
        evalParse(f.substring(1, f.length - 1))
      } else f.toDouble
    } else {
      f(opLoc) match {
        case '+' => evalParse(f.take(opLoc)) + evalParse(f.drop(opLoc + 1))
        case '-' => evalParse(f.take(opLoc)) - evalParse(f.drop(opLoc + 1))
        case '*' => evalParse(f.take(opLoc)) * evalParse(f.drop(opLoc + 1))
        case '/' => evalParse(f.take(opLoc)) / evalParse(f.drop(opLoc + 1))
      }
    }
  }
}