package trees.util

import scala.collection.Map

class Formula(val formula: String) extends (Map[String, Double] => Double) with Serializable {
  private val root = Formula.simplify(Formula.parse(formula))

  def apply(vars: Map[String, Double]): Double = root(vars)

  override def toString = formula
}

object Formula {
  val ops = "+-*/".toSet

  def apply(form: String, vars: Map[String, Double] = null): Double = {
    val root = parse(form.filter(_ != ' '))
    root(vars)
  }

  private def parse(f: String): Node = {
    var opLoc = -1
    var parensCount = 0
    var i = f.length - 1
    while (i > 0) {
      if (f(i) == '(') parensCount += 1
      else if (f(i) == ')') parensCount -= 1
      else if (parensCount == 0 && (f(i) == '+' || f(i) == '-' && !ops.contains(f(i - 1)))) {
        opLoc = i
        i = -1
      } else if (parensCount == 0 && opLoc == -1 && (f(i) == '*' || f(i) == '/')) {
        opLoc = i
      }
      i -= 1
    }
    if (opLoc < 0) {
      if (f(0) == '(') {
        parse(f.substring(1, f.length - 1))
      } else if (f.startsWith("sin(")) {
        new SingleOpNode(parse(f.substring(4, f.length - 1)), math.sin)
      } else if (f.startsWith("cos(")) {
        new SingleOpNode(parse(f.substring(4, f.length - 1)), math.cos)
      } else if (f.startsWith("tan(")) {
        new SingleOpNode(parse(f.substring(4, f.length - 1)), math.tan)
      } else if (f.startsWith("sqrt(")) {
        new SingleOpNode(parse(f.substring(5, f.length - 1)), math.sqrt)
      } else try {
        new NumberNode(f.toDouble)
      } catch {
        case ex: NumberFormatException => new VariableNode(f)
      }
    } else {
      f(opLoc) match {
        case '+' => new BinaryOpNode(parse(f.take(opLoc)), parse(f.drop(opLoc + 1)), _ + _)
        case '-' => new BinaryOpNode(parse(f.take(opLoc)), parse(f.drop(opLoc + 1)), _ - _)
        case '*' => new BinaryOpNode(parse(f.take(opLoc)), parse(f.drop(opLoc + 1)), _ * _)
        case '/' => new BinaryOpNode(parse(f.take(opLoc)), parse(f.drop(opLoc + 1)), _ / _)
      }
    }
  }

  private def simplify(n: Node): Node = n match {
    case BinaryOpNode(l, r, op) =>
      val left = simplify(l)
      val right = simplify(r)
      (left, right) match {
        case (NumberNode(n1), NumberNode(n2)) => NumberNode(op(n1, n2))
        case _ => BinaryOpNode(left, right, op)
      }
    case SingleOpNode(a, op) =>
      val arg = simplify(a)
      arg match {
        case NumberNode(n) => NumberNode(op(n))
        case _ => SingleOpNode(arg, op)
      }
    case _ => n
  }

  private sealed trait Node extends Serializable {
    def apply(vars: Map[String, Double]): Double
  }

  private case class NumberNode(num: Double) extends Node {
    def apply(vars: Map[String, Double]): Double = num
  }

  private case class VariableNode(name: String) extends Node {
    def apply(vars: Map[String, Double]): Double = vars get name getOrElse 0.0
  }

  private case class BinaryOpNode(left: Node, right: Node, op: (Double, Double) => Double) extends Node {
    def apply(vars: Map[String, Double]): Double = op(left(vars), right(vars))
  }

  private case class SingleOpNode(arg: Node, op: Double => Double) extends Node {
    def apply(vars: Map[String, Double]): Double = op(arg(vars))
  }
}