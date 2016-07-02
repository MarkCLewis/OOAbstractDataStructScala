package regexparser.util

import scala.util.parsing.combinator._

class Formula(formula: String) {
  private val root = Formula.parseAll(Formula.cond, formula).get

  def apply(vars: collection.Map[String, Double]) = root(vars)

  override def toString: String = formula
}

object Formula extends JavaTokenParsers {
  def apply(f: String) = new Formula(f)

  def eval(f: String, vars: collection.Map[String, Double] = null): Double =
    new Formula(f)(vars)

  private def cond: Parser[DNode] = """if\s*\(""".r ~> bform ~ """\)\s*""".r ~ 
    cond ~ """\s*else\s*""".r ~ cond ^^ {
      case b ~ _ ~ e1 ~ _ ~ e2 => new IfNode(b, e1, e2)
    } | form

  private def form: Parser[DNode] = term ~ rep(("+" | "-") ~ term) ^^ {
    case d ~ lst => new LeftAssocBinaryOpDNode(d, lst)
  }

  private def term: Parser[DNode] = exp ~ rep(("*" | "/") ~ exp) ^^ {
    case d ~ lst => new LeftAssocBinaryOpDNode(d, lst)
  }

  private def exp: Parser[DNode] = func ~ rep("^" ~> func) ^^ {
    case d ~ lst => new PowBinaryOpDNode(d, lst)
  }

  private def func: Parser[DNode] = """(sin|cos|tan|sqrt)\(""".r ~ cond <~ ")" ^^ {
    case f ~ n => new FunctionDNode(f, n)
  } | factor

  private def factor: Parser[DNode] = floatingPointNumber ^^ (s => new NumNode(s.toDouble)) |
    ident ^^ (s => new VarNode(s)) | "(" ~> cond <~ ")"

  private def bform: Parser[BNode] = bterm ~ rep("||" ~> bterm) ^^ {
    case b ~ lst => new LeftAssocBinaryOpBNode(b, lst, _ || _)
  }

  private def bterm: Parser[BNode] = bnot ~ rep("&&" ~> bnot) ^^ {
    case b ~ lst => new LeftAssocBinaryOpBNode(b, lst, _ && _)
  }

  private def bnot: Parser[BNode] = "!(" ~> bform <~ ")" ^^ (b => new BNotNode(b)) | bcomp

  private def bcomp: Parser[BNode] = cond ~ ("""[=!><]=|<|>""".r) ~ cond ^^ {
    case c1 ~ op ~ c2 => new CompNode(c1, op, c2)
  }

  private trait DNode {
    def apply(vars: collection.Map[String, Double]): Double
  }

  private trait BNode {
    def apply(vars: collection.Map[String, Double]): Boolean
  }

  private class LeftAssocBinaryOpDNode(first: DNode, restStr: List[~[String, DNode]]) extends DNode {
    val rest = for (~(op, n) <- restStr) yield (op match {
      case "+" => (_: Double) + (_: Double)
      case "-" => (_: Double) - (_: Double)
      case "*" => (_: Double) * (_: Double)
      case "/" => (_: Double) / (_: Double)
    }, n)
    def apply(vars: collection.Map[String, Double]): Double =
      rest.foldLeft(first(vars))((d, t) => {
        t._1(d, t._2(vars))
      })
  }

  private class PowBinaryOpDNode(first: DNode, rest: List[DNode]) extends DNode {
    def apply(vars: collection.Map[String, Double]): Double =
      math.pow(first(vars), rest.foldRight(1.0)((n, d) => math.pow(n(vars), d)))
  }

  private class NumNode(num: Double) extends DNode {
    def apply(vars: collection.Map[String, Double]): Double = num
  }

  private class VarNode(name: String) extends DNode {
    def apply(vars: collection.Map[String, Double]): Double = vars(name)
  }

  private class FunctionDNode(name: String, arg: DNode) extends DNode {
    val f: Double => Double = name match {
      case "sin(" => math.sin
      case "cos(" => math.cos
      case "tan(" => math.tan
      case "sqrt(" => math.sqrt
    }
    def apply(vars: collection.Map[String, Double]): Double = f(arg(vars))
  }

  private class IfNode(cond: BNode, e1: DNode, e2: DNode) extends DNode {
    def apply(vars: collection.Map[String, Double]): Double =
      if (cond(vars)) e1(vars) else e2(vars)
  }

  private class LeftAssocBinaryOpBNode(first: BNode, rest: List[BNode],
                                       op: (Boolean, Boolean) => Boolean) extends BNode {
    def apply(vars: collection.Map[String, Double]): Boolean =
      rest.foldLeft(first(vars))((tf, b) => op(tf, b(vars)))
  }

  private class BNotNode(arg: BNode) extends BNode {
    def apply(vars: collection.Map[String, Double]): Boolean = !(arg(vars))
  }

  private class CompNode(left: DNode, compStr: String, right: DNode) extends BNode {
    val comp: (Double, Double) => Boolean = compStr match {
      case "<" => _ < _
      case ">" => _ > _
      case "<=" => _ <= _
      case ">=" => _ >= _
      case "==" => _ == _
      case "!=" => _ != _
    }
    def apply(vars: collection.Map[String, Double]): Boolean =
      comp(left(vars), right(vars))
  }
}