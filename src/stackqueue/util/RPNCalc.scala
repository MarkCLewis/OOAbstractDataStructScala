package stackqueue.util

import stackqueue.adt.ArrayStack

object RPNCalc {
  def apply(args: Seq[String], vars: collection.Map[String, Double]): Double = {
    val stack = new ArrayStack[Double]
    for (arg <- args; if (arg.nonEmpty)) arg match {
      case "+" => stack.push(stack.pop + stack.pop)
      case "*" => stack.push(stack.pop * stack.pop)
      case "-" =>
        val tmp = stack.pop
        stack.push(stack.pop - tmp)
      case "/" =>
        val tmp = stack.pop
        stack.push(stack.pop / tmp)
      case "sin" => stack.push(math.sin(stack.pop))
      case "cos" => stack.push(math.cos(stack.pop))
      case "tan" => stack.push(math.tan(stack.pop))
      case "sqrt" => stack.push(math.sqrt(stack.pop))
      case v if (v(0).isLetter) => stack.push(try { vars(v) } catch { case ex: NoSuchElementException => 0.0 })
      case x => stack.push(try { x.toDouble } catch { case ex: NoSuchElementException => 0.0 })
    }
    stack.pop
  }
}