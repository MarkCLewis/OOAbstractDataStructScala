package test.stackqueue.util

import org.junit._
import org.junit.Assert._
import stackqueue.util.RPNCalc

class TestRPNCalc {
  @Test def basicOps {
    assertEquals(5, RPNCalc("2 3 +".split(" "), null), 0.0)
    assertEquals(6, RPNCalc("2 3 *".split(" "), null), 0.0)
    assertEquals(3, RPNCalc("6 2 /".split(" "), null), 0.0)
    assertEquals(1, RPNCalc("3 2 -".split(" "), null), 0.0)
  }

  @Test def twoOps {
    assertEquals(20, RPNCalc("2 3 + 4 *".split(" "), null), 0.0)
    assertEquals(3, RPNCalc("2 3 * 3 -".split(" "), null), 0.0)
    assertEquals(5, RPNCalc("6 2 / 2 +".split(" "), null), 0.0)
    assertEquals(0.25, RPNCalc("3 2 - 4 /".split(" "), null), 0.0)
  }

  @Test def vars {
    val v = Map("x" -> 3.0, "y" -> 2.0)
    assertEquals(20, RPNCalc("2 x + 4 *".split(" "), v), 0.0)
    assertEquals(3, RPNCalc("y 3 * x -".split(" "), v), 0.0)
    assertEquals(5, RPNCalc("6 2 / y +".split(" "), v), 0.0)
    assertEquals(0.25, RPNCalc("x y - 4 /".split(" "), v), 0.0)
  }

  @Test def specials {
    val v = Map("pi" -> 3.14159, "x" -> 3.0, "y" -> 2.0)
    assertEquals(0, RPNCalc("pi cos 1 +".split(" "), v), 1e-8)
    assertEquals(math.sqrt(2) + 3, RPNCalc("y sqrt x +".split(" "), v), 1e-8)
    assertEquals(1, RPNCalc("pi 2 / sin".split(" "), v), 1e-8)
    assertEquals(0.0, RPNCalc("x y y 2 / + - tan".split(" "), v), 1e-8)
  }
}