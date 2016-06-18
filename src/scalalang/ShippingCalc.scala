package scalalang

import io.StdIn._

object ShippingCalc {
  def main(args: Array[String]): Unit = {
    println("How many packages would you like to ship?") // This call to println sends text to standard output
    val numPackages = try {
      readInt()
    } catch {
      case _: NumberFormatException => 0 // If the user does not enter an integer value then the value will default to zero
    }
    var totalShippingCost = 0.0 // Be careful to initialize with a double value
    for (n <- 1 to numPackages) {
      println("What is the weight of package "+n+"?")
      totalShippingCost += calculateShippingCharge(readDouble())
    }
    println("Your total shipping charge is $"+totalShippingCost)
  }
  
  def calculateShippingCharge(weight: Double): Double = {
    if (weight <= 2) 2.5
    else if (weight <= 6) 4.0
    else if (weight <= 10) 5.0
    else 6.75
  } // end of calculateShippingCharge
}