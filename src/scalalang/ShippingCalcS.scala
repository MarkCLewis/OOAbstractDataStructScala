package scalalang

import io.StdIn._

object ShippingCalcS {
  def main(args: Array[String]): Unit = {
    println("How many packages would you like to ship?") // This call to println sends text to standard output
    val numPackages = try {
      readInt()
    } catch {
      case _: NumberFormatException => 0 // If the user does not enter an integer value then the value will default to zero
    }
    val shippingCharges = for (n <- 1 to numPackages) yield {
      println("What is the weight of package "+n+"?")
      calculateShippingCharge(readDouble())
    }
    println("Your total shipping charge is $"+shippingCharges.sum)
  }
  
  def calculateShippingCharge(weight: Double): Double = {
    if (weight <= 2) 2.5
    else if (weight <= 6) 4.0
    else if (weight <= 10) 5.0
    else 6.75
  } // end of calculateShippingCharge
} 
