package oobasics.bank

/**
 * Represents a loan that the bank has made to a customer.
 */
class Loan(
    private var _balance: Int,
    val customer: Customer,
    val id: String) {
  
  customer.addLoan(this) // Add this loan to the customer.
  
  /**
   * Provides public access to the balance left on this loan.
   * @return the value of the balance.
   */
  def balance = _balance

  /**
   * Make a payment on this loan. Only works for amounts greater than zero and 
   * less than or equal to the current balance.
   * @param amount the amount of the current payment.
   * @return tells if the payment went through successfully. 
   */
  def payment(amount: Int): Boolean = {
    if (amount > 0 && amount <= _balance) {
      _balance -= amount
      true
    } else false
  }
}
