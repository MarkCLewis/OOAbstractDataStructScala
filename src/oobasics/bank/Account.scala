package oobasics.bank

/**
 * Represents an account with the bank. Potentially a checking or savings account.
 */
class Account(
    private var _balance: Int,
    val customer: Customer,
    val id: String) {
  
  customer.addAccount(this) // Make sure the customer knows about this account.
  
  /**
   * Provides public access to the current balance for this account.
   * @return the value of the balance.
   */
  def balance = _balance

  /**
   * Makes a deposit to the account.  Deposit values must be positive.
   * @param amount the amount of money to deposit.
   * @return tells is the deposit occurred successfully.
   */
  def deposit(amount: Int): Boolean = {
    if (amount > 0) {
      _balance += amount
      true
    } else false
  }

  /**
   * Makes a withdraw from the account. The amount must be positive and less
   * than or equal to the current balance.
   * @param amount the amount to withdraw.
   * @return tells if the withdraw occurred successfully.
   */
  def withdraw(amount: Int): Boolean = {
    if (amount > 0 && amount <= _balance) {
      _balance -= amount
      true
    } else false
  }
}