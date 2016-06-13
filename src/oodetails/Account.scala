package oodetails

/**
 * This is a place holder put here just to let this compile in this package without duplicating all the rest of the
 * bank example.
 */
class Customer {
  def addAccount(acc: Account) = ???
}

/**
 * Represents an account with the bank. Potentially a checking or savings account.
 */
class Account private(
    private var _balance: Int,
    val customer: Customer,
    val id: String) {

  customer.addAccount(this) // Make sure the customer knows about this account.

  def this(c:Customer, id:String) = this(0, c, id)
  
  /**
   * Provides public access to the current balance for this account.
   * @return the value of the balance.
   */
  def balance = _balance

  /**
   * This provides the ability to do assignment to the balance with
   * checks that go through deposit and withdraw.
   */
  def balance_=(newBalance: Int): Unit = {
    if (newBalance > _balance) deposit(newBalance - _balance)
    else if (newBalance < _balance) withdraw(_balance - newBalance)
  }

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
   * than the current balance.
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

object Account {
  val acc = new Account(0, new Customer, "01234567")
  acc.balance = 700
  acc.balance += 40
}