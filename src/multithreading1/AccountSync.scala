package multithreading1

class AccountSync(
    private var _balance: Int,
    val id: String) {

  def balance = _balance

  def deposit(amount: Int): Boolean = synchronized {
    if (amount > 0) {
      val oldBalance = _balance
      val newBalance = oldBalance + amount
      _balance = newBalance
      true
    } else false
  }

  def withdraw(amount: Int): Boolean = synchronized {
    if (amount > 0 && amount <= _balance) {
      val oldBalance = _balance
      val newBalance = oldBalance - amount
      _balance = newBalance
      true
    } else false
  }
}