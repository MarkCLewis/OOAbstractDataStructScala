package oobasics.bank

/**
 * Represents a customer at the bank.
 */
class Customer(
    val firstName: String,
    val lastName: String,
    val id: String,
    private var _address: Address,
    private var _phoneNumber: PhoneNumber) {

  private var _accounts = List[Account]()
  private var _loans = List[Loan]()

  /**
   * Adds an account to this customer. The account must be associated with this
   * customer. Does nothing if this is already a current account.
   * @param a the account to add.
   */
  def addAccount(a: Account): Unit = {
    require(a.customer == this, "Account being added to wrong customer.")
    if (!_accounts.contains(a)) _accounts ::= a
  }

  /**
   * Adds a loan to this customer. The loan must be associated with this customer.
   * Does nothing is this is already a current loan.
   * @param loan the loan to add to this customer.
   */
  def addLoan(loan: Loan): Unit = {
    require(loan.customer == this, "Loan being added to wrong customer.")
    if (!_loans.contains(loan)) _loans ::= loan
  }

  /**
   * Remove the account with the specified ID from this customer.
   * @param id the account ID to remove.
   * @return tells if the account was there to remove.
   */
  def removeAccount(id: String): Boolean = {
    val index = _accounts.indexWhere(_.id == id)
    if (index < 0) false else {
      _accounts.patch(index, Nil, 1)
      true
    }
  }

  /**
   * Remove the loan with the specified ID from this customer.
   * @param id the loan ID to remove.
   * @return tells if the loan was there to remove.
   */
  def removeLoan(id: String): Boolean = {
    val index = _loans.indexWhere(_.id == id)
    if (index < 0) false else {
      _loans.patch(index, Nil, 1)
      true
    }
  }

  /**
   * Provides public access to the accounts.
   * @return the list of accounts.
   */
  def accounts: List[Account] = _accounts

  /**
   * Provides public access to the loans.
   * @return the list of loans.
   */
  def loans: List[Loan] = _loans

  /**
   * Change the value of this customer's address.
   * @param newAddress the new address of this customer.
   */
  def changeAddress(newAddress: Address): Unit = {
    _address = newAddress
  }

  /**
   * Change the phone number of this customer.
   * @param newPhoneNumber the new phone number of this customer.
   */
  def changePhoneNumber(newPhoneNumber: PhoneNumber): Unit = {
    _phoneNumber = newPhoneNumber
  }
}