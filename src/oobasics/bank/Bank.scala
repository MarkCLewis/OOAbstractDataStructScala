package oobasics.bank

/**
 * Represents a bank with customers, accounts, and loans.
 */
class Bank(
    private var _customers: List[Customer] = Nil,
    private var _accounts: List[Account] = Nil,
    private var _loans: List[Loan] = Nil) {

  private var nextCustomerID: Int = _customers.foldLeft(-7)(_ max _.id.toInt) + 7
  private var nextAccountID: Int = _accounts.foldLeft(-13)(_ max _.id.toInt) + 13
  private var nextLoanID: Int = _loans.foldLeft(-17)(_ max _.id.toInt) + 17

  /**
   * Add a customer to this bank. Creates a new customer with the specified name.
   * @param fname the first name of the new customer.
   * @param lname the last name of the new customer.
   * @return the new customer that was created and added.
   */
  def addCustomer(fname: String, lname: String, address: Address, phoneNumber: PhoneNumber): Customer = {
    val id = nextCustomerID.toString
    val c = new Customer(fname, lname, "0" * (8 - id.length) + id, address, phoneNumber)
    _customers ::= c
    nextCustomerID += 7
    c
  }

  /**
   * Create a new account for a specified customer. The customer must be a member
   * of this bank.
   * @param c the customer to add an account to.
   * @return the account that was created.
   */
  def createAccount(c: Customer): Account = {
    require(_customers.contains(c), "The customer must be a member of this bank.")
    val id = nextAccountID.toString
    val acc = new Account(0, c, "0" * (8 - id.length) + id)
    _accounts ::= acc
    nextAccountID += 13
    acc
  }

  /**
   * Close a specified account and remove it from the associated customer.
   * @param a the account to close.
   * @return tells if the account was found and closed.
   */
  def closeAccount(a: Account): Boolean = {
    val index = _accounts.indexOf(a)
    if (index < 0) false else {
      a.customer.removeAccount(a.id)
      _accounts = _accounts.patch(index, Nil, 1)
      true
    }
  }

  /**
   * Make a loan to a specified customer for a specified amount. The customer
   * must be a member of this bank.
   * @param c the customer to add the loan to.
   * @param amount the initial amount of the loan.
   * @return the loan that was created.
   */
  def makeLoan(c: Customer, amount: Int): Loan = {
    require(_customers.contains(c), "The customer must be a member of this bank.")
    val id = nextLoanID.toString
    val loan = new Loan(amount, c, "0" * (8 - id.length) + id)
    _loans ::= loan
    nextLoanID += 17
    loan
  }

  /**
   * Find a customer by their name.
   * @param fname the first name of the customer you are looking for.
   * @param lname the last name of the customer you are looking for.
   * @return the customer if one is found.
   */
  def findCustomer(fname: String, lname: String): Option[Customer] = {
    _customers.find(c => c.firstName == fname && c.lastName == lname)
  }

  /**
   * Find a customer by their ID.
   * @param cID the ID of the customer being searched for.
   * @return the customer if one is found.
   */
  def findCustomer(cID: String): Option[Customer] = _customers.find(_.id == cID)

  /**
   * Find an account by ID.
   * @param aID the ID of the account being searched for.
   * @return the account if one is found.
   */
  def findAccount(aID: String): Option[Account] = _accounts.find(_.id == aID)

  /**
   * Find a loan by ID.
   * @param loanID the ID of the loan being searched for.
   * @return the loan if one is found.
   */
  def findLoan(loanID: String): Option[Loan] = _loans.find(_.id == loanID)
}