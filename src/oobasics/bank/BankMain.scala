package oobasics.bank

import io.StdIn._

/**
 * Primary text-based interface for our bank.
 */
object BankMain {
  def main(args: Array[String]): Unit = {
    val bank = new Bank
    var option = 0
    var customer: Option[Customer] = None
    var account: Option[Account] = None
    var loan: Option[Loan] = None

    while (option != 99) {
      println(menu)
      println("Selected Customer: "+customer.map(c => c.firstName+" "+c.lastName).getOrElse("None"))
      println("Selected Account: "+account.map(a => a.id+" ("+a.customer.firstName+" "+a.customer.lastName).getOrElse("None"))
      println("Selected Loan: "+loan.map(l => l.id+" ("+l.customer.firstName+" "+l.customer.lastName).getOrElse("None"))
      option = readInt()
      option match {
        case 1 => customer = Some(createCustomer(bank))
        case 2 => customer = selectCustomer(bank)
        case 3 => account = customer.map(c => createAccount(bank, c))
        case 4 => account.foreach(a => closeAccount(bank, a))
        case 5 => loan = customer.map(c => makeLoan(bank, c))
        case 6 => account = selectAccount(bank)
        case 7 => loan = selectLoan(bank)
        case 8 => account.foreach(a => deposit(bank, a))
        case 9 => account.foreach(a => withdraw(bank, a))
        case 10 => account.foreach(a => checkAccountBalance(a))
        case 11 => loan.foreach(l => payLoan(bank, l))
        case 12 => loan.foreach(l => checkLoanBalance(l))
        case 13 => customer.foreach(c => changeAddress(c))
        case 14 => customer.foreach(c => changePhone(c))
        case 99 =>
        case _ => println("That is not a valid option. Please select again.")
      }
    }
    println("Goodbye.")
  }

  private def createCustomer(bank: Bank): Customer = {
    println("What is the customer's first name?")
    val firstName = readLine()
    println("What is the customer's last name?")
    val lastName = readLine()
    println("What is the customer's address? (End your input with a blank line.)")
    val address = readAddress()
    println("What is the customer's phone number?")
    val phoneNumber = new PhoneNumber(readLine())
    bank.addCustomer(firstName, lastName, address, phoneNumber)
  }

  private def selectCustomer(bank: Bank): Option[Customer] = {
    println("Do you want to find a customer by name or id? (name/id)")
    var style = readLine()
    while (style != "name" && style != "id") {
      println("Invalid response. Do you want to find a customer by name or id? (name/id)")
      style = readLine()
    }
    if (style == "name") {
      println("Enter the first and last name of the customer separated by a space.")
      val names = readLine().trim.split(" +")
      bank.findCustomer(names(0), names(1))
    } else {
      println("Enter the customer ID you are looking for.")
      val id = readLine().trim
      bank.findCustomer(id)
    }
  }

  private def createAccount(bank: Bank, customer: Customer): Account = {
    bank.createAccount(customer)
  }

  private def closeAccount(bank: Bank, account: Account): Unit = {
    bank.closeAccount(account)
  }

  private def makeLoan(bank: Bank, customer: Customer): Loan = {
    println("How much is the loan for?")
    val amount = readInt
    bank.makeLoan(customer, amount)
  }

  private def selectAccount(bank: Bank): Option[Account] = {
    println("What is the ID of the account?")
    bank.findAccount(readLine())
  }

  private def selectLoan(bank: Bank): Option[Loan] = {
    println("What is the ID of the loan?")
    bank.findLoan(readLine())
  }

  private def deposit(bank: Bank, account: Account): Unit = {
    println("How much do you want to deposit?")
    val worked = account.deposit(readInt())
    if (worked) println("The deposit was successful.")
    else println("The deposit failed.")
  }

  private def withdraw(bank: Bank, account: Account): Unit = {
    println("How much do you want to withdraw?")
    val worked = account.withdraw(readInt())
    if (worked) println("The withdraw was successful.")
    else println("The withdraw failed.")
  }

  private def checkAccountBalance(account: Account): Unit = {
    println(account.balance)
  }

  private def payLoan(bank: Bank, loan: Loan): Unit = {
    println("How much do you want to pay on the loan?")
    val worked = loan.payment(readInt())
    if (worked) println("The loan payment was successful.")
    else println("The loan payment failed.")
  }

  private def checkLoanBalance(loan: Loan): Unit = {
    println(loan.balance)
  }

  private def changeAddress(customer: Customer): Unit = {
    println("Enter a new address for "+customer.firstName+" "+customer.lastName+".")
    println("You can have multiple lines. Enter a blank line when done.")
    val address = readAddress()
    customer.changeAddress(address)
  }

  private def changePhone(customer: Customer): Unit = {
    println("Enter a new phone number for "+customer.firstName+" "+customer.lastName+".")
    val phoneNumber = new PhoneNumber(readLine())
    customer.changePhoneNumber(phoneNumber)
  }

  private def readAddress(): Address = {
    def helper(): List[String] = {
      val input = readLine()
      if (input.isEmpty()) Nil
      else input :: helper()
    }
    new Address(helper())
  }

  private val menu = """Select one of the following options.
1. Create Customer
2. Select Customer
3. Create Account
4. Close Account
5. Make Loan
6. Select Account
7. Select Loan
8. Deposit to Account
9. Withdraw from Account
10. Check Account Balance
11. Make Payment on Loan
12. Check Loan Balance
13. Change Address
14. Change Phone
99. Quit"""
}