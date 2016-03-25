package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {
  
  // Function to check if a particular type of account exists
  private def accountExist(var accType: Int): Boolean ={ 
    val flag: Boolean = false
      for(a <- accounts){
        if(a.accountType == accType)
           flag  = flag || true
      }
      return flag
    }
  
// Since the problem statetemt does not specify the limit on number of accounts of the same type a customer can open, an easier solution
// to solve the #1 will be to limit the no. of accounts to 1 of each type for each customer. The test case has also been changed to
// reflect this constraint. However, in a different aproach, we can introduce the concept of account IDs and set a counter to increment
// the account ID as a new account of the same type is opened.

  def openAccount(account: Account): Customer = {
      if(accountExist(account.accountType))
        {
          throw new IllegalArgumentException("This account type already exist. Please provide a new Account Type")
        }
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    statement = f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    statement
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a.accountType match {
      case Account.CHECKING =>
        "Checking Account\n"
      case Account.SAVINGS =>
        "Savings Account\n"
      case Account.MAXI_SAVINGS =>
        "Maxi Savings Account\n"
    }
    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"
  
   def transferAmount(amount: Double,val fromAccountType: Int,val toAccountType: Int ) {
    if (fromAccountType == toAccountType)
    {
      throw new IllegalArgumentException("Please provide two seperate account types")
    }
    else
    {
          if(!accountExist(fromAccountType) || !accountExist(toAccountType))
          {
            throw new IllegalArgumentException("One of the accounts does not exist")
          }
          else
          {
            val accountFrom: Account = new Account(fromAccountType)
                accountFrom.withdraw(amount)
            val accountTo: Account = new Account(toAccountType)
            accountTo.deposit(amount)
          }
    } 
  }
}

