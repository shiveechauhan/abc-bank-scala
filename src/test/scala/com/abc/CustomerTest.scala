package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(2)
  }
  // Test to check that one customer can have only one of a particular account type
  
 
   it should "throw IllegalArgumentException This account type already exist. Please provide a new Account Type" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.SAVINGS))
  }
  
   it should "throw IllegalArgumentException One of the accounts does not exist" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val bob: Customer = new Customer("Bob").openAccount(checkingAccount)
    bob.transferAmount(300,Account.SAVINGS,Account.CHECKING)
  }
  
  it should "throw IllegalArgumentException Please provide two seperate account types" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val bob: Customer = new Customer("Bob").openAccount(checkingAccount)
    bob.transferAmount(300,Account.CHECKING,Account.CHECKING)
  }
  // Test to transfer amount between two accounts for a customer
  
   it should "transferAmount" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val shivee: Customer = new Customer("Shivee").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(1000.0)
    savingsAccount.deposit(4000.0)
    shivee.transferAmount(300,Account.SAVINGS,Account.CHECKING)
    shivee.getStatement should be("Statement for Shivee\n" +
      "\nChecking Account\n  deposit $1000.00\nTotal $1000.00\n" +
      "\nSavings Account\n  deposit $4000.00 \nTotal $4000.00\n" +
      "\nChecking Account\n  withdrawal $300.00 \nTotal $700.00\n" +
      "\nSavings Account\n  deposit $300.00 \nTotal $4300.00\n" +
      "\nTotal In All Accounts $5000.00")
  }

  ignore should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(3)
  }
  
  
}
