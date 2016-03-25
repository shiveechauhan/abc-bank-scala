package com.abc

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }
// Refactored the withdrawal function to check if the available balance is enough to perform the withdrawal 
  def withdraw(amount: Double) {
    val totalBalance: Double = sumTransactions()
    if(totalBalance < amount)
      throw new IllegalArgumentException("You dont have enough balance for the withdrawal")
    else
    {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
    }
  }
  
  // Check if there was a withdrawal in the past 10 days. I start traversing the ListBuffer Transactions in reverse order to 
  // get the first instance of withdrawal and return true if it was less than equal to 10 days.
  
  private def pastTenDaysWithdrawal: Boolean ={
     var today: Date = DateProvider.getInstance.now
     for( t <- transactions.reverseIterator){
       if(t.amount < 0 ){
         var DateDiff: Int  = (today - t.transactionDate)
         if(DateDiff <= 10 )
         return true
       }
       return false
     }
  } 

//The function has been changed to calculate the interest rate accrued daily #3. 
  def interestEarned: Double = {
    var numDays: Int = DateProvider.dayOfYear()
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * Math.pow((1+ 0.001/365),numDays)
        else 1 + (amount - 1000) * Math.pow((1+ 0.002/365),numDays)
      case Account.MAXI_SAVINGS =>
     //Adding code to check if there was a withdrawal in past 10 days, and set the interest rate accordingly per #2
          if(pastTenDaysWithdrawal)
             amount * Math.pow((1+ 0.001/365),numDays)
          else
            amount * Math.pow((1+ 0.05/365),numDays)
      case _ =>
        amount * Math.pow((1+ 0.001/365),numDays)
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}
