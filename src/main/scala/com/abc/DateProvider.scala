package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }

  private var instance: DateProvider = null
}

class DateProvider {
  def now: Date = {
    return Calendar.getInstance.getTime
  }
  // This function returns the day of the year to calculate the number of days passed from the start of the year till current date
    def dayOfYear: Int = {
      Calendar calendar = Calendar.getInstance();
     return calendar.get(Calendar.DAY_OF_YEAR)
  }
}

