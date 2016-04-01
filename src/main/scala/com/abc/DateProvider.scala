package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }

  def dayOfYear: Int = {
    var calendar: Calendar = Calendar.getInstance();
    return calendar.get(Calendar.DAY_OF_YEAR)
  }

  private var instance: DateProvider = null
}

class DateProvider {
  def now: Date = {
    return Calendar.getInstance.getTime
  }

}

