package hzuo.euler._019

object Main extends App {

  def isLeap(year: Int) = {
    if (year % 400 == 0) true
    else if (year % 100 == 0) false
    else if (year % 4 == 0) true
    else false
  }

  def daysInMonth(year: Int, month: Int) = {
    val has30 = Set(9, 4, 6, 11)
    if (month == 2) if (isLeap(year)) 29 else 28
    else if (has30(month)) 30
    else 31
  }

  val weekdays = Stream.continually(1 to 7).flatten
  val gen = for {
    year <- 1900 to 2000
    month <- 1 to 12
    day <- 1 to daysInMonth(year, month)
  } yield {
    (year, month, day)
  }

  val target = gen.zip(weekdays).filter {
    case ((year, month, day), weekday) =>
      year != 1900 && day == 1 && weekday == 7
  }

  println(target.size)

}