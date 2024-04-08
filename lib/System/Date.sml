
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Date.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)

type LocalDate = "LocalDate"
fun local_date_now(): LocalDate => {
  val dt = null of LocalDate
  java {
    "dt = LocalDate.now();"
  }
  dt
}


type LocalTime = "LocalTime"
fun local_time_now(): LocalTime => {
  val dt = null of LocalTime
  java {
    "dt = LocalTime.now();"
  }
  dt
}


type LocalDateTime = "LocalDateTime"
fun local_date_time_now(): LocalDateTime => {
  val dt = null of LocalDateTime
  java {
    "dt = LocalDateTime.now();"
  }
  dt
}

type DateTimeFormat = "DateTimeFormatter"
fun date_format_new(ptrn: String): DateTimeFormat => {
  val df = null of DateTimeFormat
  java {
    "df = DateTimeFormatter.ofPattern(ptrn);"
  }
  df
}

fun format_date(fmt: DateTimeFormat, date: LocalDate): String => {
  val f = null of String
  java {
    "f = date.format(fmt);"
  }
  f
}

