
(* Input routines lifted from java *)

fun readString(): String => {
  val s = " "
  java {
    "s = new Scanner(System.in).next();"
  }
  s
} 

