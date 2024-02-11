
fun println(ln: Any): Unit => {
  java {
    "System.out.println(ln);"
  }
  ()
}

fun red_encode(msg: String): String => "\033[31m" + msg + "\033[0m"
fun green_encode(msg: String): String => "\033[32m" + msg + "\033[0m"
fun succ(): String => green_encode("Success")
fun err(): String => red_encode("Error:")

fun panic(msg: String): Unit => {
  java {
    "Intrinsic.panic(msg);"
  }
  ()
}

enum Result = Ok(value: Any) | Err(msg: String)

fun unwrap(r: Result): Any => match r {
  Result.Ok(value) -> value
  Result.Err(msg) -> panic(" Attempt to unwrap an error value of type `Result.Err` with message: " + red_encode(msg))
  _ -> ()
}

fun someOther(n: Result): Any => {
  unwrap(n)
}

 
fun main(): Int => {
  val n = Result.Err("Just testing things out")
  someOther(n)
  0
}
