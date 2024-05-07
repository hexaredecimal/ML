using System::Io
using System::Intrinsics
using System::Result

fun red_encode(msg: String): String => "\033[31m" + msg + "\033[0m"
fun green_encode(msg: String): String => "\033[32m" + msg + "\033[0m"
fun succ(): String => green_encode("Success")
fun err(): String => red_encode("Error:")


fun someOther(n: Result): Any => {
  unwrap_result(n)
}

 
fun main(): Unit => {
  val n = Result.Err("Just testing things out")
  val p = someOther(n)
  println(p)
  ()
}
