
enum Result = Ok(value: Any) | Err(msg: String)

fun panic(msg: String): Unit => {
  java {
    "Intrinsic.panic(msg);"
  }
  ()
}

fun unwrap(r: Result): Any => match r {
  Result.Ok(value) -> value
  Result.Err(msg) -> panic("Attempt to unwrap an error value of type `Result.Err` with message: " + msg)
  _ -> ()
}

fun safe_div(x: Int, y: Int): Result => 
  if y == 0 then 
    Result.Err("Divide by zero error")
  else Result.Ok(x / y)

fun main(): Unit => {
  val answer = safe_div(10, 2)
  val err = safe_div(15, 5)
  val ok = unwrap(answer) as Int 
  val not_ok = unwrap(err) 
  ()
}
