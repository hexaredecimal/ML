
enum Option = Some(x: Any) | None

fun println(...): Unit => {
  java {
    "for (Object arg: var_args) {"
    "  System.out.print(arg);"
    "}"
    "System.out.println();"
  }
  ()
} 


fun is_some(r: Option): Bool => match r {
  Option.Some(x) -> true
  _ -> false
}

fun main(): Int => {
  val r = Option.None 
  if is_some(r) then println("Some!!") else println("None!!!")
  0
}
