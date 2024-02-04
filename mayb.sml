
enum Maybe = Just(x: Any) | Nothing

fun println(...): Unit => {
  java {
    "for (Object arg: var_args) {"
    "  System.out.print(arg);"
    "}"
    "System.out.println();"
  }
  ()
} 

fun panic(msg: String): Unit => {
  java {
    "Intrinsic.panic(msg);"
  }
  ()
}


fun unwrap(m: Maybe): Any => match m {
  Maybe.Just(x) -> x
  Maybe.Nothing -> panic("Attempt to unwrap value of `Maybe.Nothing`")
  _ -> () (* unreachable *)
}

fun main(): Unit => {
  val v = Maybe.Nothing
  val u = unwrap(v) 
  println("Value: ", u)
  ()
}

