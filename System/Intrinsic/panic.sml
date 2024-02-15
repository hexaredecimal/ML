

fun panic(msg: String): Unit => {
  java {
    "Intrinsic.panic(msg);"
  }
  ()
}


