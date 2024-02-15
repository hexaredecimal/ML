
fun panic(msg: String): Unit => {
  val prefix = "Panic: "
  java {
    "Intrinsic.panic(prefix + msg);"
  }
  ()
}

fun todo(msg: String): Unit => {
  val prefix = "TODO: "
  java {
    "Intrinsic.panic(prefix + msg);"
  }
  ()
}

fun unreachable(): Unit => {
  val prefix = "Unreachable"
  java {
    "Intrinsic.panic(prefix);"
  }
  ()
}


