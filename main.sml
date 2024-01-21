

fun println(...): Result => {
  java {
    "for (int i = 0; i < var_args.length; i++) {"
    "  System.out.print(var_args[i]);"
    "}"
    "System.out.println();"
  }
  ()
}

fun main(): Unit => {
  val name = "Vincent"  
  println(name)
  ()
}

