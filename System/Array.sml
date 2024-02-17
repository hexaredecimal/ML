

fun length(array: List[Any]): Int => {
  (* Todo implement this function without relying on JAVA *)
  val n = 0
  java {
    "n = array.length;"
  }
  n
}

fun array_to_str(array: List[Any]): String => {
  val s = ""
  java {
    "String arr = new String();"
    "for (int i = 0; i < array.length; i++) {"
    " arr += array[i];"
    " if (i < array.length - 1)"
    "    arr += ',';"
    "}"
    "s = arr;"
  }
  s
}

