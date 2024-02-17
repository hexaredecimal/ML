
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Array.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)

fun length(array: List[Any]): Int => {
  (* Todo implement this function without relying on JAVA *)
  val n = 0
  java {
    "n = array.length;"
  }
  n
}

fun array_to_string(array: List[Any]): String => {
  val s = ""
  java {
    "String arr = new String();"
    "arr += '[';"
    "for (int i = 0; i < array.length; i++) {"
    " arr += array[i];"
    " if (i < array.length - 1)"
    "    arr += ',';"
    "}"
    "arr += ']';"
    "s = arr;"
  }
  s
}

fun tail(arr: List[Any]): Any => arr.[length(arr) - 1]

fun head(arr: List[Any]): Any => arr.[0]

fun rest(arr: List[Any]): List[Any] => {
  val ret = []
  val len = length(arr)
  java {
    "Object[] aa = new Object[len-1];"
    "for (int i = 0; i < len-1; i++) {"
    " aa[i] = arr[i + 1];"
    "}"
    "ret = aa;"
  }
  ret
}

