
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Intrinsics.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)

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


