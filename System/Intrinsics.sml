
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Intrinsics.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)

using System::Escape (* For colored error messages *)

fun panic(msg: String): Unit => {
  val prefix = fg_red("Panic: ")
  val _msg = underline(msg) 
  java {
    "Intrinsic.panic(prefix + _msg);"
  }
  ()
}

fun todo(msg: String): Unit => {
  val prefix = fg_yellow(underline("TODO: "))
  val _msg = underline(msg) 
  java {
    "Intrinsic.panic(prefix + _msg);"
  }
  ()
}

fun unreachable(): Unit => {
  val prefix = bg_red(fg_white("Unreachable"))
  java {
    "Intrinsic.panic(prefix);"
  }
  ()
}


