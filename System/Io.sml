
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Io.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)


fun print(...): Unit => {
  java {
    "for (int i = 0; i < var_args.length; i++) {"
    "  System.out.print(var_args[i]); "
    "}"
  }
  ()
}
 

fun println(...): Unit => {
  java {
    "for (int i = 0; i < var_args.length; i++) {"
    "  System.out.print(var_args[i]); "
    "}"
    "System.out.println(); "
  }
  ()
}

fun printf(fmt: String, ...): Unit => {
  java {
    "System.out.printf(fmt, var_args);"
  }
  ()
}

