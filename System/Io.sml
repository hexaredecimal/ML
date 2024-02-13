
(* Output routines lifted from java *)

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

