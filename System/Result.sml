(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Result.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)

using System::Intrinsics

enum Result = Ok(value: Any) | Err(msg: String)

fun is_ok(x: Result): Bool => match x {
  Result.Ok(value) -> true
  _ -> false
}

fun is_err(x: Result): Bool => is_ok(x) == false

fun unwrap_result(x: Result): Any => match x {
  Result.Ok(value) -> value
  Result.Err(msg) -> panic("Attempt to unwrap value of type `Result.Err` with message: `" + msg + "`")
  _ -> ()
}


