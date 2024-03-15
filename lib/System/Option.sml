
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Option.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)


using System::Intrinsics

enum Option = Some(value: Any) | None

fun is_some(x: Option): Bool => match x {
  Option.Some(value) -> true
  _ -> false
}

fun is_none(x: Option): Bool => is_some(x) == false

fun unwrap_option(x: Option): Any => match x {
  Option.Some(value) -> value
  _ -> panic("Attempt to unwrap value of type `Option.None`")
}



