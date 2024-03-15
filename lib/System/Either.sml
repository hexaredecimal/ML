
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Io.smll
*                   Created by: Gama Sibusiso
*                   Date: 18-02-2024 
* *)


using System::Intrinsics

enum Either = Left(node: Any) | Right(node: Any) 

fun unwrap_either(n: Either): Any => match n {
  Either.Left(node) -> node
  Either.Right(node) -> node
  _ -> panic("Atempt to unwrap a invalid value of type Either")
}

fun left(left: Either.Left): Any => {
  val (node) = left
  node
}
fun right(right: Either.Right): Any => {
  val (node) = right
  node
}

