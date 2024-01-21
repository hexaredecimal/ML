
(* Fibbonacci *)
fun fib(n: Int): Int =>
  if n == 0 then
      0
  else if n == 1 then
      1
  else
      fib(n-1) + fib(n-2)

fun main(): Unit => {
  let 
    fib9 = fib(9) 
  in fib9 == 34

  ()
}
