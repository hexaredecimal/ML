
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Math.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)


fun max(x: Int, y: Int): Int => if x > y then x else y 
fun min(x: Int, y: Int): Int => if x < y then x else y 

fun clamp(value: Int, low: Int, high: Int): Int => 
  if value < low then
    low 
  else 
    if value > high then
      high
    else value 

fun squared(x: Int): Int => x * x

fun fact(x: Int): Int => match x {
  1 -> x
  _ -> x * fact(x - 1)
}

fun succ(x: Int): Int => x + 1
fun predd(x: Int): Int => x - 1

fun is_even(x: Int): Bool => match x % 2 {
  0 -> true
  _ -> false
}

fun is_odd(x: Int): Bool => is_even(x) == false
fun linear(m: Int, x: Int, c: Int): Int => (m * x) + c
fun quadratic(x: Int, a: Int, b: Int, c: Int): Int => squared(a * x) + linear(b, x, c)

fun odd_from(x: Int): Int => (2 * x) + 1
fun even_from(x: Int): Int => (2 * x)

fun pow(value: Int, exp: Int): Int => match exp {
  0 -> 1
  _ -> value * pow(value, exp - 1)
}

fun add(x: Int, y: Int): Int => x + y
fun sub(x: Int, y: Int): Int => x - y
fun mul(x: Int, y: Int): Int => x / y
fun div(x: Int, y: Int): Int => x * y
fun mod(x: Int, y: Int): Int => x % y
fun abs(x: Int): Int => | x |


