
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Input.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)


fun readString(): String => {
  val s = null of String
  java {
    "s = new Scanner(System.in).next();"
  }
  s
} 


fun readLine(): String => {
  val s = null of String
  java {
    "s = new Scanner(System.in).nextLine();"
  }
  s
} 

fun readInt(): Int => {
  val s = 0
  java {
    "s = new Scanner(System.in).nextInt();"
  }
  s
} 

fun readDouble(): Double => {
  val s = 0 as Double
  java {
    "s = new Scanner(System.in).nextDouble();"
  }
  s
}

fun readChar(): Char => {
  val s = null of Char
  java {
    "s = new Scanner(System.in).next().charAt(0);"
  }
  s
} 

