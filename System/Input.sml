
(*
*    =========================== SMLL Standard Libary =====================================
*                   File: System/Input.smll
*                   Created by: Gama Sibusiso
*                   Date: 17-02-2024 
* *)


fun readString(): String => {
  val s = ""
  java {
    "s = new Scanner(System.in).next();"
  }
  s
} 

fun readInt(): String => {
  val s = 0
  java {
    "s = new Scanner(System.in).nextInt();"
  }
  s
} 

fun readDouble(): Double => {
  val s = 0
  java {
    "s = new Scanner(System.in).nextDouble();"
  }
  s
}

fun readChar(): String => {
  val s = '_'
  java {
    "s = new Scanner(System.in).next().charAt(0);"
  }
  s
} 

