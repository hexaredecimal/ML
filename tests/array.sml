
using System::Io
using System::Array

fun main(): Unit => {
  val array = [1,2,3,4,5,6,7,8,9,10]
  println("Memory location: " + array_to_string(array))
  println("Head: ", array.[0])
  ()
}

