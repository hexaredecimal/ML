fun a2(): [Int;3] => [1, 2, 3]

fun main(): Unit => {
  let 
    a1 = [40] 
  in a1.[0] + (a2()).[1]
  ()
}

