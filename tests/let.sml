fun main(): Bool => 
  (let a = 10 in a == 10) && 
    (let b = 20 in 
      (let c = 30 in 
        (let c = 20 in 
          (b == c))))
