# ML (code name)

```ml

fun fact(x: Int): Int => match x {
    1 -> 1
    _ -> x * fact(x - 1)
}

fun println(ln: String): Unit => {
    java {
        "System.out.println(ln);"
    }
    ()
}

fun main(): Unit => {
    val answer = fact(5)
    println("The factorial of 5 is = " + answer)
}

```
