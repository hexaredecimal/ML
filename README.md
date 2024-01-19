# Jack

Jack is a compiled, statically typed, pure-functional programming language inspired by ML.

This repository hosts a 3-step compiler written in Rust that does parsing, semantic analysis (including type checking) and generates Cranelift IR.

Here's an example of a Jack program you can run using `cargo run fib.ml`
```ml
fun fib(n: int): int =
    if n == 0 then
        0
    else if n == 1 then
        1
    else
        fib(n-1) + fib(n-2)

fun main(): bool = let fib9 = fib(9) in fib9 == 34
```
