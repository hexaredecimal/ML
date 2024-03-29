
# Small ML language (SMLL) 

<div align="center"> 
<img 
    alt="larry the smll mascott"
    src="https://github.com/hexaredecimal/ML/blob/master/assets/_bfbc2acb-e0df-470f-8cb4-ff6e3855fb45-removebg-preview.png" 
    width="20%" />
    
<p> A small, experimental, general purpose programming language created for exploring alternative ways of writing applications on the JVM </p>
<h2>NOTE</h2>
<p style="color:red"><b>Native backends are being developed, due to the performance of the JVM</b></p>

![example workflow](https://github.com/hexaredecimal/ML/actions/workflows/rust.yml/badge.svg)
    
</div>



# What is needed to run this?
> The latest JAVA compiler (with support for java 21 source input)

> A rust compiler


```ml
using System::Io

fun main(): Unit => println("Hello, world")
```

# How to compile?
> After downloading and installing the required software, execute the following commands


```sh
$ git clone depth=1 https://github.com/hexaredecimal/ML.git
$ cd ML
$ cargo build
```

> After running these commands you should now have a target folder on the root of the project, 
then execute the following

```sh
$ ./target/debug/smll --run main.sml                                    # This compiles the main.sml file and runs it with java
```
or

```sh
$ ./target/debug/smll --help                                            # This prints the help file
```

# Features:
- Type inference for variables
- Variant enums
- structs 
- No Implicit OOP (No inheritence, no interfaces, only data and data alone)
- Expressions not Statements


# Inspiration:

>>> SMLL is a ml derived language for the JVM which borrows a lot of concepts from languages such as 
Standard ML, Rust and Kotlin. 


