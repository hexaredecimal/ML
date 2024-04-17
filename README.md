# Small ML language (SMLL) 

<div align="center"> 
<img 
    alt="larry the smll mascott"
    src="https://github.com/hexaredecimal/ML/blob/master/assets/_bfbc2acb-e0df-470f-8cb4-ff6e3855fb45-removebg-preview.png" 
    width="20%" />
    
<p> A small, experimental, general purpose programming language created for exploring alternative ways of writing applications on the JVM </p>

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
$ ./target/debug/smll init  # Initialize a new project in the current directory and creates a `project.toml` and a directory named code
```
edit the newly created `project.toml`
and add the following under `[depends]`
```
MlIo = "0.0.1"
```
save and return to your terminal.

```
$ ./target/debug/smll build # Builds the dependencies and the projects main file
$ ./target/debug/smll run   # Run the main program file
```

# Todo:
- [X] Package manager
- [ ] `Add command` for adding new dependencies
- [ ] `Clean` command for cleaning the project. (This should cause a full rebuild)
- [ ] Use project version/author info when downloading dependencies 
- [X] Package registry [here](https://smllregistry.github.io) 

# Features:
- Type inference for variables
- Variant enums
- structs
- Expressions not Statements

# Inspiration:
>>> SMLL is a ml derived language for the JVM which borrows a lot of concepts from languages such as 
Standard ML, Rust and Kotlin. 
