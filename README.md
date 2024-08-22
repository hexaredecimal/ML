# Small ML language (SMLL) 

<div align="center"> 
<img 
    alt="larry the smll mascott"
    src="https://github.com/hexaredecimal/ML/blob/master/assets/_bfbc2acb-e0df-470f-8cb4-ff6e3855fb45-removebg-preview.png" 
    width="20%" />
    
<p> A small, experimental, general purpose programming language created for exploring alternative ways of writing applications on the JVM </p>

![example workflow](https://github.com/hexaredecimal/ML/actions/workflows/rust.yml/badge.svg)

[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fgjbae1212%2Fhit-counter&count_bg=%2379C83D&title_bg=%23555555&icon=codeigniter.svg&icon_color=%23E7E7E7&title=Visitiors&edge_flat=false)](https://hits.seeyoufarm.com)


</div>

# What is needed to run this?
> The latest JAVA compiler (with support for java 21 source input)

> A rust compiler

```ml
import System::Io 

fn main(): Unit => println("Hello, world")
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
$ ./target/debug/smll build # Builds the dependencies and the projects main file
$ ./target/debug/smll run   # Run the main program file
```


# Package Management
- SMLL has a built-in package manager which handles dependencies as they are specified in the `project.toml` file 
- To add dependencies simply edit the `project.toml` file and add the following:
```sh
RaySMLL = "0.0.1"
```
- Then run build. The compiler will download and install the dependencies in a folder in the current working directory.


# Todo:
- [X] Package manager
- [ ] `Add command` for adding new dependencies
- [ ] `Clean` command for cleaning the project. (This should cause a full rebuild)
- [ ] Use project version/author info when downloading dependencies 
- [X] Package registry [here](https://smllregistry.github.io) 

# Features:
- [X] Type inference for variables
- [X] Variant enums
- [X] Structs
- [X] Expressions not Statements


# Inspiration:
>>> SMLL is a ml derived language for the JVM which borrows a lot of concepts from languages such as 
Standard ML, Rust and ocaml. 

# Stargazers

<a href="https://star-history.com/#hexaredecimal/ML&Date">
 <picture>
   <source media="(prefers-color-scheme: dark)" srcset="https://api.star-history.com/svg?repos=hexaredecimal/ML&type=Date&theme=dark" />
   <source media="(prefers-color-scheme: light)" srcset="https://api.star-history.com/svg?repos=hexaredecimal/ML&type=Date" />
   <img alt="Star History Chart" src="https://api.star-history.com/svg?repos=hexaredecimal/ML&type=Date" />
 </picture>
</a>



