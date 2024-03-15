
fun author(): String => "Gama Sibusiso"
fun version(): String => "0.5.1"
fun lang(): String => "Small ML Language"
fun help(): String => 
"usage: smll [options] - <file>
    |  smll <file>
    
    [options]:
        --run                   Runs the program after compilation is complete
        --ir                    Print the ir for the program
        --paths [path+]         Paths where imports are loaded from. paths are separed by spaces
        --defs                  Prints the names and types of top level statements 
        --verbose               Enables the verbosity of the compiler
        --version               Shows the version of the program
        --help                  Prints this help file
"

