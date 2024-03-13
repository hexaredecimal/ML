use std::io;

/// Configuration struct for command line arguments
#[derive(Debug, Clone)]
pub struct Config {
    /// The name of the program for usage when printing help information
    ///
    pub program_name: String,

    /// The name of the file assumed to contain the entry point
    ///
    pub file: String,

    /// The verbosity of the compiler. This is used to toggle debug information while the compiler
    /// is running
    ///
    pub verbose: bool,

    /// This is a list of paths that are used to lookup imports
    ///
    pub import_paths: Vec<String>,

    /// A copy of all the command line args to be parsed
    ///
    pub args: Vec<String>,

    /// Run code after JIT compilation
    ///
    pub run: bool,

    /// Print the ir after compilation
    ///
    pub ir: bool,

    /// Print module definitions
    ///
    pub defs: bool,
}

impl Config {
    fn new() -> Self {
        Self {
            program_name: String::new(),
            file: String::new(),
            verbose: false,
            import_paths: Vec::new(),
            args: Config::args(),
            run: false,
            ir: false,
            defs: false,
        }
    }

    fn set_verbose(&mut self, v: bool) {
        self.verbose = v;
    }

    fn set_run(&mut self, v: bool) {
        self.run = v;
    }

    fn set_ir(&mut self, v: bool) {
        self.ir = v;
    }

    fn set_defs(&mut self, v: bool) {
        self.defs = v;
    }

    fn init(&mut self) {
        let init_message = r#"
    Zulu programming language project setup 
        "#;

        println!("{}", init_message);
        println!("Enter entry point (default: Main.zulu): ");
        let entry = io::read_to_string(io::stdin()).unwrap();
        println!("{:?}", entry);
    }

    /// Parse command line arguments and create a config.
    pub fn parse() -> Box<Self> {
        let mut conf = Config::new();
        let name = conf.clone().parse_name();
        conf.program_name = name;
        let len = conf.args.len();
        if len == 1 {
            conf.clone().report("no input file provided".to_string());
        }

        let mut c = conf.clone();
        let args = conf.args.get(1..len).unwrap();
        let args = args.to_vec();
        let len = args.len();
        let mut held = false;
        let mut collect_paths = false;
        let mut collect_file = false;
        for (index, arg) in args.clone().into_iter().enumerate() {
            if arg.starts_with("--") {
                if held {
                    held = false;
                    if collect_paths {
                        collect_paths = false;
                    }
                }
                match arg.as_str() {
                    "--defs" => {
                        c = c.clone();
                        c.set_defs(true);
                    }
                    "--run" => {
                        c = c.clone();
                        c.set_run(true);
                    }
                    "--ir" => {
                        c = c.clone();
                        c.set_ir(true);
                    }
                    "--verbose" => {
                        c = c.clone();
                        c.set_verbose(true);
                    }
                    "--version" => {
                        c.clone()
                            .report(format!("{} version 0.5.1", c.program_name.clone()));
                    }
                    "--help" => {
                        c.clone().report(c.clone().help().to_string());
                    }
                    "--init" => {
                        c.clone().init();
                    }
                    "--paths" => {
                        held = true;
                        collect_paths = true;
                    }
                    d => {
                        c.clone()
                            .report(format!("invalid argument `{}` provided", d));
                    }
                }
            } else if arg.as_str() == "-" {
                held = false;
                collect_paths = false;
                collect_file = true;
            } else if index < len && held {
                if collect_paths {
                    // WTF? What was I thinking Here???
                    // TODO: Fix this mess as soon as possible, but for not it works so it it no
                    // high priority
                    c = c.clone();
                    c.import_paths.push(arg);
                    c = c.clone();
                }
            } else if collect_file {
                c.file = arg;
            } else {
                c.file = arg;
            }
        }
        Box::new(c)
    }

    fn args() -> Vec<String> {
        std::env::args().collect()
    }

    fn parse_name(self) -> String {
        let name = self.args.first().unwrap().clone();
        let names_split: Vec<&str> = name.split("/").collect();
        let names_split = names_split.to_vec();
        names_split.last().unwrap().to_string()
    }

    fn help(self) -> String {
        let name = self.parse_name();
        format!(
            r#"
usage: {} [options] - <file>
    |  {} <file>
    
    [options]:
        --run                   Runs the program after compilation is complete
        --ir                    Print the ir for the program
        --paths [path+]         Paths where imports are loaded from. paths are separed by spaces
        --defs                  Prints the names and types of top level statements 
        --verbose               Enables the verbosity of the compiler
        --version               Shows the version of the program
        --help                  Prints this help file

"#,
            name.clone(),
            name
        )
        .trim_start()
        .to_string()
    }

    /// report a Config error
    pub fn report(self, message: String) {
        println!("{}", message);
        std::process::exit(1);
    }
}
