use std::{fs, io};
use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};


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

    /// Build the dependencies and the project
    ///
    pub build: bool,

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
            import_paths: vec!["./".to_string(), "./.smll_deps/src/".to_string()],
            args: Config::args(),
            build: false,
            run: false,
            ir: false,
            defs: false,
        }
    }

    fn set_verbose(&mut self, v: bool) {
        self.verbose = v;
    }

    fn set_build(&mut self, v: bool) {
        self.build = v;
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
        let green = Some(Color::Green);
        let red = Some(Color::Red);
        let yellow = Some(Color::Yellow);
        let gray = Some(Color::Rgb(150, 150, 150));
        let white = Some(Color::White);

        let mut stdout = StandardStream::stdout(ColorChoice::Always);

        let init_message = "Create new SMLL project";
        let init_message = format!("{}", init_message);

        stdout.set_color(ColorSpec::new().set_fg(yellow)).unwrap();
        writeln!(&mut stdout, "{}", init_message).unwrap();

        stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();
        write!(&mut stdout, "Enter project name: ").unwrap();

        stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
        writeln!(&mut stdout, "[Default: Project]: ").unwrap();

        // println!("Enter project name: [Default: Project]");
        let mut entry = String::new();
        io::stdin().read_line(&mut entry).unwrap();
        let entry = if &entry == "\n" { "Project".to_owned() } else { entry.trim().to_owned() };
        let project_config_path = "./project.toml";
        let project_text = format!(r#"
[project]
name = "{entry}"
version = "0.1.0"
authors = ["You"]
edition = "2024"

[depends]

"#);
        
        fs::write(project_config_path, project_text).unwrap();
        let project_src = "./code/main.sml";
        let code = r#"
(* main.smll - Happy coding *)
fun main(): Unit => ()
"#;
        let _ = fs::create_dir("./code");
        let _ = fs::create_dir("./.smll_deps");
        fs::write("./.smll_deps/depends", "").unwrap();
        fs::write(project_src, code).unwrap();
        println!("Done creating smll project");
    }

    /// Parse command line arguments and create a config.
    pub fn parse() -> Box<Self> {
        let mut conf = Config::new();
        let name = conf.clone().parse_name();
        conf.program_name = name;
        let len = conf.args.len();
        if len == 1 {
            conf.clone().report("no input file provided!!");
        }

        let mut c = conf.clone();
        let args = conf.args.get(1..len).unwrap();
        let args = args.to_vec();
        let len = args.len();
        let mut held = false;
        let mut collect_paths = false;
        let mut collect_file = false;
        for (index, arg) in args.clone().into_iter().enumerate() {
            if !arg.is_empty() {
                if held {
                    held = false;
                    if collect_paths {
                        collect_paths = false;
                    }
                }
                match arg.as_str() {
                    "defs" => {
                        c = c.clone();
                        c.set_defs(true);
                    }
                    "build" => {
                        c = c.clone();
                        c.set_build(true);
                    }
                    "run" => {
                        c = c.clone();
                        c.set_run(true);
                    }
                    "ir" => {
                        c = c.clone();
                        c.set_ir(true);
                    }
                    "verbose" => {
                        c = c.clone();
                        c.set_verbose(true);
                    }
                    "version" => {
                        c.clone()
                            .report(&format!("{} version 1.0", c.program_name.clone()));
                    }
                    "help" => {
                        c.clone().report(&c.clone().help());
                    }
                    "init" => {
                        c.clone().init();
                    }
                    "paths" => {
                        held = true;
                        collect_paths = true;
                    }
                    d => {
                        c.file = arg;
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
            } else {
            }
        }
        Box::new(c)
    }

    fn args() -> Vec<String> {
        std::env::args().collect()
    }

    fn parse_name(self) -> String {
        let name = self.args.first().unwrap().clone();
        let names_split: Vec<&str> = name.split('/').collect();
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
        init                  Initialize a new project
        build                 Build the project and the dependencies
        run                   Runs the program after compilation is complete
        ir                    Print the ir for the program
        paths [path+]         Paths where imports are loaded from. paths are separed by spaces
        defs                  Prints the names and types of top level statements 
        verbose               Enables the verbosity of the compiler
        version               Shows the version of the program
        help                  Prints this help file

                  ___           ___                                 
                 /  /\         /__/\                                
                /  /:/_       |  |::\                               
               /  /:/ /\      |  |:|:\    ___     ___   ___     ___ 
              /  /:/ /::\   __|__|:|\:\  /__/\   /  /\ /__/\   /  /\
             /__/:/ /:/\:\ /__/::::| \:\ \  \:\ /  /:/ \  \:\ /  /:/
             \  \:\/:/~/:/ \  \:\~~\__\/  \  \:\  /:/   \  \:\  /:/ 
              \  \::/ /:/   \  \:\         \  \:\/:/     \  \:\/:/  
               \__\/ /:/     \  \:\         \  \::/       \  \::/   
                 /__/:/       \  \:\         \__\/         \__\/    
                 \__\/         \__\/

                                         _            _                             
     ___ ___ ___ ___ ___ ___ _____ _____|_|___ ___   | |___ ___ ___ _ _ ___ ___ ___ 
    | . |  _| . | . |  _| .'|     |     | |   | . |  | | .'|   | . | | | .'| . | -_|
    |  _|_| |___|_  |_| |__,|_|_|_|_|_|_|_|_|_|_  |  |_|__,|_|_|_  |___|__,|_  |___|
    |_|         |___|                         |___|            |___|       |___|

                    v1.0 - Made with <3 and rust - Gama Sibusiso

"#,
            name.clone(),
            name
        )
        .trim_start()
        .to_string()
    }

    /// report a Config error
    pub fn report(&self, message: &str) {
        println!("{}", message);
        std::process::exit(1);
    }
}
