use pkg_compile_time::*;
use std::io::Write;
use std::{fs, io};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

static VERSION: &str = env!("CARGO_PKG_VERSION");
static AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

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

    /// Selects the compilation target
    pub target: Target,

    /// Don't delete the temporary files
    pub emit: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Target {
    Java,
    Unknown(String),
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
            target: Target::Java,
            emit: false,
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

    fn set_emit(&mut self, v: bool) {
        self.emit = v;
    }

    fn init(&mut self) {
        let _green = Some(Color::Green);
        let _red = Some(Color::Red);
        let yellow = Some(Color::Yellow);
        let gray = Some(Color::Rgb(150, 150, 150));
        let white = Some(Color::White);

        let mut stdout = StandardStream::stdout(ColorChoice::Always);

        let init_message = "Create new SMLL project";
        let init_message = init_message.to_string();

        stdout.set_color(ColorSpec::new().set_fg(yellow)).unwrap();
        writeln!(&mut stdout, "{}", init_message).unwrap();

        stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();
        write!(&mut stdout, "Enter project name: ").unwrap();

        stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
        writeln!(&mut stdout, "[Default: Project]: ").unwrap();

        let mut entry = String::new();
        io::stdin().read_line(&mut entry).unwrap();
        let entry = if &entry == "\n" {
            "Project".to_owned()
        } else {
            entry.trim().to_owned()
        };
        let project_config_path = "./project.toml";
        let project_text = format!(
            r#"
[project]
name = "{entry}"
version = "0.1.0"
authors = ["You"]
edition = "2024"

[depends]

"#
        );

        fs::write(project_config_path, project_text).unwrap();
        let project_src = "./code/main.smll";
        let code = r#"
(* main.smll - Happy coding *)
fn main(): Unit => ()
"#;
        let _ = fs::create_dir("./code");
        let _ = fs::create_dir("./.smll_deps");
        let _ = fs::create_dir("./.smll_deps/libs");
        fs::write("./.smll_deps/depends", "").unwrap();
        fs::write("./.smll_deps/statics", "").unwrap();
        fs::write("./.smll_deps/jars", "").unwrap();
        fs::write("./.smll_deps/imports", "").unwrap();
        fs::write(project_src, code).unwrap();
        println!("Done creating smll project");
        std::process::exit(0);
    }

    pub fn init_dirs() {
        let _ = fs::create_dir("./code");
        let _ = fs::create_dir("./.smll_deps");
        let _ = fs::create_dir("./.smll_deps/libs");

        let deps = "./.smll_deps/depends";
        if fs::metadata(deps).is_err() {
            fs::write(deps, "").unwrap();
            fs::write("./.smll_deps/statics", "").unwrap();
            fs::write("./.smll_deps/jars", "").unwrap();
            fs::write("./.smll_deps/imports", "").unwrap();
        }
    }
    /// Parse command line arguments and create a config.
    pub fn parse() -> Box<Self> {
        let mut conf = Config::new();
        let name = conf.clone().parse_name();
        conf.program_name = name;
        let len = conf.args.len();

        let red = Some(Color::Red);
        let yellow = Some(Color::Yellow);
        let _gray = Some(Color::Rgb(150, 150, 150));
        let white = Some(Color::White);

        let mut stdout = StandardStream::stdout(ColorChoice::Always);

        if len == 1 {
            stdout.set_color(ColorSpec::new().set_fg(red)).unwrap();
            write!(&mut stdout, "Error: ").unwrap();

            stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();
            write!(&mut stdout, "no command line options passed, try `").unwrap();

            stdout.set_color(ColorSpec::new().set_fg(yellow)).unwrap();
            write!(&mut stdout, "help").unwrap();

            stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();
            writeln!(&mut stdout, "`").unwrap();
            std::process::exit(21);
        }

        let mut c = conf.clone();
        let args = conf.args.get(1..len).unwrap();
        let args = args.to_vec();
        let mut held = false;
        let mut collect_paths = false;
        let mut collect_target = false;
        let mut collect_file = false;
        for arg in args.clone().iter() {
            if collect_file {
                break;
            }

            if !arg.is_empty() {
                match arg.as_str() {
                    "defs" => {
                        c.set_defs(true);
                    }
                    "build" => {
                        c.set_build(true);
                    }
                    "run" => {
                        c.set_run(true);
                    }
                    "ir" => {
                        c.set_ir(true);
                    }
                    "verbose" => {
                        c.set_verbose(true);
                    }
                    "emit" => {
                        c.set_emit(true);
                    }
                    "version" => {
                        c.report(&format!("{} version {VERSION}", c.program_name.clone()));
                    }
                    "help" => {
                        c.report(&c.clone().help());
                    }
                    "init" => {
                        c.init();
                    }
                    "paths" => {
                        held = true;
                        collect_paths = true;
                    }
                    "target" => {
                        held = true;
                        collect_target = true;
                    }
                    _ => {
                        if held {
                            if collect_paths {
                                for path in arg.split(':') {
                                    c.import_paths.push(path.to_string())
                                }
                                held = false;
                                collect_paths = false;
                            }

                            if collect_target {
                                c.target = match arg.as_str() {
                                    "java" => Target::Java,
                                    _ => Target::Unknown(arg.clone()),
                                };
                                held = false;
                                collect_target = false;
                            }
                        } else {
                            c.file = arg.to_string();
                        }
                    }
                }
            } else if arg.as_str() == "-" {
                held = false;
                collect_paths = false;
                collect_file = true;
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
        let date = pkg_compile_date!();
        let time = pkg_compile_time!();

        format!(
            r#"
usage: {} [options] - <file>
    |  {} <file>
    
    [options]:
        init                            Initialize a new project
        build                           Build the project and the dependencies
        run                             Runs the program after compilation is complete
        ir                              Print the ir for the program
        emit                            Save the ir to a file
        paths [path+]                   Paths where imports are loaded from. paths are separed by spaces
        defs                            Prints the names and types of top level statements
        target [java,js]                Selects the target to compile to               
        verbose                         Enables the verbosity of the compiler
        version                         Shows the version of the program
        help                            Prints this help file

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

                        v{VERSION} - Made with <3 and rust - {AUTHOR} 
                      (mfanakagama@gmail.com) - (hexarevision.co.za)
                                       Compiled on 
                                    [{date}  {time}]
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
