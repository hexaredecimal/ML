use std::{fs, process::Command};
use std::io::{self, Write};

use smll_lang::{config::Config, error::Result};


static HEAD: &'static str = r#"
interface Block<T> {
    T run(); 
}

enum Void {
    Unit
}
"#;

static IMPORTS: &'static str = 
r#"
import java.io.*;
import java.util.*;
"#;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<()> {
    let conf = Config::parse();
    if conf.file == "".to_string() {
        conf.clone().report(format!("no input files provided"));
    }

    let syspath = "./sys/";
    let conv = format!("{syspath}SystConv.java");

    let convs = fs::read_to_string(conv).expect("failed to real sys file");


    let lambdas = generate_lambdas(20); 
    let head2 = format!("{}\n{}\n\n{}{}\n", IMPORTS, lambdas, convs, HEAD);
    let res = smll_lang::compile_and_run(conf.clone())?;
    let program = format!("{head2}\n{}", res);
    let out_name = make_output_file_name(conf.file);
    if conf.ir {
        println!("{}", res);
    } else {
        println!("Writing ir to file: `{out_name}.java`");
        fs::write(format!("./{}.java", out_name), program.as_bytes()).unwrap();
        let mut cmd = Command::new("javac"); 
        let cmd = cmd
            .arg("--enable-preview")
            .arg("--source")
            .arg("21")
            .arg("-d")
            .arg("./build")
            .arg(format!("./{}.java", out_name));

        let output = cmd
            .output()
            .expect(format!("failed to execute command {:?}", cmd).as_str());

        let status = output.status.code().unwrap(); 

        if status != 0 {
            io::stdout().write_all(&output.stdout).unwrap();
            io::stderr().write_all(&output.stderr).unwrap();
            println!("process exited with status: {}", output.status);
        } else {
            println!("process exited with status: {}", output.status);
        }

    }

    if conf.ir == false && conf.run {
        let mut cmd = Command::new("java"); 
        let cmd = cmd
            .arg("--enable-preview")
            .arg("-classpath")
            .arg("./build")
            .arg(format!("{}", out_name));

        let output = cmd
            .output()
            .expect(format!("failed to execute command {:?}", cmd).as_str());

        let status = output.status.code().unwrap(); 

        io::stdout().write_all(&output.stdout).unwrap();
        io::stderr().write_all(&output.stderr).unwrap();

    }
    Ok(())
}

fn generate_lambdas(max: i32) -> String {
    
    let ret = "R"; 
    let ty = "T"; 
    let mut lambdas: Vec<String> = Vec::new(); 
    for i in 0..max {
        let mut vs: Vec<String> = Vec::with_capacity(i as usize);
        let mut vy: Vec<String> = Vec::with_capacity(i as usize);
        for j in 0..i  {
            vs.push(format!("{ty}{j}")); 
            vy.push(format!("{ty}{j} o_{i}_{j}")); 
        }

        let input_generics = vs.join(", "); 
        let method_generics = vy.join(", "); 

        let generics = format!("{ret}{}", if vs.len() >= 1 { format!(", {input_generics}") } else { "".to_string() }); 
        let method_generics = if vy.len() >= 1 { format!("{method_generics}") } else { "".to_string() }; 

        let method = format!("\t{ret} apply({method_generics});");
        let interface = format!("interface Lambda_{} <{generics}> {}\n{}\n{}", i, "{", method, "}");
        lambdas.push(interface);
    }
    
    let lines = "=".repeat(20); 
    let comment = format!("//{lines}This section is generated by the compiler{lines}"); 
    format!("{comment}\n{}\n{comment}\n", lambdas.join("\n"))
}

fn make_output_file_name(file_name: String) -> String {
    if file_name.ends_with(".sml") == false {
        println!(
            "Invalid input file. expected a file with `.sml` extension but found `{}`",
            file_name.clone()
        );
        std::process::exit(1);
    }

    let splits: Vec<_> = file_name.split(".sml").collect();

    let out_file = splits.first().unwrap();
    let out_file = out_file.to_string();
    let out_file = out_file.to_lowercase();


    let splits: Vec<_> = out_file.split("/").collect();

    let name = if splits.len() > 1 {
        splits.last().unwrap().to_string()
    } else {
        out_file.clone() 
    };

    let len = name.len();
    let first = name.get(0..1).unwrap();
    let mut first = first.to_uppercase();
    let rest = name.get(1..len).unwrap();
    first.push_str(rest);

    format!("{}", first)
}
