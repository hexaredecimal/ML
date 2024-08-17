use rust_embed::RustEmbed;
use smll_lang::{config::Config, error::Result, manager::Manager};
use std::io::{self, Write};
use std::{fs, process::Command};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

#[derive(RustEmbed)]
#[folder = "sys/"]
struct Asset;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<()> {
    let mut conf = Config::parse();
    let data_conv = Asset::get("SystConv.java").unwrap();
    let convs = std::str::from_utf8(data_conv.data.as_ref()).unwrap();

    let green = Some(Color::Green);
    let red = Some(Color::Red);
    let _yellow = Some(Color::Yellow);
    let gray = Some(Color::Rgb(150, 150, 150));
    let white = Some(Color::White);

    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    Config::init_dirs();

    let statics = fs::read_to_string("./.smll_deps/statics").unwrap();
    let imports = fs::read_to_string("./.smll_deps/imports").unwrap();
    let jars = fs::read_to_string("./.smll_deps/jars").unwrap();

    let imports = format!("{statics}\n\n{imports}\n\n");

    let lambdas = generate_lambdas(20);
    let structures = create_internal_structures();
    let head2 = format!("{imports}\n{lambdas}\n{convs}\n{structures}\n");
    let mut package_manager = Manager::new();
    conf.file = "./code/main.smll".to_string();
    if conf.ir {
        let res = smll_lang::compile_and_run(&conf)?;
        println!("{imports}\n{res}");
    } else if conf.build {
        package_manager.resolve_dependencies();

        if !package_manager.tobuild.is_empty() {
            for (package, files) in &package_manager.tobuild {
                stdout.set_color(ColorSpec::new().set_fg(green)).unwrap();
                write!(&mut stdout, "Compiling ").unwrap();

                stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
                writeln!(&mut stdout, "[{package}] ").unwrap();
                let mut done = 0;
                for file in files {
                    conf.file = file.to_string();
                    let res = smll_lang::compile_and_run(&conf);
                    if res.is_err() {
                        done += 1;
                    }
                    // let program = format!("{head2}\n{}", res);
                    // let out_name = make_output_file_name(&conf.file);
                    // compile_to_java(&out_name, &program);
                }

                if done != 0 {
                    stdout.set_color(ColorSpec::new().set_fg(red)).unwrap();
                    write!(&mut stdout, "Builing failed: ").unwrap();

                    stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
                    write!(&mut stdout, "{done} / {}", files.len()).unwrap();

                    stdout.set_color(ColorSpec::new().set_fg(red)).unwrap();
                    writeln!(&mut stdout, " built. ").unwrap();
                    return Ok(());
                } else {
                    stdout.set_color(ColorSpec::new().set_fg(green)).unwrap();
                    write!(&mut stdout, "Done compiling ").unwrap();

                    stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
                    writeln!(&mut stdout, "[{package}] ").unwrap();
                }
            }
        }

        stdout
            .set_color(ColorSpec::new().set_fg(Some(Color::Rgb(255, 255, 255))))
            .unwrap();
        conf.file = "./code/main.smll".to_string();
        let res = smll_lang::compile_and_run(&conf)?;
        let program = format!("{head2}\n{}", res);
        let out_name = make_output_file_name(&conf.file);
        compile_to_java(&out_name, &program, jars.trim(), conf.emit);
    } else if conf.run {
        let res = smll_lang::compile_and_run(&conf)?;
        let program = format!("{head2}\n{}", res);
        let out_name = make_output_file_name(&conf.file);
        compile_to_java(&out_name, &program, jars.trim(), conf.emit);

        stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();
        let mut cmd = Command::new("java");
        let mut classes = vec!["./build"];
        if !jars.is_empty() {
            classes.push(&jars);
        }

        let jars = classes.join(":");
        let cmd = cmd
            .arg("--enable-preview")
            .arg("--class-path")
            .arg(&jars)
            .arg(out_name);

        writeln!(&mut stdout, "Running: {cmd:?}").unwrap();
        let output = cmd
            .output()
            .unwrap_or_else(|_| panic!("failed to execute command {:?}", cmd));

        let _status = output.status.code().unwrap();

        io::stdout().write_all(&output.stdout).unwrap();
        io::stderr().write_all(&output.stderr).unwrap();
        stdout.set_color(ColorSpec::new().set_fg(green)).unwrap();
        writeln!(&mut stdout, "Done").unwrap();
    } else if conf.defs {
        smll_lang::compile_and_run(&conf)?;
    } else {
        let _ = &conf.report("no input files provided");
    }
    Ok(())
}

fn compile_to_java(out_name: &str, program: &str, class_path: &str, save: bool) {
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    let white = Some(Color::White);
    let red = Some(Color::Red);
    let green = Some(Color::Green);

    stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();

    fs::write(format!("./{}.java", out_name), program.as_bytes()).unwrap();
    let mut cmd = Command::new("javac");
    let cmd = cmd
        .arg("--class-path")
        .arg(class_path.trim())
        .arg("--enable-preview")
        .arg("--source")
        .arg("21")
        .arg("-d")
        .arg("./build")
        .arg(format!("./{}.java", out_name));

    writeln!(&mut stdout, "Compiling: {cmd:?}").unwrap();
    let output = cmd
        .output()
        .unwrap_or_else(|_| panic!("failed to execute command {:?}", cmd));

    if !save {
        fs::remove_file(format!("{out_name}.java")).unwrap();
    }

    let status = output.status.code().unwrap();

    if status != 0 {
        io::stdout().write_all(&output.stdout).unwrap();
        io::stderr().write_all(&output.stderr).unwrap();

        stdout.set_color(ColorSpec::new().set_fg(red)).unwrap();
        writeln!(&mut stdout, "Compiling failed").unwrap();
        std::process::exit(1);
    } else {
        stdout.set_color(ColorSpec::new().set_fg(green)).unwrap();
        writeln!(&mut stdout, "Compiled successfully").unwrap();
    }
}

fn generate_lambdas(max: i32) -> String {
    let ret = "R";
    let ty = "T";
    let mut lambdas: Vec<String> = Vec::new();
    for i in 0..max {
        let mut vs: Vec<String> = Vec::with_capacity(i as usize);
        let mut vy: Vec<String> = Vec::with_capacity(i as usize);
        for j in 0..i {
            vs.push(format!("{ty}{j}"));
            vy.push(format!("{ty}{j} o_{i}_{j}"));
        }

        let input_generics = vs.join(", ");
        let method_generics = vy.join(", ");

        let generics = format!(
            "{ret}{}",
            if !vs.is_empty() {
                format!(", {input_generics}")
            } else {
                "".to_string()
            }
        );
        let method_generics = if !vy.is_empty() {
            method_generics
        } else {
            "".to_string()
        };

        let method = format!("\t{ret} apply({method_generics});");
        let interface = format!(
            "interface Lambda_{} <{generics}> {}\n{}\n{}",
            i, "{", method, "}"
        );
        lambdas.push(interface);
    }

    let lines = "=".repeat(20);
    let comment = format!("//{lines}This section is generated by the compiler{lines}");
    format!("{comment}\n{}\n{comment}\n", lambdas.join("\n"))
}

fn make_output_file_name(file_name: &String) -> String {
    let out_file = file_name.strip_suffix(".smll").unwrap_or_else(|| {
        println!("Invalid file name: {file_name}, expected file with extension .smll");
        std::process::exit(1);
    });

    let name = out_file
        .rsplit('/')
        .next()
        .unwrap_or(out_file)
        .to_lowercase();

    let (first, rest) = name.split_at(1);
    let first = first.to_uppercase();
    format!("{}{}", first, rest)
}

fn create_internal_structures() -> &'static str {
    r#"
    interface Block<T> {
        T run(); 
    }

    enum Void {
        Unit
    }
    "#
}
