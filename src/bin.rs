use jack_lang::{error::Result, config::Config};
 
fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<()> {
    let conf = Config::parse(); 
    if conf.file == "".to_string() {
        conf.clone().report(format!("no input file provided"));
    }

    let head = 
    r#"
// Intrinsic functions space
//

interface Block<T> {
    T run(); 
}

enum Void {
    Unit
}
    "#; 
    let res = jack_lang::compile_and_run(conf)?;
    println!("{head}\n{}", res);
    Ok(())
}
