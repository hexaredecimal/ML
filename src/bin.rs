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

    let res = jack_lang::compile_and_run(conf)?;
    println!("{:?}", res);
    Ok(())
}
