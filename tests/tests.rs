use jack_lang::compile_and_run;
use jack_lang::config::Config;

#[test]
fn fib_test() {
    let mut conf = Config::parse();
    conf.file = "fib.sml".to_string();

    let result = compile_and_run(conf).unwrap();
    assert_eq!(result, 1);
}

#[test]
fn comments_test() {
    let mut conf = Config::parse();
    conf.file = "comments.sml".to_string();
    assert_eq!(compile_and_run(conf).unwrap(), 0);
}

#[test]
fn float_test() {
    let mut conf = Config::parse();
    conf.file = "float.sml".to_string();
    assert_eq!(compile_and_run(conf).unwrap(), 1);
}

#[test]
fn simple_test() {
    let mut conf = Config::parse();
    conf.file = "simple.sml".to_string();
    assert_eq!(compile_and_run(conf).unwrap(), 1);
}

#[test]
fn operations_test() {
    let mut conf = Config::parse();
    conf.file = "operations.sml".to_string();
    assert_eq!(compile_and_run(conf).unwrap(), 1);
}

#[test]
fn let_test() {
    let mut conf = Config::parse();
    conf.file = "let.sml".to_string();
    assert_eq!(compile_and_run(conf).unwrap(), 1);
}

#[test]
fn array_test() {
    let mut conf = Config::parse();
    conf.file = "array.sml".to_string();
    assert_eq!(compile_and_run(conf).unwrap(), 42);
}
