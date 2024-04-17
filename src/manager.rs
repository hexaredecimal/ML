use std::{collections::HashMap, fmt::Display, fs};
use git2::Repository; 
use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

pub struct Manager <'a> {
    depends: &'a str,
    path: &'a str,
    pub tobuild: Vec<(String, Vec<String>)>
}


impl <'a> Manager <'a> {
    pub fn new() -> Self {
        Self { depends: "depends", path: "./.smll_deps", tobuild: vec![] }
    }

    fn depends_list(&self, path: &str) -> (String, HashMap<String, String>){
        let project_toml_text = fs::read_to_string(path).unwrap(); 
        let project_toml = tsu::toml_from_str(project_toml_text);

        let project_info = project_toml.get("project").unwrap(); 
        let project_name = project_info.get("name").unwrap();

        let mut deps: HashMap<String, String> = HashMap::new();
        let project_depends = project_toml.get("depends");

        if project_depends.is_some() {
            let project_depends = project_depends.unwrap();
            let values = project_depends.as_table().unwrap();

            for (key, val) in values {
                deps.insert(key.clone(), val.to_string());
            }
        }
        (project_name.as_str().unwrap().to_string(), deps)
    }

    pub fn resolve_dependencies(&mut self) {
        let r = self.process("./project.toml"); 
        println!("{r}");
    }


    fn process(&mut self, path: &str) -> PackageResult {
        let (name, depends) = self.depends_list(path); 
        let deps_count = depends.keys().count();
        // TODO: Rewrite this and donot hard code the registry url, load it from file
        let repos = "https://github.com/smllregistry";
        let lines = repos.lines();

        let green = Some(Color::Green);
        let red = Some(Color::Red);
        let yellow = Some(Color::Yellow);
        let gray = Some(Color::Rgb(150, 150, 150));
        let white = Some(Color::White);

        let mut stdout = StandardStream::stdout(ColorChoice::Always);

        stdout.set_color(ColorSpec::new().set_fg(yellow)).unwrap();
        write!(&mut stdout, "Resolving dependencies for ").unwrap();

        stdout.set_color(ColorSpec::new().set_fg(green)).unwrap();
        writeln!(&mut stdout, "{name}").unwrap();

        let _ = fs::create_dir("./smll_deps/src");
        let mut correct = 0;
        let mut skipped = 0; 
        for line in lines {
            for (key, val) in &depends {
                let url = format!("{line}/{key}");
                let dest = format!("./.smll_deps/src/{key}");
                if fs::metadata(&dest).is_ok() {
                    stdout.set_color(ColorSpec::new().set_fg(yellow)).unwrap();
                    writeln!(&mut stdout, "Skipping {key}").unwrap();
                    skipped += 1;
                    continue;
                }

                let str = fs::read_to_string("./.smll_deps/depends").unwrap();
                let mut lines: Vec<String> = vec![]; 

                for st in str.lines() {
                    let st = st.to_string(); 
                    if st.is_empty() || lines.contains(&st) {
                        continue;
                    }
                    lines.push(st);
                }

                if !lines.contains(&url) {
                    lines.push(url.clone());
                }

                let str = lines.join("\n");
                fs::write("./.smll_deps/depends", str).unwrap();
                let clone = Repository::clone(&url, &dest);

                stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();
                write!(&mut stdout, "Downloading ").unwrap();

                stdout.set_color(ColorSpec::new().set_fg(yellow)).unwrap();
                writeln!(&mut stdout, "{key} - {val}").unwrap();

                match clone {
                    Ok(repo) => {
                        correct += 1;
                        stdout.set_color(ColorSpec::new().set_fg(green)).unwrap();
                        write!(&mut stdout, "Download complete ").unwrap();

                        stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
                        writeln!(&mut stdout, "{key}").unwrap();
                        let inner = format!("{dest}/project.toml");

                        let mut v: Vec<String> = vec![];
                        for element in std::path::Path::new(&dest).read_dir().unwrap() {
                            let path = element.unwrap().path();
                            if let Some(extension) = path.extension() {
                                if extension == "sml" {
                                    v.push(format!("{}", path.to_str().unwrap()));
                                }
                            }
                        }
                        self.tobuild.push((key.clone(), v));
                        self.process(&inner);
                    }, 
                    Err(e) => {
                        println!("Failed to Download {key}");

                        stdout.set_color(ColorSpec::new().set_fg(red)).unwrap();
                        write!(&mut stdout, "Download failed for ").unwrap();

                        stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
                        writeln!(&mut stdout, "{key}").unwrap();
                    }
                }
            }
            if correct == deps_count {
                break;
            }
        }

        stdout.set_color(ColorSpec::new().set_fg(white)).unwrap();
        write!(&mut stdout, "Done resolving dependencies for ").unwrap();

        stdout.set_color(ColorSpec::new().set_fg(gray)).unwrap();
        writeln!(&mut stdout, "[{name}]").unwrap();

        if correct == deps_count {
            return PackageResult::Success;
        } else if skipped == 0 && correct != deps_count {
            return PackageResult::Error(correct as i32, deps_count as i32);
        }

        if skipped > 0 {
            return PackageResult::Skipped(skipped);
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum PackageResult {
    Success, 
    Error(i32, i32), 
    Skipped(i32)
}

impl Display for PackageResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let green = Some(Color::Green);
        let red = Some(Color::Red);
        let yellow = Some(Color::Yellow);
        let white = Some(Color::White);

        let mut stdout = StandardStream::stdout(ColorChoice::Always);

        match self {
            Self::Success => {
                stdout.set_color(ColorSpec::new().set_fg(green)).unwrap();
                write!(&mut stdout, "Downloaded all packaged successfully").unwrap(); 
                writeln!(f, "")
            }
            Self::Error(correct, deps_count) => {
                let err = format!("Failed to get dependencies: {correct} / {deps_count} found");
                stdout.set_color(ColorSpec::new().set_fg(red)).unwrap();
                write!(&mut stdout, "{err}").unwrap(); 
                writeln!(f, "")
            }
            Self::Skipped(skipped) => {
                let err =  format!("{skipped} dependenc{} got skipped", if *skipped == 1 { "y" } else { "ies"} );
                stdout.set_color(ColorSpec::new().set_fg(yellow)).unwrap();
                write!(&mut stdout, "{err}").unwrap(); 
                writeln!(f, "")
            }
        }
    }
} 

