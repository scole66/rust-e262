use clap::Parser;
use color_eyre::eyre::Context;
use color_eyre::eyre::{eyre, Result};
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
use std::process::{Command, Stdio};
use std::str::from_utf8;
use std::str::FromStr;
use yaml_rust::{Yaml, YamlLoader};

#[derive(Debug)]
enum Phase {
    Parse,
    Resolution,
    Runtime,
}
impl FromStr for Phase {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "parse" => Ok(Phase::Parse),
            "resolution" => Ok(Phase::Resolution),
            "runtime" => Ok(Phase::Runtime),
            _ => Err(format!("Bad Phase: {s}")),
        }
    }
}
#[derive(Debug)]
struct Negative {
    phase: Phase,
    error_type: String,
}
#[derive(Debug)]
enum Marker {
    Raw,
    Strict,
    NonStrict,
}
impl fmt::Display for Marker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Marker::Raw => write!(f, "raw"),
            Marker::Strict => write!(f, "strict"),
            Marker::NonStrict => write!(f, "non-strict"),
        }
    }
}
#[derive(Debug)]
struct Source {
    source: String,
    mark: Marker,
}
#[allow(dead_code)]
#[derive(Debug)]
struct TestInfo {
    source: Vec<Source>,
    description: String,
    features: Vec<String>,
    negative: Option<Negative>,
    module: bool,
    async_test: bool,
}

fn construct_test(path: &Path, can_block: bool) -> Result<TestInfo> {
    const METASTART: &str = "/*---";
    const METAEND: &str = "---*/";

    // Load the test file into memory (this file contains the test's metadata, along with the test itself)
    let file = File::open(path)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;

    // Extract the metadata
    let metadata_start_index = contents.find(METASTART).map(|s| s + METASTART.len());
    let metadata_end_index = contents.find(METAEND);
    if let (Some(start), Some(end)) = (metadata_start_index, metadata_end_index) {
        let yaml = &contents[start..end].replace("\r\n", "\n").replace('\r', "\n");
        let metadata = YamlLoader::load_from_str(yaml)?;
        if metadata.len() != 1 {
            return Err(eyre!("Badly formed test metadata (too many or zero yaml documents)"));
        }
        let metadata = &metadata[0];

        if !matches!(metadata, &Yaml::Hash(..)) {
            return Err(eyre!("Badly formed test metadata (not a hash)"));
        }
        let info = metadata.as_hash().unwrap();

        let description = info
            .get(&Yaml::String("description".into()))
            .map_or_else(String::new, |x| x.as_str().unwrap().trim().to_string());
        let includes = info.get(&Yaml::String("includes".into())).map_or_else(Vec::new, |x| {
            x.as_vec().unwrap().iter().map(|item| item.as_str().unwrap().to_string()).collect::<Vec<_>>()
        });
        let features = info.get(&Yaml::String("features".into())).map_or_else(Vec::new, |x| {
            x.as_vec().unwrap().iter().map(|item| item.as_str().unwrap().to_string()).collect::<Vec<_>>()
        });
        let negative = info.get(&Yaml::String("negative".into())).map(|item| item.as_hash().unwrap()).map(|hash| {
            let phase = hash.get(&Yaml::String("phase".into())).unwrap().as_str().unwrap().parse::<Phase>().unwrap();
            let error_type = hash.get(&Yaml::String("type".into())).unwrap().as_str().unwrap().to_string();
            Negative { phase, error_type }
        });
        let mut flag_only_strict = false;
        let mut flag_no_strict = false;
        let mut flag_module = false;
        let mut flag_raw = false;
        let mut flag_async = false;
        let mut flag_can_block_is_false = false;
        let mut flag_can_block_is_true = false;
        if let Some(flags) = info.get(&Yaml::String("flags".into())) {
            for item in flags.as_vec().unwrap() {
                let flag = item.as_str().unwrap();
                match flag {
                    "onlyStrict" => flag_only_strict = true,
                    "noStrict" => flag_no_strict = true,
                    "module" => flag_module = true,
                    "raw" => flag_raw = true,
                    "async" => flag_async = true,
                    "CanBlockIsFalse" => flag_can_block_is_false = true,
                    "CanBlockIsTrue" => flag_can_block_is_true = true,
                    _ => (),
                }
            }
        }

        let mut source = vec![];

        if !(can_block && flag_can_block_is_false || !can_block && flag_can_block_is_true) {
            if flag_raw {
                source.push(Source { source: contents, mark: Marker::Raw });
            } else {
                for strict in [true, false] {
                    if (!strict && flag_only_strict) || (strict && flag_no_strict) {
                        continue;
                    }
                    let mut test_source = String::new();
                    if strict {
                        test_source.push_str("\"use strict\";\n");
                    }
                    test_source.push_str(&load_harness_file("assert.js")?);
                    test_source.push_str(&load_harness_file("sta.js")?);
                    if flag_async {
                        test_source.push_str(&load_harness_file("doneprintHandle.js")?);
                    }
                    for item in &includes {
                        test_source.push_str(&load_harness_file(item)?);
                    }
                    test_source.push_str(&contents);
                    source.push(Source {
                        source: test_source,
                        mark: if strict { Marker::Strict } else { Marker::NonStrict },
                    });
                }
            }
        }

        Ok(TestInfo { description, source, features, negative, module: flag_module, async_test: flag_async })
    } else {
        Ok(TestInfo {
            source: vec![Source { source: contents, mark: Marker::Raw }],
            description: String::new(),
            features: vec![],
            negative: None,
            module: false,
            async_test: false,
        })
    }
}

fn load_harness_file(filename: &str) -> Result<String> {
    const HARNESS_ROOT: &str = "/home/scole/rustplay/test262/harness";
    //const HARNESS_ROOT: &str = "/Users/scole/fun/test262/harness";
    let path = Path::new(HARNESS_ROOT).join(filename);
    let file = File::open(&path).context(format!("Opening {}", path.to_string_lossy()))?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(contents)
}

enum Status {
    Pass,
    Fail,
}
impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Status::Pass => "PASS",
                Status::Fail => "FAIL",
            }
        )
    }
}

#[derive(Parser, Debug, Default)]
struct Arguments {
    path: String,
    #[arg(short, long, id = "OUTPATH")]
    keep_constructed: Option<String>,
}

fn main() -> Result<()> {
    let ignored_features = ["caller", "async", "cross-realm", "class"];
    color_eyre::install()?;

    let args = Arguments::parse();
    let test_name = args.path;
    let info = construct_test(Path::new(&test_name), false)?;

    if !ignored_features.iter().map(ToString::to_string).any(|f| info.features.contains(&f)) {
        for source in &info.source {
            if let Some(path) = args.keep_constructed.as_ref() {
                let output_path = Path::new(path).join(format!("{}.js", source.mark));
                let mut file = File::create(output_path)?;
                file.write_all(source.source.as_bytes())?;
            }

            let mut child = Command::new("target/release/res")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .arg("/dev/stdin")
                .spawn()?;
            let mut stdin = child.stdin.take().expect("stdin should exist on child");
            let source_text = source.source.clone();
            let jh =
                std::thread::spawn(move || stdin.write_all(source_text.as_bytes()).expect("writing should be ok?"));
            let result = child.wait_with_output()?;
            jh.join().expect("join should succeed");
            let finished_ok = result.status.success();
            let test_status = if finished_ok {
                let stdout = from_utf8(result.stdout.as_slice())?;
                let final_line = stdout.lines().last().unwrap_or("");
                if let Some(Negative { phase, error_type }) = &info.negative {
                    match phase {
                        Phase::Parse => {
                            let expected = format!("During compilation: [{error_type}: ");
                            if final_line.starts_with(&expected) {
                                Status::Pass
                            } else {
                                Status::Fail
                            }
                        }
                        Phase::Resolution => {
                            let expected = format!("During resolution: [{error_type}: ");
                            if final_line.starts_with(&expected) {
                                Status::Pass
                            } else {
                                Status::Fail
                            }
                        }
                        Phase::Runtime => {
                            let expected = format!("Thrown: {error_type}: ");
                            if final_line.starts_with(&expected) {
                                Status::Pass
                            } else {
                                Status::Fail
                            }
                        }
                    }
                } else if final_line.starts_with("Thrown: ") {
                    Status::Fail
                } else {
                    Status::Pass
                }
            } else {
                Status::Fail
            };
            println!("{test_status}: {test_name} -- {}", source.mark);
        }
    }

    Ok(())
}
