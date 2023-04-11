#![allow(dead_code)]

use color_eyre::eyre::{eyre, Result};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
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
struct TestInfo {
    source: Vec<String>,
    description: String,
    features: Vec<String>,
    negative: Option<Negative>,
    module: bool,
    async_test: bool,
}

fn construct_test(path: &Path, can_block: bool) -> Result<TestInfo> {
    // Load the test file into memory (this file contains the test's metadata, along with the test itself)
    let file = File::open(path)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;

    // Extract the metadata
    const METASTART: &str = "/*---";
    const METAEND: &str = "---*/";

    let metadata_start_index = contents.find(METASTART).map(|s| s + METASTART.len());
    let metadata_end_index = contents.find(METAEND);
    if let (Some(start), Some(end)) = (metadata_start_index, metadata_end_index) {
        let metadata = YamlLoader::load_from_str(&contents[start..end])?;
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
            .map(|x| x.as_str().unwrap().to_string())
            .unwrap_or_else(String::new);
        let includes = info
            .get(&Yaml::String("includes".into()))
            .map(|x| x.as_vec().unwrap().iter().map(|item| item.as_str().unwrap().to_string()).collect::<Vec<_>>())
            .unwrap_or_else(Vec::new);
        let features = info
            .get(&Yaml::String("features".into()))
            .map(|x| x.as_vec().unwrap().iter().map(|item| item.as_str().unwrap().to_string()).collect::<Vec<_>>())
            .unwrap_or_else(Vec::new);
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
            for item in flags.as_vec().unwrap().iter() {
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
                source.push(contents);
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
                    for item in includes.iter() {
                        test_source.push_str(&load_harness_file(item)?);
                    }
                    test_source.push_str(&contents);
                    source.push(test_source);
                }
            }
        }

        Ok(TestInfo { description, source, features, negative, module: flag_module, async_test: flag_async })
    } else {
        Ok(TestInfo {
            source: vec![contents],
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
    let path = Path::new(HARNESS_ROOT).join(filename);
    let file = File::open(path)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args: Vec<String> = env::args().collect();
    let test_name = &args[1];
    let out_dir = &args[2];
    let info = construct_test(Path::new(test_name), false)?;

    for (idx, source) in info.source.iter().enumerate() {
        let description = &info.description;
        println!("Test-262: {test_name} -- {description}");

        let base = Path::new(test_name).file_stem().unwrap().to_str().unwrap();
        let output_path = Path::new(out_dir).join(format!("{base}-{idx}.js"));
        let mut file = File::create(output_path)?;
        file.write_all(source.as_bytes())?;
    }

    //println!("{info:#?}");
    Ok(())
}
