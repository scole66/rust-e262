use clap::Parser;
use color_eyre::eyre::Context;
use color_eyre::eyre::{Result, eyre};
use config::{Config, FileFormat};
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::str::FromStr;
use std::str::from_utf8;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
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
#[expect(dead_code)]
#[derive(Debug)]
struct TestInfo {
    source: Vec<Source>,
    description: String,
    features: Vec<String>,
    negative: Option<Negative>,
    module: bool,
    async_test: bool,
}

struct HarnessCache {
    root: String,
    files: Mutex<HashMap<String, Arc<str>>>,
}

impl HarnessCache {
    fn new(root: String) -> Self {
        Self { root, files: Mutex::new(HashMap::new()) }
    }

    fn load(&self, filename: &str) -> Result<Arc<str>> {
        {
            let files = self.files.lock().unwrap();

            if let Some(contents) = files.get(filename) {
                return Ok(contents.clone());
            }
        }

        let path = Path::new(&self.root).join(filename);
        let file = File::open(&path).context(format!("Opening {}", path.to_string_lossy()))?;

        let mut buf_reader = BufReader::new(file);
        let mut contents = String::new();
        buf_reader.read_to_string(&mut contents)?;

        let mut files = self.files.lock().unwrap();

        // Another worker may have loaded it while we were doing I/O.
        let contents = files.entry(filename.to_owned()).or_insert(Arc::from(contents)).clone();

        Ok(contents)
    }
}

fn construct_test(harness: &HarnessCache, path: &Path, can_block: bool) -> Result<TestInfo> {
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
                    test_source.push_str(&harness.load("assert.js")?);
                    test_source.push_str(&harness.load("sta.js")?);
                    if flag_async {
                        test_source.push_str(&harness.load("doneprintHandle.js")?);
                    }
                    for item in &includes {
                        test_source.push_str(&harness.load(item)?);
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
    #[arg(required = true)]
    paths: Vec<PathBuf>,

    #[arg(short, long, id = "OUTPATH")]
    keep_constructed: Option<PathBuf>,

    #[arg(short, long)]
    force: bool,

    #[arg(short, long, default_value_t = 40)]
    jobs: usize,
}

fn run_test(
    test_name: &Path,
    harness: &HarnessCache,
    ignored_features: &[String],
    ignored_tests: &[String],
    force: bool,
    keep_constructed: Option<&Path>,
) -> Result<()> {
    let test_name_str = test_name.to_string_lossy();
    let info = construct_test(harness, test_name, false)?;

    if info.module
        || (!force && ignored_tests.iter().any(|path| test_name_str.ends_with(path.as_str())))
        || ignored_features.iter().any(|f| info.features.contains(f))
    {
        return Ok(());
    }

    for source in &info.source {
        if let Some(path) = keep_constructed {
            // More on this below: this needs a unique name now that tests
            // can execute concurrently.
            let output_path = path.join(format!("{}-{}.js", sanitize_path(test_name), source.mark));
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

        let jh = std::thread::spawn(move || {
            stdin.write_all(source_text.as_bytes()).expect("writing should be ok?");
        });

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
                        if final_line.starts_with(&expected) { Status::Pass } else { Status::Fail }
                    }
                    Phase::Resolution => {
                        let expected = format!("During resolution: [{error_type}: ");
                        if final_line.starts_with(&expected) { Status::Pass } else { Status::Fail }
                    }
                    Phase::Runtime => {
                        let expected = format!("Thrown: {error_type}: ");
                        if final_line.starts_with(&expected) { Status::Pass } else { Status::Fail }
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

        println!("{test_status}: {} -- {}", test_name.display(), source.mark);
    }

    Ok(())
}

fn sanitize_path(path: &Path) -> String {
    path.to_string_lossy()
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '-' || c == '_' { c } else { '_' })
        .collect()
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let config = Config::builder()
        .add_source(config::File::new("test262-config.yaml", FileFormat::Yaml))
        .add_source(config::Environment::with_prefix("TEST262"))
        .build()?;

    let ignored_features = config
        .get_array("skipped_features")?
        .into_iter()
        .map(config::Value::into_string)
        .collect::<std::result::Result<Vec<_>, _>>()?;

    let harness_path = config.get_string("harness_root")?;
    let harness = HarnessCache::new(harness_path);
    harness.load("assert.js")?;
    harness.load("sta.js")?;
    harness.load("doneprintHandle.js")?;

    let ignored_tests = config
        .get_array("skipped_tests")?
        .into_iter()
        .map(config::Value::into_string)
        .collect::<std::result::Result<Vec<_>, _>>()?;

    let args = Arguments::parse();

    if args.jobs == 0 {
        return Err(eyre!("--jobs must be greater than zero"));
    }

    let next = AtomicUsize::new(0);
    let first_error = std::sync::Mutex::new(None);

    std::thread::scope(|scope| {
        let worker_count = args.jobs.min(args.paths.len());

        for _ in 0..worker_count {
            scope.spawn(|| {
                loop {
                    let index = next.fetch_add(1, Ordering::Relaxed);

                    let Some(test_name) = args.paths.get(index) else {
                        break;
                    };

                    if let Err(err) = run_test(
                        test_name,
                        &harness,
                        &ignored_features,
                        &ignored_tests,
                        args.force,
                        args.keep_constructed.as_deref(),
                    ) {
                        eprintln!("ERROR: {}: {err:?}", test_name.display());

                        let mut first = first_error.lock().unwrap();
                        if first.is_none() {
                            *first = Some(err);
                        }
                    }
                }
            });
        }
    });

    if let Some(err) = first_error.into_inner().unwrap() {
        return Err(err);
    }

    Ok(())
}
