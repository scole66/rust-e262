fn main() {
    cc::Build::new()
        .file("src/dtoa.c")
        .warnings(false)
        .compile("dtoa");
    println!("cargo:rerun-if-changed=src/dtoa.c");
}
