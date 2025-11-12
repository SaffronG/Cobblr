fn main() {
    println!("cargo:rerun-if-changed=src/cobblr.lalrpop");

    lalrpop::Configuration::new()
        .generate_in_source_tree() // 👈 ensures src/cobblr.rs is written directly
        .process_file("src/cobblr.lalrpop")
        .unwrap();
}
