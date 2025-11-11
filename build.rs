fn main() {
    println!("cargo:rerun-if-changed=src/parser.lalrpop");
    lalrpop::Configuration::new().process_current_dir().unwrap();
}
