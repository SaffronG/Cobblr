fn main() {
    println!("cargo:rerun-if-changed=src/cobblr.lalrpop");
    println!("Building grammar file...");
    lalrpop::process_root().unwrap();
}
