use std::env;
use std::process::Command;

fn main() {
    let pwd = env::current_dir().unwrap();

    let mut cmd = Command::new("g++");

    cmd.current_dir(&pwd)
       .args(&["-g", "-shared", "-std=c++0x", "-lre2", "-lc++"]);

    if cfg!(target_os = "macos") {
        cmd.arg("-olibre2.dylib");
    }
    else if cfg!(target_os = "linux") {
        cmd.arg("-olibre2.so");
    }
    else {
        panic!("find your self a linux or mac");
    }

    cmd.arg(pwd.join("re2.cc"))
       .status()
       .expect("fail to compile re2.cc");

    println!("cargo:rustc-link-search=native={}", pwd.to_str().unwrap());
}
