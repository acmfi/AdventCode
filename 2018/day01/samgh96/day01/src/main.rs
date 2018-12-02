use std::fs::File;
use std::io::prelude::*;

fn load_file_to_string() -> String {
    let mut file = File::open("./input.txt").expect("file loading failed");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("error loading file contents");
    contents
}

fn reduce_frequencies(v: Vec<&str>) -> i32 {
    v.iter().fold(0, |mut sum, &x| {
        println!("{}", x);
        if (!x.is_empty()) {
            sum += x.parse::<i32>().unwrap();
        }
        sum
    })
}

fn main() {
    let contents = load_file_to_string();
    let res = reduce_frequencies(contents.split('\n').collect());

    println!("result:\n{}", res);
}
