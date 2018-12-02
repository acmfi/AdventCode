use std::collections::HashSet;
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
        if !x.is_empty() {
            sum += x.parse::<i32>().unwrap();
        }
        sum
    })
}

fn reduce_frequencies_with_set(v: Vec<&str>) -> i32 {
    let mut set = HashSet::new();
    let mut accum = 0;
    set.insert(0);

    loop {
        for x in v.iter() {
            if !x.is_empty() {
                accum += x.parse::<i32>().unwrap();
                if !set.insert(accum) {
                    return accum;
                }
            }
        }
    }
}

fn main() {
    let contents = load_file_to_string();
    println!("--- DAY 01 PART 1: ---");
    let res = reduce_frequencies(contents.split('\n').collect());
    println!("result:\n{}", res);
    println!("--- DAY 01 PART 2: ---");
    let res2 = reduce_frequencies_with_set(contents.split('\n').collect());
    println!("result:\n{}", res2);
}
