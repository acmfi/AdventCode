use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

use regex::Regex;

fn main() {
    let lines = lines_from_file("/home/jorge/input.txt").expect("File not found or other error.");

    first_star(&lines);
    second_start(&lines);
}

fn lines_from_file(filename: impl AsRef<Path>) -> io::Result<Vec<String>> {
    BufReader::new(File::open(filename)?).lines().collect()
}

fn first_star(lines: &Vec<String>) {
    let forward_rx = Regex::new(r"forward (\d+)").unwrap();
    let up_rx = Regex::new(r"up (\d+)").unwrap();
    let down_rx = Regex::new(r"down (\d+)").unwrap();

    let mut forward = 0;
    let mut depth = 0;

    for line in lines {
        if forward_rx.is_match(line) {
            forward += forward_rx
                .captures(line)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<i32>()
                .unwrap();
        } else if up_rx.is_match(line) {
            depth -= up_rx
                .captures(line)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<i32>()
                .unwrap();
        } else {
            depth += down_rx
                .captures(line)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<i32>()
                .unwrap();
        }
    }

    println!("First Star: {}", forward * depth);
}

fn second_start(lines: &Vec<String>) {
    let forward_rx = Regex::new(r"forward (\d+)").unwrap();
    let up_rx = Regex::new(r"up (\d+)").unwrap();
    let down_rx = Regex::new(r"down (\d+)").unwrap();

    let mut forward = 0;
    let mut depth = 0;
    let mut aim = 0;

    for line in lines {
        if forward_rx.is_match(line) {
            let value = forward_rx
                .captures(line)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<i32>()
                .unwrap();

            forward += value;
            depth += aim * value;
        } else if up_rx.is_match(line) {
            aim -= up_rx
                .captures(line)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<i32>()
                .unwrap();
        } else {
            aim += down_rx
                .captures(line)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<i32>()
                .unwrap();
        }
    }

    println!("First Star: {}", forward * depth);
}
