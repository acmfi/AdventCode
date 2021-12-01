use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

fn main() {
    let lines = lines_from_file("/home/jorge/input.txt").expect("File not found or other error.");

    first_star(&lines);
    second_start(&lines);
}

fn lines_from_file(filename: impl AsRef<Path>) -> io::Result<Vec<String>> {
    BufReader::new(File::open(filename)?).lines().collect()
}

fn first_star(lines: &Vec<String>) {
    let mut prev = -1;
    let mut increase = -1;

    for line in lines {
        if prev < line.parse::<i32>().unwrap() {
            increase += 1;
        }

        prev = line.parse::<i32>().unwrap();
    }

    println!("First start: {}", increase);
}

fn second_start(lines: &Vec<String>) {
    let int_lines: Vec<i32> = lines.iter().map(|x| x.parse::<i32>().unwrap()).collect();
    let mut prev = -1;
    let mut increase = -1;

    for i in 0..int_lines.len() - 2 {
        let actual = int_lines[i] + int_lines[i + 1] + int_lines[i + 2];
        if prev < actual {
            increase += 1
        }
        prev = actual;
    }

    println!("Second star: {}", increase);
}
