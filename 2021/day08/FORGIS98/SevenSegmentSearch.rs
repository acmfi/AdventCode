use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let file = BufReader::new(File::open("/home/jorge/input.txt").unwrap());

    let mut end: Vec<String> = Vec::new();

    for l in file.lines() {
        let txt = l.unwrap();
        let (_, last) = txt.split_at(txt.find('|').unwrap_or(txt.len()));
        end.push(last.to_string().split_off(2));
    }

    first_star(&end);
}

fn first_star(lines: &Vec<String>) {
    let mut count: i32 = 0;

    for line in lines {
        for l in line.split(' ') {
            if l.len() == 7 || l.len() < 5 {
                count += 1;
            }
        }
    }

    println!("First Star: {}", count);
}