use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let file = BufReader::new(File::open("/home/jorge/input.txt").unwrap());

    let arr: Vec<Vec<i32>> = file
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|number| number as i32 - 0x30)
                .collect()
        })
        .collect();

    first_star(arr);
}

fn first_star(lines: Vec<Vec<i32>>) {
    let matrix: Vec<Vec<i32>> = (0..lines[0].len())
        .map(|i| {
            lines
                .iter()
                .map(|inner| inner[i].clone())
                .collect::<Vec<i32>>()
        })
        .collect();

    let mut gamma = String::new();
    let mut epsilon = String::new();

    for row in matrix {
        if row.iter().filter(|&n| *n == 1).count() > row.len() / 2 {
            gamma.push_str("1");
            epsilon.push_str("0");
        } else {
            gamma.push_str("0");
            epsilon.push_str("1");
        }
    }

    let gamma = i32::from_str_radix(&gamma, 2).unwrap();
    let epsilon = i32::from_str_radix(&epsilon, 2).unwrap();

    println!("First Star: {}", gamma * epsilon);
}
