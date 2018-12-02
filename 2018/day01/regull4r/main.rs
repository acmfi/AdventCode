use std::fs::File;
use std::io::{BufRead, BufReader};
use std::vec::Vec;

fn puzzle1() -> i32 {
    let input = File::open("input.txt").unwrap();
    let reader = BufReader::new(input);
    let res: i32 = reader
        .lines()
        .fold(0, |acc, x| acc + x.unwrap().parse::<i32>().unwrap());
    res
}

fn puzzle2() -> i32 {
    let input = File::open("input.txt").unwrap();
    let reader = BufReader::new(input);
    let mut reaches: Vec<i32> = Vec::new(); 
    reaches.push(0);
    let values: Vec<i32> = reader
        .lines()
        .map(|x| x.unwrap().parse::<i32>().unwrap())
        .collect();
    for val in values.iter().cycle() {
        let last_reach = reaches.last().cloned().unwrap();
        let new_freq = val;
        let new_reach = last_reach + new_freq;
        if reaches.contains(&new_reach) {
            reaches.push(new_reach);
            //println!(" LAST :{:?}", reaches);
            break;
        }
        reaches.push(new_reach);
        //println!("{:?}", reaches);
    }
    return reaches.last().cloned().unwrap();
}

fn main() {
    let puzzle1_res = puzzle1();
    println!("Puzzle 1 solution: {}", puzzle1_res);

    let puzzle2_res = puzzle2();
    println!("Puzzle 2 solution: {}", puzzle2_res);
}
