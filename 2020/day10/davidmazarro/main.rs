use std::fs::read_to_string;

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let mut adapters = input
        .lines()
        .map(|l| l.parse::<u32>().unwrap())
        .collect::<Vec<u32>>();
    adapters.sort();
    adapters.push(*adapters.last().unwrap() + 3);
    let mut iterator = adapters.iter();
    let mut differences: Vec<u32> = Vec::new();
    let mut prev = 0;
    let mut next = iterator.next();
    while next != None {
        differences.push(next.unwrap() - prev);
        prev = *next.unwrap();
        next = iterator.next();
    }
    let jolts_one_times_three = differences.iter().filter(|&x| *x == 3).count()
        * differences.iter().filter(|&x| *x == 1).count();
    println!("*** 1st star ***");
    println!("{}", jolts_one_times_three);

    // adapters.insert(0, 0);
    // let possibilities = count_possibles(&adapters);
    // println!("*** 2nd star ***");
    // println!("{}", possibilities);
}
