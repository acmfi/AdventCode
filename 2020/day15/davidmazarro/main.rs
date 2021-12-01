use std::fs::read_to_string;

fn next_number(prev_numbers: &Vec<u64>) -> u64 {
    let mut prev_occurrence = 0;
    let mut is_found = false;
    let mut rev_iter = prev_numbers.iter().rev();
    let last_number = rev_iter.next().unwrap();
    for elem in rev_iter {
	prev_occurrence += 1;
	if elem == last_number {
	    is_found = true;
	    break;
	}
    }
    if is_found { prev_occurrence } else { 0 }
}

fn main() {
    let input = read_to_string("input.txt").unwrap().replace("\n", "");
    let mut seed: Vec<u64> = Vec::new();
    for elem in input.split(",") {
	seed.push(elem.parse::<u64>().unwrap());
    }
    let mut prev_numbers: Vec<u64> = Vec::new();
    for elem in &seed { prev_numbers.push(*elem); };
    for _ in 0..(2020 - seed.len()) {
	prev_numbers.push(next_number(&prev_numbers));
    }
    println!("*** 1st star ***");
    println!("{}", prev_numbers.last().unwrap());

    // Brute-force like this is absurdly slow,
    // I didn't submit the second star because of this
    // (and I didn't want to rework the whole thing).
    // RUN AT YOUR OWN RISK!
    let mut prev_numbers: Vec<u64> = Vec::new();
    for elem in &seed { prev_numbers.push(*elem); };
    for _ in 0..(30000000 - seed.len()) {
	prev_numbers.push(next_number(&prev_numbers));
    }
    println!("*** 2nd star ***");
    println!("{}", prev_numbers.last().unwrap());
}
