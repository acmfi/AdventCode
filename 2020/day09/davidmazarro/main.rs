use std::fs::read_to_string;

fn is_sum_prev25(prev25: &[u64], num: &u64) -> bool {
    for &first in prev25 {
        for &second in prev25 {
            if first == second {
                continue;
            } else if first + second == *num {
                return true;
            }
        }
    }
    false
}

fn find_weakness(prev: &[u64], num: &u64) -> Option<u64> {
    for start in 0..prev.len() {
        for end in (start + 2)..prev.len() {
            let sum_range: u64 = prev[start..end].iter().sum();
            if sum_range == *num {
		let min_range: &u64 = prev[start..end].iter().min().unwrap();
		let max_range: &u64 = prev[start..end].iter().max().unwrap();
                return Some(min_range + max_range);
            } else if sum_range > *num {
                break;
            }
        }
    }
    None
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let numbers: Vec<u64> = input.lines().map(|l| l.parse::<u64>().unwrap()).collect();
    // We start checking from the 25th number on the list
    let mut n = 25;
    for _ in n..numbers.len() {
        if !is_sum_prev25(&numbers[(n - 25)..n], &numbers[n]) {
            break;
        } else {
            n += 1;
        }
    }

    let weakness = find_weakness(&numbers, &numbers[n]);
    println!("*** 1st star ***");
    println!("{}", &numbers[n]);
    println!("*** 2nd star ***");
    println!("{}", weakness.unwrap());
}
