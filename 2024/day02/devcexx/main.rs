use std::{error::Error, io::{self, Read}, result};
use itertools::Itertools;

fn check_safety<I: Iterator<Item = i32>>(mut input: I) -> bool {
    fn pair_valid(l: i32, r: i32, dir: i32) -> bool {
        let diff = l - r;
        diff.signum() == dir && diff * dir <= 3
    }

    let (Some(first), Some(second)) = (input.next(), input.next()) else {
        // If number of elements is < 2, it should be considered safe.
        return true;
    };
    let direction = (second - first).signum();
    if direction == 0 || !pair_valid(second, first, direction) {
        return false;
    }

    let mut prev = second;
    for next in input {
        if !pair_valid(next, prev, direction) {
            return false;
        }

        prev = next;
    }

    return true;
}

fn main() -> result::Result<(), Box<dyn Error>> {
    let mut input = String::new();

    io::stdin().read_to_string(&mut input)?;

    let nsafe = input.lines().into_iter()
        .enumerate()
        .try_fold(0, |acc, (line_idx, line)| {
            let trim = line.trim();
            if trim.is_empty() {
                return Ok::<_, String>(acc);
            }

            let is_safe = trim.split(" ")
                .map(|entry| entry.parse::<i32>().map_err(|_| format!("Invalid line {}", line_idx + 1)))
                .process_results(|entries| check_safety(entries))?;

            Ok(if is_safe {
                acc + 1
            } else {
                acc
            })
        })?;


    println!("{}", nsafe);
    Ok(())
}
