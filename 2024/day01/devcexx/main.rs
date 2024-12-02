use std::{error::Error, io::{self, Read}, result};

use itertools::Itertools;

fn main() -> result::Result<(), Box<dyn Error>> {
    let mut input = String::new();

    io::stdin().read_to_string(&mut input)?;

    let (mut left, mut right): (Vec<_>, Vec<_>) = input.lines().into_iter()
        .enumerate()
        .map(|(line_idx, line)| {
            let mut split = line.split("   ");
            match (split.next().and_then(|e| e.parse::<i32>().ok()), split.next().and_then(|e| e.parse::<i32>().ok())) {
                (Some(l), Some(r)) => Ok((l, r)),
                _ => Err(format!("Failed to parse line {}", line_idx + 1))
            }

        }).try_collect()?;

    left.sort();
    right.sort();

    let diff: i32 = left.into_iter().zip(right.into_iter())
        .map(|(l, r)| (l - r).abs())
        .sum();

    println!("{}", diff);
    Ok(())
}
// 1189304
