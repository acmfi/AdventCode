use std::{error::Error, io::{self, Read}, iter::Peekable, result};

use itertools::Itertools;

fn consume_while<E, I, F>(p: &mut Peekable<I>, f: F) -> usize where I: Iterator<Item = E>, F: Fn(&E) -> bool {
    let mut cnt = 0;
    while {
        match p.peek() {
            Some(next) => f(next),
            None => false,
        }
    } {
        cnt += 1;
        p.next();
    }

    cnt
}

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

    let diff: i32 = left.iter().zip(right.iter())
        .map(|(l, r)| (l - r).abs())
        .sum();

    let mut left_it = left.into_iter().peekable();
    let mut right_it = right.into_iter().peekable();

    let mut similarity = 0;

    while let (Some(next_left), Some(_)) = (left_it.peek().copied(), right_it.peek().copied()) {
        let current_left_count = consume_while(&mut left_it, |e| *e == next_left);
        let _ = consume_while(&mut right_it, |e| *e < next_left);
        let current_right_count = consume_while(&mut right_it, |e| *e == next_left);

        similarity += current_left_count * current_right_count * next_left as usize;
    }

    println!("Diff: {}", diff);
    println!("Similarity: {}", similarity);

    Ok(())
}
