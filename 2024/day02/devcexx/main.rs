use std::{error::Error, io::{self, Read}, result};
use itertools::Itertools;

struct Except<I> {
    iterator: I,
    skip_index: usize,
    _cur_index: usize
}

impl<I> Except<I> {
    fn new(iterator: I, skip_index: usize) -> Except<I> {
        Except {
            iterator,
            skip_index,
            _cur_index: 0
        }
    }
}

impl<E, I> Iterator for Except<I> where I: Iterator<Item = E> {
    type Item = E;

    fn next(&mut self) -> Option<Self::Item> {
        if self._cur_index == self.skip_index {
            self.iterator.next();
        }

        self._cur_index += 1;
        self.iterator.next()
    }
}

fn check_fully_safe<I: Iterator<Item = i32>>(mut input: I) -> bool {
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

fn check_almost_safe(elems: &[i32]) -> bool {
    if elems.len() <= 1 {
        return true;
    }

    for i in 0..elems.len() {
        if check_fully_safe(Except::new(elems.iter().copied(), i)) {
            return true;
        }
    }

    false
}

fn main() -> result::Result<(), Box<dyn Error>> {
    let mut input = String::new();

    io::stdin().read_to_string(&mut input)?;

    let lines = input.lines().into_iter()
        .enumerate()
        .filter_map(|(line_idx, line)| {
            let trim = line.trim();
            if trim.is_empty() {
                return None;
            }

            let parsed_line: Result<Vec<_>, _> = trim.split(" ")
                .map(|entry| entry.parse::<i32>().map_err(|_| format!("Invalid line {}", line_idx + 1)))
                .try_collect();
            Some(parsed_line)
        });

    let mut fully_safe: u32 = 0;
    let mut almost_safe: u32 = 0;

    for line in lines {
        let line = line?;

        if check_fully_safe(line.iter().copied()) {
            fully_safe += 1;
        }

        if check_almost_safe(&line) {
            almost_safe += 1;
        }
    }


    println!("Fully safe: {}", fully_safe);
    println!("Almost safe: {}", almost_safe);
    Ok(())
}
