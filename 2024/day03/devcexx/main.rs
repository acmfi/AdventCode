use std::{error::Error, io::{self, Read}, result};

mod sol1 {
    use regex::Regex;

    pub fn solve_muls(input: &str) -> i32 {
        let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
        re.captures_iter(input).map(|m| {
            m[1].parse::<i32>().unwrap() * m[2].parse::<i32>().unwrap()
        }).sum()
    }
}

mod sol2 {
    // Just for giving a solution that at least doesn't rely fully on
    // regex, because it feels kinda cheating
    pub fn solve_muls(input: &str) -> i32 {
        const MUL_KEYWORD: &'static str = "mul";

        input.match_indices(MUL_KEYWORD).filter_map(|(mul_pos, _)| {
            let mut occurrence = &input[mul_pos + MUL_KEYWORD.len()..];

            if &occurrence[0..1] != "(" {
                return None
            }

            let Some(comma_idx) = occurrence[0..5].chars().position(|c| c == ',') else {
                return None;
            };

            let Ok(first_num) = occurrence[1..comma_idx].parse::<i32>() else {
                return None;
            };

            occurrence = &occurrence[comma_idx+1..];
            let Some(paren_idx) = occurrence[0..comma_idx+4].chars().position(|c| c == ')') else {
                return None;
            };

            let Ok(second_num) = occurrence[0..paren_idx].parse::<i32>() else {
                return None;
            };

            Some(first_num * second_num)
        }).sum()
    }
}

use sol1::solve_muls;
// use sol2::solve_muls;

fn main() -> result::Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    println!("{}", solve_muls(&input));
    Ok(())
}
