use std::{error::Error, io, result};

use ndarray::{Array, Array2};

fn match_word(word: &[u8], buf: &Array2<u8>, cy: usize, cx: usize, dirx: isize, diry: isize) -> bool {
    for (index, ch) in word.iter().enumerate() {
        let x = cx as isize + dirx * index as isize;
        let y = cy as isize + diry * index as isize;

        if x < 0 || y < 0 || y >= buf.dim().0 as isize || x >= buf.dim().1 as isize {
            return false;
        }

        if *ch != buf[(y as usize, x as usize)] {
            return false;
        }
    }

    true
}

fn match_word_anydir(word: &[u8], matrix: &Array2<u8>, cy: usize, cx: usize) -> i32 {
    let mut cnt = 0;

    for dy in [-1, 0, 1] {
        for dx in [-1, 0, 1] {
            if match_word(word, &matrix, cy, cx, dx, dy) {
                cnt += 1;
            }
        }
    }

    cnt
}

fn main() -> result::Result<(), Box<dyn Error>> {
    let mut buf = Vec::new();
    let lines = io::stdin().lines().peekable();
    let mut line_len = 0;
    let mut line_count = 0;


    for (index, line) in lines.enumerate() {
        let line = line?;
        if index == 0 {
            line_len = line.len();
        }

        if line.trim().len() > 0 {
            if line.len() != line_len {
                return Err(format!("Invalid line length for line at {}. Expected length of {} but {} got", index + 1, line_len, line.len()).into());
            }

            buf.extend(line.chars().map(|c| c as u8).collect::<Vec<_>>());
        }

        line_count += 1;
    }

    let matrix = Array::from_shape_vec((line_count, line_len), buf).unwrap();

    let mut cnt = 0;
    for y in 0..matrix.dim().0 {
        for x in 0..matrix.dim().1 {
            if matrix[(y, x)] == b'X' {
                cnt += match_word_anydir(b"XMAS", &matrix, y, x);
            }
        }
    }

    println!("{}", cnt);
    Ok(())
}
