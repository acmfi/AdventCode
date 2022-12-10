#[derive(PartialEq)]
enum Ins {
    NOP,
    ADDX(i32),
}

use crate::Ins::*;

impl Ins {
    fn exec(&self, n : i32) -> i32 {
        match self {
            NOP => n,
            ADDX(i) => n+i
        }
    }
}

type Struct = Vec<Ins>;

fn stars(l : &Struct) {
    let mut state : Vec<i32> = vec![1,1,1];
    for x in l {
        let result = x.exec(*state.last().unwrap());
        if let ADDX(_) = x {
            state.push(result);
        }
        state.push(result);
    }
    //Star 1
    let n : Vec<i32> = (0..6).map(|i| {
        let n = i*40+20;
        state[n as usize]*n
    }).collect();
    println!("{:?}", n.iter().sum::<i32>());
    // Star2
        for j in 0..6 {
        for i in 0..40 {
            let n = i+j*40;
            print!("{}", if (i as i32 - state[n+1]).abs() <= 1 {'#'} else {' '});
        }
        println!();
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            let mut n = l.split_whitespace();
            Some(match n.next() {
                Some("addx") => ADDX(n.next().unwrap().parse().unwrap()),
                _ => NOP
            })
        }
        else { None }
    }).collect();

    stars(&l);
}
