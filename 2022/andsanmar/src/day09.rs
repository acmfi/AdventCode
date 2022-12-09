#[derive(PartialEq,Debug)]
enum Dir {
    U,
    D,
    L,
    R
}

use crate::Dir::*;
use std::collections::HashSet;

type Struct = Vec<(Dir, u64)>;

fn parse(input : &str) -> Option<(Dir, u64)> {
    let mut e = input.split_whitespace();
    let d = match e.next() {
        Some("U") => Some(U),
        Some("D") => Some(D),
        Some("L") => Some(L),
        Some("R") => Some(R),
        _ => None
    };
    let n = match e.next() {
        Some(s) => match s.parse() {
            Ok(i) => Some(i),
            _ => None
        },
        None => None
    };
    match (d,n) {
        (None, _) => None,
        (_,None) => None,
        (Some(a),Some(b)) => Some((a,b))
    }
}

fn mov ((x, y) : (i64,i64), d : &Dir) -> (i64, i64) {
    match d {
        D => (x, y-1),
        U => (x, y+1),
        L => (x-1, y),
        R => (x+1, y)
    }
}

fn next_pos(h : (i64,i64), t : (i64,i64), i : (i64, i64)) -> (i64,i64) {
    let x = h.0-t.0;
    let y = h.1-t.1;
    if x.abs() <= 1 && y.abs() <= 1 {
        t
    } else {
        i
    }
}

fn next_pos2(h : (i64,i64), t : (i64,i64)) -> (i64,i64) { // This can be used as well for the first star
    let u = |n : i64| {n.abs()/n * (n.abs()-1)};
    let x = h.0-t.0;
    let y = h.1-t.1;
    if x.abs() <= 1 && y.abs() <= 1 {
        t
    } else if x.abs() > y.abs() {
        (t.0 + u(x), h.1)
    } else if y.abs() > x.abs() {
        (h.0, t.1 + u(y))
    } else {
        (t.0 + u(x), t.1 + u(y))
    }
}

fn star1(l : &Struct) {
    let mut pos : HashSet<(i64,i64)> = HashSet::new();
    let mut h = (0,0);
    let mut t = (0,0);

    for (d,n) in l {
        for _ in 0..*n {
            let i = h;
            h = mov(h, d);
            t = next_pos(h,t,i);
            pos.insert(t);
        }
    }
    
    println!("{:?}", pos.len());
}

fn star2(l : &Struct) {
    let mut pos : HashSet<(i64,i64)> = HashSet::new();
    let mut t = [(0,0);10];

    for (d,n) in l {
        for _ in 0..*n {
            t[0] = mov(t[0], d);
            for i in 1..10 {
                t[i] = next_pos2(t[i-1],t[i]);
            }
            pos.insert(t[9]);
        }
    }
    
    println!("{:?}", pos.len());
}



fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        parse(l)
    }).collect();

    star1(&l);
    star2(&l);
}
