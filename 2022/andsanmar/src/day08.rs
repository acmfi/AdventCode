use std::collections::HashSet;
use std::cmp::max;

type Struct = Vec<Vec<i8>>;

fn star1(l : &Struct) {
    let mut coords : HashSet<(usize, usize)> = HashSet::new();

    let mut f = |i,j,last| {
        let z : &Vec<i8> = &l[i];
        let e = z[j];
        if last < e {
            coords.insert((i,j));
            e
        } else {
            last
        }
    };

    for i in 0..l.len() {
        let _ = (0..l[0].len()).fold(-1, |last1, j| f(i,j,last1));
        let _ = (0..l[0].len()).rev().fold(-1, |last1, j| f(i,j,last1));
    }

    for j in 0..l[0].len() {
        let _ = (0..l.len()).fold(-1, |last1, i| f(i,j,last1));
        let _ = (0..l.len()).rev().fold(-1, |last1, i| f(i,j,last1));
    }
    
    println!("{:?}", coords.len());
}

fn scenic_score(l : &Struct, (i, j) : (usize, usize)) -> u64 {
    let h = l[i][j];

    let f = |(acc, k, dir), x| {
        let e : i8 = if dir {
            let z : &Vec<i8> = &l[x];
            z[j]
        } else {
            l[i][x]
        };
        if k {
            (acc + 1,
             if k && e < h {
                 true
             } else {
                 false
             },
             dir)
        } else {
            (acc, k, dir)
        }
    };

    let a = ((i+1)..l.len()).fold((0, true, true), f).0;
    let b = ((j+1)..l[0].len()).fold((0, true, false), f).0;
    let c = (0..i).rev().fold((0, true, true), f).0;
    let d = (0..j).rev().fold((0, true, false), f).0;
    a*b*c*d
}

fn star2(l : &Struct) {
    let mut m = 0;
    for i in 1..l.len() {
        for j in 1..l[0].len() {
            m = max(scenic_score(l, (i,j)), m);
        }
    }
    println!("{:?}", m);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            Some(l.chars().map(|e| e.to_digit(10).unwrap() as i8).collect())
        }
        else { None }
    }).collect();

    star1(&l);
    star2(&l);
}
