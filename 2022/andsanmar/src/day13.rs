#[derive(Debug,Clone)]
enum Elem {
    List(Vec<Elem>),
    Int(i32)
}

use crate::Elem::*;
use std::cmp::Ordering;

type Struct = Vec<(Elem,Elem)>;

impl PartialEq for Elem {
    fn eq(&self, other: &Self) -> bool {
        if let (Int(n1), Int(n2)) = (self, other) {
            return n1 == n2;
        }

        let v1 = match self {
            Int(n) => { vec![Int(*n)] },
            List(v) => { v.to_vec() }
        };
        let v2 = match other {
            Int(n) => { vec![Int(*n)] },
            List(v) => { v.to_vec() }
        };
        v1.iter().zip(&v2).fold(true, |acc,(e1,e2)| acc && e1 == e2) && v1.len() == v2.len()
    }
}

impl Eq for Elem {}

impl PartialOrd for Elem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Elem {
    fn cmp(&self, other: &Self) -> Ordering {
        // println!("Compare {:?} {:?}", &self, &other);
        if let (Int(n1), Int(n2)) = (self, other) {
            return n1.cmp(n2);
        }
        let v1 = match self {
            Int(n) => { vec![Int(*n)] },
            List(v) => { v.to_vec() }
        };
        let v2 = match other {
            Int(n) => { vec![Int(*n)] },
            List(v) => { v.to_vec() }
        };
        let r = v1.iter().zip(&v2).fold(Ordering::Equal, |acc,(e1,e2)| {
            if acc == Ordering::Equal {(*e1).cmp(&e2)} else {acc}});
        if r == Ordering::Equal {
            v1.len().cmp(&v2.len())
        } else {
            r
        }
    }
}

fn star1(l : &Struct) {
    let mut r = 0;
    for (i,(a,b)) in l.iter().enumerate() {
        if a < b {
            r += i + 1;
        }
    }
    println!("{}", r);
}

fn star2(l : Struct) {
    let mut l : Vec<&Elem> = l.iter().map(|(a,b)| [a,b]).flatten().collect();
    let v1 = List(vec![List(vec![Int(2)])]);
    let v2 = List(vec![List(vec![Int(6)])]);
    l.push(&v1);
    l.push(&v2);
    l.sort();
    let mut pos1 = 0;
    let mut pos2 = 0;
    for (i,e) in l.iter().enumerate() {
        if *e == &v1 {
           pos1 = i +1;
        }
        if *e == &v2 {
           pos2 = i +1;
        }
    }
    println!("{:?}", pos1*pos2);
}

fn parse(input : &[char], index : usize) -> (Elem,usize) {
    if input[index] == '[' {
        let mut end = index+1;
        let mut elems = vec![];
        if input[end] == ']' { return (List(elems),end+1) }
        while input[end] != ']' {
            let (elem,next) = parse(input, end);
            end = next;
            elems.push(elem);
            if input[end] == ',' {end += 1;}
        }
        (List(elems),end+1)
    } else {
        let mut end = index;
        while input[end] != ',' && input[end] != ']' {
            end += 1;
        }
        let n = input[index..end].iter().collect::<String>().parse().unwrap();
        (Int(n),end)
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().split("\n\n").filter_map(|l| {
        if !l.is_empty() {
            let mut n = l.lines();
            let l1 : Vec<char> = n.next().unwrap().chars().collect();
            let l2 : Vec<char> = n.next().unwrap().chars().collect();
            Some((parse(&l1, 0).0, parse(&l2, 0).0))
        }
        else { None }
    }).collect();

    star1(&l);
    star2(l);
}
