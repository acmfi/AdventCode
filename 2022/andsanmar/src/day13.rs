#[derive(Debug)]
enum Elem {
    List(Vec<Elem>),
    Int(i32)
}

use crate::Elem::*;
use std::cmp::Ordering;

type Struct = Vec<(Elem,Elem)>;

impl PartialEq for Elem {
    fn eq(&self, other: &Self) -> bool {
        let f = |v1 : &Vec<Elem>, v2 : &Vec<Elem>| {v1.iter().zip(v2).fold(true, |acc,(e1,e2)| acc && e1 == e2) && v1.len() == v2.len()};
        match (self, other) {
            (Int(n1), Int(n2)) => {n1 == n2},
            (List(v1), Int(n2)) => {f(v1,&vec![Int(*n2)])},
            (Int(n1), List(v2)) => {f(&vec![Int(*n1)],v2)},
            (List(v1), List(v2)) => {f(v1,v2)},
        }
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
        let f = |v1 : &Vec<Elem>, v2 : &Vec<Elem>| {
            let r = v1.iter().zip(v2).fold(Ordering::Equal, |acc,(e1,e2)| {
                if acc == Ordering::Equal {(*e1).cmp(&e2)} else {acc}});
            if r == Ordering::Equal {
                v1.len().cmp(&v2.len())
            } else {
                r
            }
        };
        match (self, other) {
            (Int(n1), Int(n2)) => {n1.cmp(n2)},
            (List(v1), Int(n2)) => {f(v1,&vec![Int(*n2)])},
            (Int(n1), List(v2)) => {f(&vec![Int(*n1)],v2)},
            (List(v1), List(v2)) => {f(v1,v2)},
        }
    }
}

fn star1(l : &Struct) {
    let r = l.iter().enumerate().fold(0, |acc,(i,(a,b))| {
        if a < b {
            acc + i + 1
        } else {
            acc
        }
    });
    println!("{}", r);
}

fn star2(l : Struct) {
    let mut l : Vec<&Elem> = l.iter().map(|(a,b)| [a,b]).flatten().collect();
    let v1 = List(vec![List(vec![Int(2)])]);
    let v2 = List(vec![List(vec![Int(6)])]);
    l.push(&v1);
    l.push(&v2);
    l.sort();
    let pos = l.iter().enumerate().filter_map(|(i,e)| {
        if *e == &v1 || *e == &v2 {
            Some(i+1)
        } else {
            None
        }
    });
    println!("{:?}", pos.fold(1, |acc,n| n*acc));
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
