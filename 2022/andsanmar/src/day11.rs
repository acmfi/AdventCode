#[derive(Debug)]
enum OP {
    SUM,
    MUL,
    SUB,
    DIV
}

use crate::OP::*;

type Operation = (Option<u64>, OP, Option<u64>);
type Items = Vec<u64>;

#[derive(Debug)]
struct Monkey {
    operation : Operation, // None == old
    test : (u64, usize, usize) // 1st divisible by, 2nd true, 3rd false
}

impl Monkey {
    fn process(&self, items : &Items,  modulo : u64, star1 : bool) -> Vec<(usize,u64)> {
        items.iter().map(|i| {
            let ni = (apply(*i, &self.operation)/ if star1 {3} else {1}) % modulo;
            if ni % self.test.0 == 0 {
                (self.test.1, ni)
            } else {
                (self.test.2, ni)
            }
        }).collect()
    }
}


fn apply(n : u64, (e1,op,e2) : &Operation) -> u64 {
    let n1 = if let Some(i) = e1 { *i } else { n };
    let n2 = if let Some(i) = e2 { *i } else { n };
    match op {
        SUM => n1 + n2,
        MUL => n1 * n2,
        SUB => n1 * n2,
        DIV => n1 / n2
    }
}

type Struct = Vec<(Items,Monkey)>; // Monkey = definitions, 2nd = current items

fn stars(l : &Struct, bound : usize) {
    let modulo = l.iter().map(|(_,m)| m.test.0).fold(1, |acc,n| acc * n);
    let mut items : Vec<Vec<u64>> = l.iter().map(|(i,_)| i.to_vec()).collect();
    let mut freq : Vec<usize> = vec![0;l.len()];
    for _ in 0..bound {
        for (i,(_,m)) in l.iter().enumerate() {
            freq[i] += items[i].len();
            for (n,v) in m.process(&items[i].to_vec(), modulo, bound == 20) {
                items[n].push(v);
            }
            items[i] = vec![];
        }
    }
    freq.sort_by(|a,b| b.cmp(a));
    println!("{:?}", freq[0] * freq[1]);
}

fn parse_items(input : &str) -> Vec<u64> {
    input.split_whitespace().skip(2).map(|e| e.split(",").next().unwrap().parse().unwrap()).collect()
}

fn parse_op(input: &str)  -> (Option<u64>, OP, Option<u64>) {
    let mut e = input.split("=").skip(1).next().unwrap().split_whitespace();
    let e1 = match e.next() {
        Some("old") => None,
        Some(a) => Some(a.parse().unwrap()),
        _ => None
    };
    let op = match e.next() {
        Some("+") => SUM,
        Some("*") => MUL,
        Some("-") => SUB,
        _ => DIV,
    };
    let e2 = match e.next() {
        Some("old") => None,
        Some(a) => Some(a.parse().unwrap()),
        _ => None
    };
    (e1, op, e2)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().split("\n\n").filter_map(|monkey| {
        if !monkey.is_empty() {
            let mut e = monkey.lines();
            e.next();
            let items = parse_items(e.next().unwrap());
            let operation = parse_op(e.next().unwrap());
            let div = e.next().unwrap().split_whitespace().last().unwrap().parse().unwrap();
            let t = e.next().unwrap().split_whitespace().last().unwrap().parse().unwrap();
            let f = e.next().unwrap().split_whitespace().last().unwrap().parse().unwrap();
            Some((items, Monkey {operation, test : (div, t, f)}))
        }
        else { None }
    }).collect();

    stars(&l, 20);
    stars(&l, 10000);
}
