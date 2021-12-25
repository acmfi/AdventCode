use std::fmt;
use std::mem;

#[derive(Debug,PartialEq,Clone)]
enum Node {
    Pair(Box<Node>,Box<Node>),
    Lit(u32)
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Lit(a) =>  write!(f, "{}", a),
            Node::Pair(a,b) =>  write!(f, "[{},{}]", a, b),
        }
    }
}

struct Tree {
    root : Box<Node>
}

impl Tree {
    fn append(&mut self, elem: &Box<Node>) {
        let prev_root : Box<Node> = mem::replace(&mut self.root, Box::new(Node::Lit(0)));
        self.root = Box::new(Node::Pair(prev_root, Box::clone(elem)));
    }
}

fn add_first(n : &mut Node, to_add : u32, left : bool) -> bool {
    match n {
        Node::Pair(a,b) => add_first(if left {&mut *a} else {&mut *b}, to_add, left),
        Node::Lit(e) => {*n = Node::Lit(*e+to_add); true},
    }
}

fn find_explosion(n : &mut Node, level : u8) -> Option<(u32, u32)> {
    match level {
        4 => {
            if let Node::Pair(a, b) = n {
                if let Node::Lit(x) = **a {
                    if let Node::Lit(y) = **b {
                        *n = Node::Lit(0);
                        return Some((x,y))
                    }
                } 
            } 
            None
        },
        _ => match n {
            Node::Lit(_) => None,
            Node::Pair(l,r) => {
                match find_explosion(l, level + 1) {
                    Some((a,b)) => {
                        let sl = a;
                        let sr = if add_first(r, b, true) { 0 } else { b };
                        Some((sl,sr))
                    },
                    None => {
                        match find_explosion(r, level + 1) {
                            Some((a,b)) => {
                                let sl = if add_first(l, a, false) { 0 } else { a };
                                let sr = b;
                                Some((sl,sr))
                            },
                            None => None
                        }
                    }
                }
            }
        }
    }
}

fn split(n : &mut Node) -> bool {
    match n {
        Node::Lit(e) if *e >= 10 => {
            *n = Node::Pair(Box::new(Node::Lit(*e/2)), Box::new(Node::Lit(*e/2+*e%2)));
            true
        },
        Node::Pair(a, b) => {
            if split(a) { true } else { split(b) }
        },
        _ => false
    }
}

fn reduce (n : &mut Node) {
    if find_explosion(n, 0) == None {
        if split(n) {
            reduce(n)
        }
    } else {
        reduce(n)
    }
}

fn magnitude(n : &Node) -> u32 {
    match n {
        Node::Lit(e) => *e,
        Node::Pair(a,b) => 3*magnitude(&a) + 2*magnitude(&b)
    }
}

fn star1 (mut s: Vec<Tree>) {
    let mut i = s.iter_mut();
    let a : &mut Tree = i.next().unwrap();
    reduce(&mut a.root);
    while let Some(to_insert) = i.next() {
        a.append(&mut to_insert.root);
        reduce(&mut a.root);
    }
    println!("{}", magnitude(&a.root));
}

fn star2 (s: Vec<Tree>) {
    let mut magnitudes = vec![];
    for x in 0..s.len() {
        let base : &Tree = s.get(x).unwrap();
        for y in 0..s.len() {
            if x != y {
                let t1 = &mut Tree { root : Box::clone(&base.root) };
                let base2 = s.get(y).unwrap();
                let t2 = &mut Tree { root : Box::clone(&base2.root) };
                reduce (&mut t1.root);
                t1.append(&mut t2.root);
                reduce(&mut t1.root);
                magnitudes.push(magnitude(&t1.root));
            }
        }
    }
    println!("{}", magnitudes.iter().max().unwrap());
}

fn parse_pair<'a>(s : &'a str) -> (Node, &'a str) {
    match s.chars().nth(0) {
        Some('[') => {
            let (left, s_r) = parse_pair(&s[1..]);
            let (right, res) = parse_pair(s_r);
            (Node::Pair(Box::new(left), Box::new(right)), res)
        },
        Some(']') | Some(',') => parse_pair(&s[1..]),
        Some(c) => {
            let n = match c.to_digit(10) {
                Some(e) => e,
                None => panic!()
            };
            (Node::Lit(n),&s[1..])
        },
        None => panic!()
    }
}

fn parse_input<'a>(s : &'a String) -> Vec<Tree> {
    s.lines().filter_map(|l| {
        match l.len() {
            0 => None,
            _ => Some(Tree { root : Box::new(parse_pair(l).0)})
        }
    }).collect()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s1: Vec<Tree> = parse_input(&filename);
    let s2: Vec<Tree> = parse_input(&filename);
    star1(s1);
    star2(s2);
}
