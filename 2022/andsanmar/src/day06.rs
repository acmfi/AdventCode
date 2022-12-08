use std::collections::HashSet;

type Struct = Vec<char>;

fn star(l : &Struct, j : usize) {
    let n = (j..l.len()).filter(|i| {
        let a : HashSet<&char> = l[i-j..*i].iter().collect();
        a.len() == j
    }).next();
    println!("{:?}", n.unwrap());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            Some(l.chars())
        }
        else { None }
    }).flatten().collect();

    star(&l, 4);
    star(&l, 14);
}
