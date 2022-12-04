use std::collections::HashSet;

type Struct = Vec<(HashSet<char>, HashSet<char>)>;

fn matching(c1 : &HashSet<char>, c2 : &HashSet<char>) -> Vec<char> {
    c1.intersection(c2).map(|c| *c).collect()
}

fn num(l : char) -> i64 {
    l as i64 - if l.is_uppercase() { 'A' as i64 - 27 } else { 'a' as i64 - 1 }
}

fn star1(l : &Struct) {
    let r : Vec<Vec<i64>> = l.iter().map(|(a, b)| {
        matching(a, b).iter().map(|letter| num(*letter)).collect()
    }).collect();
    println!("{:?}", r.iter().flatten().sum::<i64>());
}

fn star2(l : &Struct) {
    let iter = l.chunks(3);
    let mut elems : Vec<i64> = vec![];
    for i in iter {
        let i0 : HashSet<&char> = i[0].0.union(&i[0].1).collect();
        let i1 : HashSet<&char> = i[1].0.union(&i[1].1).collect();
        let i2 : HashSet<&char> = i[2].0.union(&i[2].1).collect();
        let coll : Vec<i64> = i0.intersection(&i1).map(|a| *a).collect::<HashSet<&char>>().intersection(&i2).map(|a| num(**a)).collect();
        elems.push(coll[0])
    }
    println!("{:?}", elems.iter().sum::<i64>());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            let len = l.len();
            let comp_1 = l.chars().take(len/2).collect();
            let comp_2 = l.chars().skip(len/2).take(len/2).collect();
            Some((comp_1, comp_2))
        }
        else { None }
    }).collect();

    star1(&l);
    star2(&l);
}
