use std::collections::HashMap;

type Polymer<'a> = &'a str;
type Rule<'a> = (Polymer<'a>,String);

fn star1((p,rules) : &(Polymer, Vec<Rule>)) {
    let mut polymer = p.to_string();
    for _ in 0..10 {
        for (from, to) in rules {
            while polymer.contains(from) {
                polymer = polymer.replace(from, &to);
            }
        }
        polymer = polymer.to_uppercase();
    }
    let letters : Vec<char> = polymer.chars().collect();
    let mut keys = letters.clone();
    keys.sort();
    keys.dedup();
    let ocurrences : Vec<usize> = keys.iter().map(|c| letters.iter().filter(|e| *e == c).count()).collect();
    let max = ocurrences.iter().max().unwrap();
    let min = ocurrences.iter().min().unwrap();
    println!("{:?}", max-min);
}

fn star2((p,rules) : &(Polymer, Vec<Rule>)) {
    let fixed_rules : Vec<Rule> = rules.iter().map(|(a,b)| (*a,b.to_uppercase())).collect();
    let mut polymer : HashMap<&str, usize> = HashMap::new();
    for i in 1..p.len() {
        let pair = &p[i-1..=i];
        polymer.entry(pair).or_insert(0);
        let n : &mut usize = polymer.get_mut(pair).unwrap();
        *n += 1;
    }
    for _ in 0..40 {
        let mut new_polymer : HashMap<&str, usize> = HashMap::new();
        for (from, to) in &fixed_rules {
            let existing = *polymer.get(from).unwrap_or(&0);
            for key in [&to[1..],&to[..2]] {
                new_polymer.entry(key).or_insert(0);
                let n : &mut usize = new_polymer.get_mut(key).unwrap();
                *n += existing;
            }
        }
        polymer = new_polymer;
    }
    let mut ocurrences_double : HashMap<char,usize> = HashMap::new();
    for (k,v) in polymer {
        for c in k.chars() {
            ocurrences_double.entry(c).or_insert(0);
            let n : &mut usize = ocurrences_double.get_mut(&c).unwrap();
            *n += v;
        }
    }
    // First and last ocurrences must be counted twice as well
    let n = ocurrences_double.get_mut(&p.chars().nth(0).unwrap()).unwrap();
    *n += 1;
    let n = ocurrences_double.get_mut(&p.chars().nth(p.len()-1).unwrap()).unwrap();
    *n += 1;
    
    let max = ocurrences_double.values().max().unwrap()/2;
    let min = ocurrences_double.values().min().unwrap()/2;
    println!("{:?}", max-min);
}

fn parse_input(s : &String) -> (&str, Vec<Rule>) {
    let mut n = s.split("\n\n");
    let x = n.next().unwrap();
    let y = n.next().unwrap().lines().filter_map(|l| {
        let mut t = l.split(" -> ");
        let from : Polymer = t.next().unwrap();
        let mut to = from.clone().to_string();
        let new : String = t.next().unwrap().to_lowercase();
        to.insert(1, new.chars().nth(0).unwrap());
        Some((from,to))
    }).collect();
    (x,y)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: (Polymer, Vec<Rule>) = parse_input(&filename);
    star1(&s);
    star2(&s);
}
