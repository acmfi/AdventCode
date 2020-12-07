use std::fs;
use std::collections::HashMap;

fn search_contained<'a>(to_carry : &Vec<String>, m: &HashMap<Vec<String>, Vec<Vec<String>>>) -> Vec<Vec<String>> {
    let mut r : Vec<Vec<String>> = Vec::new();
    match m.get(to_carry) {
        None => {},
        Some(parent) => for e in parent {
            r.push(e.to_vec());
            for x in search_contained(e,m) {
                r.push(x)
            }
        }
    }
    r.sort();
    r.dedup();
    r
}

fn nested_bags<'a>(bag : &Vec<String>, m: &HashMap<Vec<String>, Vec<(usize,Vec<String>)>>) -> usize {
    match m.get(bag) {
        None => 0,
        Some(parent) => parent.iter().fold(0, |acc, (size, n_bag)|
                                           acc + size + size*nested_bags(n_bag, m))
    }
}

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let shiny_gold : Vec<String> = vec!["shiny","gold"].iter().map(|z| String::from(*z)).collect();
    let mut contains: HashMap<Vec<String>, Vec<(usize,Vec<String>)>> = HashMap::new();
    let mut contained_by: HashMap<Vec<String>, Vec<Vec<String>>> = HashMap::new();
    for l in r.lines() {
        let mut d = l.split("contain");
        let source : Vec<String> = d.next().unwrap().split_whitespace().collect::<Vec<&str>>().split_last().unwrap().1.iter().map(|z| String::from(*z)).collect();
        let contained : Vec<(usize,Vec<String>)> = d.next().unwrap().split(",").map(|s| {
            let defined = s.trim().split_whitespace().collect::<Vec<&str>>();
            let (n, b) = defined.split_first().unwrap();
            ( match n {
                &"no" => 0,
                _ => n.parse::<usize>().unwrap()
            }, b.to_vec().split_last().unwrap().1.iter().map(|z| String::from(*z)).collect())
        }).collect();
        for (_x,y) in contained.to_vec() {
            contained_by.entry(y).or_insert(vec![]).push(source.to_vec())}        
        contains.insert(source, contained);
    }
    let star1 = search_contained(&shiny_gold, &contained_by);
    println!("{:?}", star1.len());
    let star2 = nested_bags(&shiny_gold, &contains);
    println!("{:?}", star2);
}
