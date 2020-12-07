use std::fs;
use std::collections::HashMap;

fn search_contained<'a>(to_carry : &String, m: &HashMap<String, Vec<String>>) -> Vec<String> {
    match m.get(to_carry) {
        None => Vec::new(),
        Some(parent) => {
            let mut r = parent.to_vec();
            for e in parent { r.append(&mut search_contained(e,m))}
            r
        }
    }
}

fn nested_bags<'a>(bag : &String, m: &HashMap<String, Vec<(usize,String)>>) -> usize {
    match m.get(bag) {
        None => 0,
        Some(parent) => parent.iter().fold(
            0, |acc, (size, n_bag)| acc + size + size*nested_bags(n_bag, m))
    }
}

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let shiny_gold : String = String::from("shiny gold");
    let mut contains: HashMap<String, Vec<(usize,String)>> = HashMap::new();
    let mut contained_by: HashMap<String, Vec<String>> = HashMap::new();
    for l in r.lines() {
        let mut d = l.split("contain");
        let source : String = d.next().unwrap().split_whitespace().collect::<Vec<&str>>().split_last().unwrap().1.join(" ");
        let contained : Vec<(usize,String)> = d.next().unwrap().split(",").map(|s| {
            let defined = s.trim().split_whitespace().collect::<Vec<&str>>();
            let (n, b) = defined.split_first().unwrap();
            ( match n {
                &"no" => 0,
                _ => n.parse::<usize>().unwrap()
            }, b.to_vec().split_last().unwrap().1.join(" "))
        }).collect();
        for (_x,y) in contained.to_vec() {
            contained_by.entry(y).or_insert(vec![]).push(source.clone())}
        contains.insert(source, contained);
    }
    let mut star1 = search_contained(&shiny_gold, &contained_by);
    star1.sort(); star1.dedup(); // We leave the unique elements
    println!("{:?}", star1.len());
    let star2 = nested_bags(&shiny_gold, &contains);
    println!("{:?}", star2);
}
