use std::collections::HashMap;
use std::fs::read_to_string;

fn parse_contain(contents: String) -> Option<Vec<(String, u32)>> {
    if contents.contains("no other bags") {
        return None;
    }
    let mut splitted = contents
        .split(" bags, ")
        .map(|s| s.split(" bag, "))
        .flatten()
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    let new_last: String = splitted
        .pop()
        .unwrap()
        .split(" bags.")
        .map(|s| s.split(" bag."))
        .flatten()
        .collect::<Vec<&str>>()
        .get(0)
        .unwrap()
        .to_string();
    splitted.push(new_last.to_string());
    let with_amounts: Vec<(String, u32)> = splitted
        .iter()
        .map(|s| {
            let num_and_bag: Vec<&str> = s.splitn(2, ' ').collect();
            (
                num_and_bag.get(1).unwrap().to_string(),
                num_and_bag.get(0).unwrap().parse::<u32>().unwrap(),
            )
        })
        .collect();
    return Some(with_amounts);
}

// This function doesn't exactly do what's asked;
// instead of checking if a bag can eventually hold a shiny gold,
// it calculates recursively how many shiny golds a kind of bag can hold.
// The answer to this is can actually be used to solve the 1st star,
// as you can count how many kinds of bags can hold more than 1 shiny gold.
// I did this because I originally misunderstood the problem and later realized
// this was overkill lmao
fn count_shiny_golds(bag: &String, hashmap: &HashMap<String, Option<Vec<(String, u32)>>>) -> u32 {
    match hashmap.get(bag) {
        None => 0,
        Some(value) => match value {
            None => 0,
            Some(vec) => vec
                .iter()
                .map(|(bag_type, how_many)| match &bag_type[..] {
                    "shiny gold" => *how_many,
                    _ => how_many * count_shiny_golds(bag_type, hashmap),
                })
                .sum(),
        },
    }
}

fn required_bags(bag: &String, hashmap: &HashMap<String, Option<Vec<(String, u32)>>>) -> u32 {
    match hashmap.get(bag) {
        None => 0,
        Some(value) => match value {
            None => 0,
            Some(vec) => vec
                .iter()
                .map(|(bag_type, how_many)| how_many + how_many * required_bags(bag_type, hashmap))
                .sum(),
        },
    }
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.lines().collect();
    let parsed: Vec<_> = lines
        .iter()
        .map(|line| {
            let splitted = line.split(" bags contain ").collect::<Vec<_>>();
            (
                splitted.get(0).unwrap().to_string(),
                parse_contain(splitted.get(1).unwrap().to_string()),
            )
        })
        .collect();
    let mut rules = HashMap::new();
    for (k, v) in parsed {
        rules.insert(k, v);
    }

    let number_bags = rules
        .keys()
        .map(|k| count_shiny_golds(k, &rules))
        .filter(|n| *n >= 1)
        .count();
    let number_required_bags = required_bags(&String::from("shiny gold"), &rules);
    println!("*** 1st star ***");
    println!("{}", number_bags);
    println!("*** 2nd star ***");
    println!("{}", number_required_bags);
}
