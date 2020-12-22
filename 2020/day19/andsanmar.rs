use std::fs;
use std::collections::HashMap;

#[derive(Debug)]
enum Rule {
    Lit(bool),
    Chain(Vec<Vec<usize>>)
    // Or(Rule, Rule),
    // And(Rule, Rule)
}

fn match_rule(message : &str, rule_ids : &[usize], rules : &HashMap<usize,Rule>) -> bool {
    match rule_ids.split_first() {
        None => message == "",
        Some((first_rule,next_rules)) => match &rules[first_rule] {
            Rule::Lit(c) => (message.chars().nth(0) == Some(if *c {'a'} else {'b'}) && match_rule(&message[1..], next_rules, rules)),
            Rule::Chain(chain) => {
                chain.iter().any(|seq_rules| {
                    let mut r : Vec<usize> = seq_rules.to_vec(); // TODO without replicating vec
                    r.extend(next_rules.iter().map(|x| *x).collect::<Vec<usize>>());
                    match_rule(message, &r.as_slice(), rules)
                })
            }
        }
    }
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let mut it = r.split("\n\n");
    let mut rules : HashMap<usize,Rule> = it.next().unwrap().lines().map(|l| {
        let mut rule = l.split(':');
        let rule_n : usize = rule.next().unwrap().parse().unwrap();
        let rule_def : Rule = match rule.next().unwrap().trim() {
            "\"a\"" => Rule::Lit(true),
            "\"b\"" => Rule::Lit(false),
            chain => Rule::Chain({
                chain.split_whitespace().collect::<Vec<&str>>().split(|n| n == &"|").map(|e| e.iter().map(|n| n.parse::<usize>().unwrap()).collect::<Vec<usize>>()).collect()
            }),
        };
        (rule_n, rule_def)
    }).collect();
    let messages : Vec<&str> = it.next().unwrap().lines().collect();
    {
        let valid_messages = messages.iter().filter(|m| match_rule(m, &[0], &rules));
        println!("{:?}", valid_messages.count());
    }
    {
        rules.insert(8, Rule::Chain(vec![vec![42],vec![42,8]]));
        rules.insert(11, Rule::Chain(vec![vec![42,31],vec![42,11,31]]));
        let valid_messages_2 = messages.iter().filter(|m| match_rule(m, &[0], &rules));
        println!("{:?}", valid_messages_2.count());
    }
}
