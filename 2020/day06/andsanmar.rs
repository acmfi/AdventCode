use std::fs;

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let all_questions = r.split("\n\n").map( |x| {
        let mut q : Vec<char> = x.lines().map(|l| l.chars()).flatten().collect();
        q.sort();
        q.dedup();
        q
    });
    println!("{}", all_questions.map(|x| x.len()).sum::<usize>());
    let questions_common = r.split("\n\n").map( |x| {
        let q : Vec<Vec<char>> = x.lines().map(|x| x.chars().collect()).collect();
        q.iter().fold( q.first().unwrap().to_vec(), |all_answered, lq| {
            let mut intersection = vec![];
            for e in lq {
                if all_answered.contains(&e) {
                    intersection.push(*e);
                }
            }
            intersection
        })
    });
    println!("{}", questions_common.map(|x| x.len()).sum::<usize>());
}
