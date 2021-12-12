fn cont_to_end(s: &Vec<(&str,&str)>, current_path : Vec<&str>, current : &str) -> usize {
    let mut paths = 0;
    for (a,b) in s.iter().filter(|(a,b)| *a == current || *b == current) {
        let next = if *a==current {b} else {a};
        if next == &"end" {
            paths += 1;
        } else {
            if !(next.chars().all(|c| c.is_lowercase()) && current_path.contains(next)) {
                let mut new_path = current_path.clone();
                new_path.push(next);
                paths += cont_to_end(s, new_path, next);
            }
        }
    }
    paths
}

fn star1(s: &Vec<(&str,&str)>) {
    println!("{}", cont_to_end(s, vec!["start"], "start"));
}

fn cont_to_end2(s: &Vec<(&str,&str)>, current_path : Vec<&str>, current : &str, chosen_twice : Option<&str>) -> usize {
    let mut paths = 0;
    for (a,b) in s.iter().filter(|(a,b)| *a == current || *b == current) {
        let next = if *a==current {b} else {a};
        if next == &"end" {
            paths += 1;
        } else {
            if !(next.chars().all(|c| c.is_lowercase()) && current_path.contains(next)) {
                let mut new_path = current_path.clone();
                new_path.push(next);
                paths += cont_to_end2(s, new_path, next, chosen_twice);
            } else {
                if chosen_twice == None && next != &"start"  {
                    let mut new_path = current_path.clone();
                    new_path.push(next);
                    paths += cont_to_end2(s, new_path, next, Some(next));
                }
            }
        }
    }
    paths
}

fn star2(s: &Vec<(&str,&str)>) {
    println!("{}", cont_to_end2(s, vec!["start"], "start", None));
}

fn parse_input<'a>(s : &'a String) -> Vec<(&'a str,&'a str)> {
    s.lines().filter_map(|l| {
        let r : Vec<&str> = l.split("-").collect();
        match r.len() {
            0 | 1 => None,
            _ => Some((r[0], r[1]))
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
    let s: Vec<(&str,&str)> = parse_input(&filename);
    star1(&s);
    star2(&s);
}
