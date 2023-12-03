fn star1(l : &Vec<String>) {
    let r : u32 = l.iter().filter_map(|code| {
        let v : Vec<u32> = code.chars().filter_map(|c| c.to_digit(10)).collect();
        Some(v.first()?*10 + v.last()?)
    }).sum();
    println!("{:?}", r);
}

fn subs(s : &str, from : &str, to : &str) -> String {
    str::replace(s, from, to)
}

fn star2(l : &Vec<String>) {
    let new_l : Vec<String> = l.iter()
        .map(|line| subs(line, "one", "one1one"))
        .map(|line| subs(&line, "two", "two2two"))
        .map(|line| subs(&line, "three", "three3three"))
        .map(|line| subs(&line, "four", "four4four"))
        .map(|line| subs(&line, "five", "five5five"))
        .map(|line| subs(&line, "six", "six6six"))
        .map(|line| subs(&line, "seven", "seven7seven"))
        .map(|line| subs(&line, "eight", "eight8eight"))
        .map(|line| subs(&line, "nine", "nine9nine")).collect();
    star1(&new_l)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let binding = std::fs::read_to_string(args[1].clone()).unwrap();
    let l : Vec<String> = binding.lines().map(|s| s.to_string()).collect();

    star1(&l);
    star2(&l);
}
