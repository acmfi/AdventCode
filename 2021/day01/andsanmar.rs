fn star1(l : &Vec<u64>) {
    println!("{}", l.iter().skip(1).zip(l).filter(|(a,b)| a>b).count());
}

fn star2(l : &Vec<u64>) {
    println!("{}", l.iter().skip(3).zip(l).filter(|(a,b)| a>b).count());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Vec<u64> = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| match l.parse() {
        Ok(n) => Some(n),
        _ => None
    }).collect();

    star1(&l);
    star2(&l);
}
