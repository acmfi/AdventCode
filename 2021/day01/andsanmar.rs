fn star1(l : &Vec<u64>) {
    let matched = l.iter().skip(1).zip(l);
    println!("{}", matched.filter(|(a,b)| a>b).count());
}

fn star2(l : &Vec<u64>) {
    let l1 : Vec<u64> = l.iter().skip(1).zip(l.iter().skip(2)).zip(l).map(|((a,b),c)| a+b+c).collect();
    let matched = l1.iter().skip(1).zip(&l1);
    println!("{}", matched.filter(|(a,b)| a>b).count());
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
