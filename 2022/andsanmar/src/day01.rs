fn star1(l : &Vec<Vec<u64>>) {
    println!("{:?}", l.iter().map(|s| s.iter().sum::<u64>()).max());
}

fn star2(l : &Vec<Vec<u64>>) {
    let mut set : Vec<u64> = l.iter().map(|s| s.iter().sum::<u64>()).collect();
    set.sort_by(|a,b| b.cmp(a));
    println!("{:?}", set.iter().take(3).sum::<u64>());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Vec<Vec<u64>> = std::fs::read_to_string(args[1].clone()).unwrap().split("\n\n").map(|s| s.lines().filter_map(|l| match l.parse() {
        Ok(n) => Some(n),
        _ => None
    }).collect()).collect();

    star1(&l);
    star2(&l);
}
