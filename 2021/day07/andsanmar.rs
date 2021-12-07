fn calculate_fuel_crab(pos: usize, s:&Vec<usize>, f: fn(usize) -> usize) -> usize {
    s.iter().map(|x| {
        let dist = (*x as isize - pos as isize).abs() as usize;
        f(dist)
    }).sum()
}

fn star(s:&Vec<usize>, f: fn(usize) -> usize) {
    let min : usize = *s.iter().min().unwrap();
    let max : usize = *s.iter().max().unwrap();
    println!("{}", (min..max).map(|p| calculate_fuel_crab(p, s, f)).min().unwrap());
}

fn parse_input(s : String) -> Vec<usize> {
    s.split(',').filter_map(|n| {
        match n.trim().parse() {
            Ok(n) => Some(n),
            _ => None
        }
    }).collect()
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let s: Vec<usize> = parse_input(std::fs::read_to_string(args[1].clone()).unwrap());
    star(&s, |x| x);
    // Gauss summation
    star(&s, |x| (x*(x+1))/2);
}
