fn simulate_day(fishs :&mut Vec<usize>) {
    for i in 0..fishs.len() {
        match fishs[i] {
            0 => {
                fishs[i] = 6;
                fishs.push(8);
            },
            n => fishs[i] = n-1
        }   
    }
}
    
fn star1(s:&Vec<usize>) {
    let mut fishs = s.clone();
    for _ in 0..80 {
        simulate_day(&mut fishs);
    }
    println!("{}", &fishs.len());
}

fn simulate_inv_day(inv_fishs : &mut [usize;9]) {
    let new_fishes = inv_fishs[0];
    for x in 1..9 {
        inv_fishs[x-1] = inv_fishs[x];
    }
    inv_fishs[6] += new_fishes;
    inv_fishs[8] = new_fishes;
}

fn star2(s:&Vec<usize>) {
    let mut inv_fishs : [usize;9] = [0;9];
    for x in 0..9 {
        inv_fishs[x] = s.iter().filter(|n| **n == x).count();
    }
    for _ in 0..256 {
        simulate_inv_day(&mut inv_fishs);
    }
    println!("{}", inv_fishs.iter().sum::<usize>());
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
    star1(&s);
    star2(&s);
}
