type Struct = Vec<Vec<u64>>;

fn star1(l : &Struct) {
    let ov = |a,b,c,d| {
        a <= c && b >= d
    };
    println!("{:?}", l.iter().filter(|v| ov(v[0], v[1], v[2], v[3]) || ov(v[2], v[3], v[0], v[1])).count());
}


fn star2(l : &Struct) {
    let ov = |a,b,c,_d| {
        a <= c && b >= c
    };
    println!("{:?}", l.iter().filter(|v| ov(v[0], v[1], v[2], v[3]) || ov(v[2], v[3], v[0], v[1])).count());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            Some(l.split(',').map(|s| s.split('-').map(|e| e.parse().unwrap())).flatten().collect())
        }
        else { None }
    }).collect();

    star1(&l);
    star2(&l);
}
