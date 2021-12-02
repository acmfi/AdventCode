fn star1(l : &Vec<(i8,i64)>) {
    let (hor,ver):(Vec<(i8,i64)>,Vec<(i8,i64)>)= l.iter().partition(|(n,_f)| *n == 0);
    let hor_adv : i64 = hor.iter().map(|x| x.1).sum();
    let ver_adv : i64 = ver.iter().map(|x| x.1*(x.0 as i64)).sum();
    println!("{}",(hor_adv*ver_adv).abs());
}

fn star2(l : &Vec<(i8,i64)>) {
    let (hor,depth,_aim) : (i64,i64,i64) = l.iter().fold(
        (0,0,0), |(h,d,a),(p,n)| {
            match p {
                -1 => (h,d,a+n),
                1 => (h,d,a-n),
                0 => (h+n,d+a*n,a),
                _ => (h,d,a)
            }           
        }
    );
    println!("{}",(hor*depth).abs());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Vec<(i8,i64)> = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        let n : Vec<&str> = l.split_whitespace().collect();
        let dir : i8 = match n[0] {
            "forward" => 0,
            "up" => 1,
            "down" => -1,
            _ => return None
        };
        Some((dir, n[1].parse().unwrap()))
    }).collect();

    star1(&l);
    star2(&l);
}
