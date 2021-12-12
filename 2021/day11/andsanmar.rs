fn flash(octopuses : &mut Vec<Vec<u8>>) -> usize {
    let y_dim = octopuses[0].len();
    let x_dim = octopuses.len();
    let mut flashes = 0;
    for x in 0..octopuses.len() {
        for y in 0..octopuses[x].len() {
            if octopuses[x][y] >= 10 {
                flashes += 1;
                for i in 0..=2 {
                    for j in 0..=2 {
                        if (i != 1 || j != 1) && (x+i-1 < x_dim && y+j-1 < y_dim) {
                            octopuses[x+i-1][y+j-1] += 1;
                        }
                    }
                }
                octopuses[x][y] = 0;
                flashes += flash(octopuses);
                octopuses[x][y] = 0;
            }
        }
    }
    flashes
}

fn stars(s : &Vec<Vec<u8>>) {
    let mut octopuses = s.clone();
    let mut flashes = 0;
    for _ in 0..100 {
        for l in octopuses.iter_mut() {
            for c in l.iter_mut() {
                *c += 1;
            }
        }
        flashes += flash(&mut octopuses);
    }
    println!("{}", flashes);
    for x in 0.. {
        for l in octopuses.iter_mut() {
            for c in l.iter_mut() {
                *c += 1;
            }
        }
        flash(&mut octopuses);
        if octopuses.iter().all(|l| l.iter().all(|c| *c == 0)) {
            println!("{}", x+101);
            return;
        }
    }    
}

fn parse_input<'a>(s : &'a String) -> Vec<Vec<u8>> {
    s.lines().filter_map(|l| {
        let r : Vec<u8> = l.chars().filter_map(
            |c| match c.to_digit(10) {
                Some(n) => Some(n as u8),
                _ => None
            }).collect();
        match r.len() {
            0 => None,
            _ => Some(r)
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
    let s: Vec<Vec<u8>> = parse_input(&filename);
    stars(&s);
}
