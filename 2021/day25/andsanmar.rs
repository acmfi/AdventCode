type Cucumbers = Vec<Vec<Option<bool>>>;

fn stars(mut s : Cucumbers) {
    let mut iterations = 0;
    let l_x = s.len();
    let l_y = s[0].len();
    let mut moved : bool = false;
    loop {
        println!("{}", iterations);
        for l in &s {
            println!("{}", l.iter().map(|c| match *c {
                Some(true) => '>',
                Some(false) => 'v',
                None => '.',
            }).collect::<String>());
        }
        let mut n_s : Cucumbers = vec![vec![None;l_y];l_x];
        for x in 0..l_x {
            for y in 0..l_y {
                let n_y = (y+1) % l_y;
                if Some(true) == s[x][y] {
                    if s[x][n_y] == None {
                        n_s[x][n_y] = Some(true);
                        moved = true;
                    } else {
                        n_s[x][y] = Some(true);
                    }
                }
            }
        }
        for x in 0..l_x {
            for y in 0..l_y {
                let n_x = (x+1) % l_x;
                if Some(false) == s[x][y] {
                    if s[n_x][y] != Some(false) && n_s[n_x][y] == None {
                        n_s[n_x][y] = Some(false);
                        moved = true;
                    } else {
                        n_s[x][y] = Some(false);
                    }
                }
            }
        }
        s = n_s;
        iterations += 1;
        if !moved {
            break;
        }
        moved = false;

    }
    println!("{}", iterations);
}

fn parse_input<'a>(s : &'a String) -> Cucumbers {
    s.lines().filter_map(|l| {
        if l.len() == 0 { return None; }
        Some(l.chars().map(|c| { match c {
            '>' => Some(true),
            'v' => Some(false),
            _ => None
        }}).collect())
    }).collect()
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    // println!("{:?}", divisions(&(0,0,0),&(3,3,3),&(3,3,3),&(8,8,8)))
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: Cucumbers = parse_input(&filename);
    stars(s);
}
