#[derive(Debug,PartialEq,Eq,Copy,Clone)]
enum RPS {
    Rock,
    Paper,
    Scissors
}

impl RPS {
    fn points(&self) -> u64 {
        match self {
            RPS::Rock => 1,
            RPS::Paper => 2,
            RPS::Scissors => 3
        }
    }

    fn won_by(&self)  -> RPS {
        match self {
            RPS::Rock => RPS::Paper,
            RPS::Paper => RPS::Scissors,
            RPS::Scissors => RPS::Rock
        }
    }
}

type Struct = Vec<(RPS,RPS)>;

fn points((p1, p2)  : (RPS, RPS)) -> u64 {
    (if p1 == p2 {
        3
    } else if p2 == p1.won_by() {
        6
    } else {
        0
    }) + p2.points()
}

fn points2((p1, p2)  : (RPS, RPS)) -> u64 {
    match p2 {
        RPS::Paper => 3 + p1.points(), // Draw
        RPS::Scissors => 6 + p1.won_by().points(), // Win
        RPS::Rock => 0 + p1.won_by().won_by().points() // Lose
    }
}

fn star1(l : &Struct) {
    println!("{:?}", l.iter().map(|p| points(*p)).sum::<u64>());
}

fn star2(l : &Struct) {
    println!("{:?}", l.iter().map(|p| points2(*p)).sum::<u64>());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            let els : Vec<char> = l.chars().collect();
            Some((
                match els[0] {
                    'A' => RPS::Rock,
                    'B' => RPS::Paper,
                    _ => RPS::Scissors
                },
                match els[2] {
                    'X' => RPS::Rock,
                    'Y' => RPS::Paper,
                    _ => RPS::Scissors
                }
            ))}
        else { None }
    }).collect();

    star1(&l);
    star2(&l);
}
