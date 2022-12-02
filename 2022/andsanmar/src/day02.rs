#[derive(Debug,PartialEq,Eq,Copy,Clone)]
enum RPS {
    Rock,
    Paper,
    Scissors
}

type Struct = Vec<(RPS,RPS)>;

fn points_figure(fig : RPS) -> u64 {
    match fig {
        RPS::Rock => 1,
        RPS::Paper => 2,
        RPS::Scissors => 3
    }
}

fn winner (inp : RPS) -> RPS {
    match inp {
        RPS::Rock => RPS::Paper,
        RPS::Paper => RPS::Scissors,
        RPS::Scissors => RPS::Rock
    }
}

fn points((p1, p2)  : (RPS, RPS)) -> u64 {
    (if p1 == p2 {
        3
    } else if p2 == winner(p1) {
        6
    } else {
        0
    }) + points_figure(p2)
}

fn points2((p1, p2)  : (RPS, RPS)) -> u64 {
    match p2 {
        RPS::Paper => 3 + points_figure(p1), // Draw
        RPS::Scissors => 6 + points_figure(winner(p1)), // Win
        RPS::Rock => 0 + points_figure(winner(winner(p1))) // Lose
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
