type Fold = (bool,usize);
type Point = (usize,usize);

fn print_points(points : &Vec<Point>) {
    for y in 0..=*points.iter().map(|(_,b)| b).max().unwrap() {
        for x in 0..=*points.iter().map(|(a,_)| a).max().unwrap() {
            print!("{}", if points.contains(&(x,y)) {"XX"} else {"  "});
        }
        println!();
    }
}

fn stars((mut points,foldings) : (Vec<Point>, Vec<Fold>)) {
    let mut first = true;
    for (fold_x, n) in foldings {
        for (x,y) in points.iter_mut() {
            if fold_x {
                if *x > n {
                    *x = n - (*x - n)
                }
            } else {
                if *y > n {
                    *y = n - (*y - n)
                }                
            }
        }
        if first {
            points.sort();
            points.dedup();
            println!("{:?}", points.len());
            first = false;
        }
    }
    print_points(&points);
}

fn parse_input(s : &String) -> (Vec<Point>, Vec<Fold>) {
    let mut n = s.split("\n\n");
    let x = n.next().unwrap().lines().filter_map(|l| {
        let t : Vec<usize> = l.split(",").filter_map(
            |n| match n.parse() {
                Ok(i) => Some(i),
                _ => None
            }).collect();
        match t.len() {
            2 => Some((t[0],t[1])),
            _ => None
        }
    }).collect();
    let y = n.next().unwrap().lines().filter_map(|l| {
        let t : &str = l.split(" ").nth(2).unwrap();
        let n = t.split("=").nth(1).unwrap().parse().unwrap();
        match t.chars().nth(0) {
            Some('x') => Some((true,n)),
            Some('y') => Some((false,n)),
            _ => None
        }
    }).collect();
    (x,y)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: (Vec<Point>, Vec<Fold>) = parse_input(&filename);
    stars(s);
}
