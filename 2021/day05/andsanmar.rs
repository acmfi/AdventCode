type Point = (usize,usize);
type Segment = (Point,Point);
type Grid = Vec<Vec<usize>>;

fn print_grid(grid : &Grid) {
    for l in grid {
        println!("{:?}",l.iter().map(|e| match e {
            0 => ".".to_string(),
            n => (*n).to_string()
        }).collect::<Vec<String>>())
    }
}

fn stars(s:&Vec<Segment>) {
    let (straight, diagonal) : (Vec<&Segment>,Vec<&Segment>) = s.iter().partition(|x| x.0.0 == x.1.0 || x.0.1 == x.1.1 );
    let max_h : usize = s.iter().map(|x| x.0.0.max(x.1.0)).max().unwrap();
    let max_v : usize = s.iter().map(|x| x.0.1.max(x.1.1)).max().unwrap();
    let mut grid : Grid = vec![vec![0;max_v+1];max_h+1];
    for (i,e) in straight {
        if i.0 == e.0 {
            let min = i.1.min(e.1);
            let max = i.1.max(e.1);
            for y in min..=max {
                grid[i.0][y] += 1
            }
        } else if i.1 == e.1 {
            let min = i.0.min(e.0);
            let max = i.0.max(e.0);
            for x in min..=max {
                grid[x][i.1] += 1
            }
        }
    }
    println!("{}", grid.iter().flatten().filter(|x| **x>1).count());
    for (i,e) in diagonal {
        let min_x = i.0.min(e.0);
        let max_x = i.0.max(e.0);
        if i.0 < e.0 {
            if i.1 < e.1 {
                for inc in 0..=(max_x-min_x) {
                    grid[i.0+inc][i.1+inc] += 1
                }
            } else {
                for inc in 0..=(max_x-min_x) {
                    grid[i.0+inc][i.1-inc] += 1
                }
            }
        } else {
            if i.1 < e.1 {         
                for inc in 0..=(max_x-min_x) {
                    grid[i.0-inc][i.1+inc] += 1
                }
            } else {
                for inc in 0..=(max_x-min_x) {
                    grid[i.0-inc][i.1-inc] += 1
                }                
            }
        }
    }
    println!("{}", grid.iter().flatten().filter(|x| **x>1).count());
    //print_grid(&grid);
}

fn parse_input(s : String) -> Vec<Segment> {
    s.lines().filter_map(|l| {
        let els : Vec<Vec<usize>> = l.split("->").map(
            |e| e.split(",").filter_map(|x| match x.trim().parse() {
                Ok(n) => Some(n),
                _ => None
            }).collect()).collect();
        match els.len() {
            2 => match (els[0].len(),els[1].len()) {
                (2,2) => Some(((els[0][0],els[0][1]),(els[1][0],els[1][1]))),
                _ => None
            },
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
    let s: Vec<Segment> = parse_input(std::fs::read_to_string(args[1].clone()).unwrap());
    stars(&s);
}
